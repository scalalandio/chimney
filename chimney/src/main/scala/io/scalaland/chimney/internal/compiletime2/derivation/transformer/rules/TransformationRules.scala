package io.scalaland.chimney.internal.compiletime2.derivation.transformer.rules

import hearth.fp.syntax.*
import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.transformer.Derivation
import io.scalaland.chimney.partial

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformationRules`.
  *
  * Chimney's OWN rule engine is preserved (NOT replaced by Hearth's `Rules`/`Rule.Applicability` combinator): the
  * `ExpansionResult.AttemptNextRule(reason)` accumulation, logging and error semantics are kept exactly as before, just
  * running on MIO. (Hearth's `Rules` is close - Matched/Yielded mirror Expanded/AttemptNextRule - but it returns the
  * accumulated yield-reasons instead of logging them one-by-one and has no `TransformationExpr` awareness, so
  * delegating would change the logs that tests assert on.)
  *
  * Differences vs the old version:
  *   - `TransformationExpr#map`/`#flatMap` build lambdas with Hearth's `LambdaBuilder` instead of `ExprPromise`
  *     (`Expr.Function1.instance`/`fulfilAsLambda` counterparts),
  *   - the old `TransformationExprPromiseOps` (ops over `ExprPromise[From, TransformationExpr[To]]`) becomes
  *     [[TransformationExprBuilderOps]] (ops over `LambdaBuilder[From, TransformationExpr[To]]`, where `From` is now
  *     the lambda shape, e.g. `A => *`) - method names preserved (`foldTransformationExpr`, `exprPartition`, `isTotal`,
  *     `isPartial`, `ensureTotal`, `ensurePartial`),
  *   - `.log(msg)` becomes `.logInfo(msg)` (see the package object).
  */
private[compiletime2] trait TransformationRules { this: Derivation & hearth.MacroCommons =>

  import ChimneyType.Implicits.*

  /** Defines "derivation rule" ("if condition is met then derive").
    *
    * Since we cannot restrict how condition is checked (running some predicate or using PartialFunction is too
    * restrictive), we have to express matching or not with the result:
    *   - `Expanded` means that rule applied and created `Expr` value
    *   - `AttemptNextRule` means that rule decided that conditions aren't met The `DerivationResult` as a whole might
    *     also fail, which means that rule did apply but couldn't derive expression.
    */
  abstract protected class Rule(val name: String) {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]]
  }
  protected object Rule {

    sealed trait ExpansionResult[+A]
    object ExpansionResult {
      // successfully expanded TransformationExpr
      final case class Expanded[A](transformationExpr: TransformationExpr[A]) extends ExpansionResult[A]

      // continue expansion with another rule on the list
      final case class AttemptNextRule(reason: Option[String]) extends ExpansionResult[Nothing]
    }

    /** Attempt to apply rules in order in which they are on list. The first match wins. */
    def expandRules[From, To](
        rules: List[Rule]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[TransformationExpr[To]] = rules match {
      case Nil =>
        DerivationResult.notSupportedTransformerDerivation(ctx).logInfo("Tested all derivation rules, none matched")
      case rule :: nextRules =>
        DerivationResult
          .namedScope(s"Attempting expansion of rule ${rule.name}")(
            rule.expand[From, To].logFailure(errors => errors.prettyPrint)
          )
          .flatMap {
            case ExpansionResult.Expanded(transformationExpr) =>
              DerivationResult
                .log(s"Rule ${rule.name} expanded successfully: ${transformationExpr.prettyPrint}")
                .as(transformationExpr.asInstanceOf[TransformationExpr[To]])
            case ExpansionResult.AttemptNextRule(Some(reason)) =>
              DerivationResult.log(
                s"Rule ${rule.name} decided to pass on to the next rule - some conditions were fulfilled but at least one failed: $reason"
              ) >>
                expandRules[From, To](nextRules)
            case ExpansionResult.AttemptNextRule(None) =>
              DerivationResult.log(s"Rule ${rule.name} decided to pass on to the next rule") >>
                expandRules[From, To](nextRules)
          }
    }
  }

  /** Let us store both `Expr[A]` and `Expr[partial.Result[A]]` as one type for convenience. */
  sealed protected trait TransformationExpr[A] extends scala.Product with Serializable {

    import TransformationExpr.{PartialExpr, TotalExpr}

    implicit private lazy val A: Type[A] = this match {
      case TotalExpr(expr)   => Expr.typeOf(expr)
      case PartialExpr(expr) =>
        val ChimneyType.PartialResult(a) = Expr.typeOf(expr): @unchecked
        a.Underlying.asInstanceOf[Type[A]]
    }

    final def map[B: Type](f: Expr[A] => Expr[B]): TransformationExpr[B] = this match {
      case TotalExpr(expr)   => TotalExpr(f(expr))
      case PartialExpr(expr) =>
        // '{ ${ expr }.map { a: $A => ${ b } } }
        PartialExpr(ChimneyExpr.PartialResult.map(expr)(LambdaBuilder.of1[A]().map(f).build[B]))
    }

    final def flatMap[B: Type](f: Expr[A] => TransformationExpr[B]): TransformationExpr[B] = this match {
      case TotalExpr(expr)   => f(expr)
      case PartialExpr(expr) =>
        LambdaBuilder
          .of1[A]()
          .map(f)
          .foldTransformationExpr { (totalE: LambdaBuilder[A => *, Expr[B]]) =>
            // '{ ${ expr }.map { a: $A => ${ b } } }
            PartialExpr(expr.map[B](totalE.build[B]))
          } { (partialE: LambdaBuilder[A => *, Expr[partial.Result[B]]]) =>
            // '{ ${ expr }.flatMap { a: $A => ${ resultB } } }
            PartialExpr(expr.flatMap[B](partialE.build[partial.Result[B]]))
          }
    }

    final def fold[B](onTotal: Expr[A] => B)(onPartial: Expr[partial.Result[A]] => B): B = this match {
      case TotalExpr(expr)   => onTotal(expr)
      case PartialExpr(expr) => onPartial(expr)
    }

    final def toEither: Either[Expr[A], Expr[partial.Result[A]]] =
      fold[Either[Expr[A], Expr[partial.Result[A]]]](e => Left(e))(e => Right(e))

    final def isTotal: Boolean = fold(_ => true)(_ => false)
    final def isPartial: Boolean = fold(_ => false)(_ => true)

    final def ensureTotal: Expr[A] = fold(identity) { expr =>
      // $COVERAGE-OFF$should never happen unless we messed up
      assertionFailed(
        s"Derived partial.Result expression where total Transformer expects direct value: ${Expr.prettyPrint(expr)}"
      )
      // $COVERAGE-ON$
    }
    final def ensurePartial: Expr[partial.Result[A]] = fold { expr =>
      implicit val A: Type[A] = Expr.typeOf(expr)
      ChimneyExpr.PartialResult.Value(expr).upcast[partial.Result[A]]
    }(identity)

    def prettyPrint: String = fold(Expr.prettyPrint)(Expr.prettyPrint)
  }
  protected object TransformationExpr {
    def fromTotal[A](expr: Expr[A]): TransformationExpr[A] = TotalExpr(expr)
    def fromPartial[A](expr: Expr[partial.Result[A]]): TransformationExpr[A] = PartialExpr(expr)

    final case class TotalExpr[A](expr: Expr[A]) extends TransformationExpr[A]
    final case class PartialExpr[A](expr: Expr[partial.Result[A]]) extends TransformationExpr[A]
  }

  /** Old `TransformationExprPromiseOps`, on `LambdaBuilder` instead of `ExprPromise` (see the trait's ScalaDoc). */
  implicit final class TransformationExprBuilderOps[From[_], To](
      builder: LambdaBuilder[From, TransformationExpr[To]]
  ) {

    def foldTransformationExpr[B](onTotal: LambdaBuilder[From, Expr[To]] => B)(
        onPartial: LambdaBuilder[From, Expr[partial.Result[To]]] => B
    ): B = exprPartition.fold(onTotal, onPartial)

    def exprPartition: Either[LambdaBuilder[From, Expr[To]], LambdaBuilder[From, Expr[partial.Result[To]]]] =
      builder.partition(_.toEither)

    def isTotal: Boolean = foldTransformationExpr(_ => true)(_ => false)
    def isPartial: Boolean = foldTransformationExpr(_ => false)(_ => true)

    def ensureTotal: LambdaBuilder[From, Expr[To]] = builder.map(_.ensureTotal)
    def ensurePartial: LambdaBuilder[From, Expr[partial.Result[To]]] = builder.map(_.ensurePartial)
  }

  protected val rulesAvailableForPlatform: List[Rule]
}
