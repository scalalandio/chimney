package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformationRules { this: Derivation =>

  import ChimneyType.Implicits.*

  abstract protected class Rule(val name: String) {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]]
  }

  protected object Rule {

    sealed trait ExpansionResult[+A]

    object ExpansionResult {
      // successfully expanded TransformationExpr
      case class Expanded[A](transformationExpr: TransformationExpr[A]) extends ExpansionResult[A]

      // continue expansion with another rule on the list
      case object AttemptNextRule extends ExpansionResult[Nothing]
    }

    def expandRules[From, To](
        rules: List[Rule]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[TransformationExpr[To]] = rules match {
      case Nil =>
        DerivationResult.notSupportedTransformerDerivation(ctx)
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
            case ExpansionResult.AttemptNextRule =>
              DerivationResult.log(s"Rule ${rule.name} decided to pass on to the next rule") >>
                expandRules[From, To](nextRules)
          }
    }
  }

  sealed protected trait TransformationExpr[A] extends scala.Product with Serializable {

    import TransformationExpr.{PartialExpr, TotalExpr}

    implicit private lazy val A: Type[A] = this match {
      case TotalExpr(expr) => Expr.typeOf(expr)
      case PartialExpr(expr) =>
        val ChimneyType.PartialResult(a) = Expr.typeOf(expr): @unchecked
        a.Underlying.asInstanceOf[Type[A]]
    }

    final def map[B: Type](f: Expr[A] => Expr[B]): TransformationExpr[B] = this match {
      case TotalExpr(expr)   => TotalExpr(f(expr))
      case PartialExpr(expr) => PartialExpr(ChimneyExpr.PartialResult.map(expr)(Expr.Function1.instance(f)))
    }

    final def flatMap[B: Type](f: Expr[A] => TransformationExpr[B]): TransformationExpr[B] = this match {
      case TotalExpr(expr) => f(expr)
      case PartialExpr(expr) =>
        ExprPromise
          .promise[A](ExprPromise.NameGenerationStrategy.FromType)
          .map(f)
          .foldTransformationExpr { (totalE: ExprPromise[A, Expr[B]]) =>
            // '{ ${ expr }.map { a: $A => ${ b } } }
            PartialExpr(expr.map[B](totalE.fulfilAsLambda))
          } { (partialE: ExprPromise[A, Expr[partial.Result[B]]]) =>
            // '{ ${ expr }.flatMap { a: $A => ${ resultB } } }
            PartialExpr(expr.flatMap[B](partialE.fulfilAsLambda))
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
      assertionFailed(
        s"Derived partial.Result expression where total Transformer expects direct value: ${Expr.prettyPrint(expr)}"
      )
    }
    final def ensurePartial: Expr[partial.Result[A]] = fold { expr =>
      implicit val A: Type[A] = Expr.typeOf(expr)
      ChimneyExpr.PartialResult.Value(expr).upcastExpr[partial.Result[A]]
    }(identity)

    def prettyPrint: String = fold(Expr.prettyPrint)(Expr.prettyPrint)
  }
  protected object TransformationExpr {
    def fromTotal[A](expr: Expr[A]): TransformationExpr[A] = TotalExpr(expr)
    def fromPartial[A](expr: Expr[partial.Result[A]]): TransformationExpr[A] = PartialExpr(expr)

    final case class TotalExpr[A](expr: Expr[A]) extends TransformationExpr[A]
    final case class PartialExpr[A](expr: Expr[partial.Result[A]]) extends TransformationExpr[A]
  }

  implicit final class TransformationExprPromiseOps[From, To](promise: ExprPromise[From, TransformationExpr[To]]) {

    def foldTransformationExpr[B](onTotal: ExprPromise[From, Expr[To]] => B)(
        onPartial: ExprPromise[From, Expr[partial.Result[To]]] => B
    ): B = promise.map(_.toEither).foldEither(onTotal)(onPartial)

    def exprPartition: Either[ExprPromise[From, Expr[To]], ExprPromise[From, Expr[partial.Result[To]]]] =
      promise.map(_.toEither).partition

    final def isTotal: Boolean = foldTransformationExpr(_ => true)(_ => false)
    final def isPartial: Boolean = foldTransformationExpr(_ => false)(_ => true)

    def ensureTotal: ExprPromise[From, Expr[To]] = promise.map(_.ensureTotal)
    def ensurePartial: ExprPromise[From, Expr[partial.Result[To]]] = promise.map(_.ensurePartial)
  }

  protected val rulesAvailableForPlatform: List[Rule]
}
