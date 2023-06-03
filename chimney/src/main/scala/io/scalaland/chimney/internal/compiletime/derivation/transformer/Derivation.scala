package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}
import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Derivation extends Definitions with ResultOps with ImplicitSummoning {

  import ChimneyTypeImplicits.*

  /** Intended use case: recursive derivation */
  final protected def deriveTransformationResultExpr[From, To](implicit
      ctx: TransformerContext[From, To]
  ): DerivationResult[DerivedExpr[To]] =
    DerivationResult.namedScope(
      ctx match {
        case _: TransformerContext.ForTotal[?, ?] =>
          s"Deriving Total Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}"
        case _: TransformerContext.ForPartial[?, ?] =>
          s"Deriving Partial Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}"
      }
    ) {
      Rule.expandRules[From, To](rulesAvailableForPlatform)
    }

  final protected def deriveRecursiveTransformationExpr[NewFrom: Type, NewTo: Type](
      newSrc: Expr[NewFrom]
  )(implicit ctx: TransformerContext[?, ?]): DerivationResult[DerivedExpr[NewTo]] = {
    val newCtx: TransformerContext[NewFrom, NewTo] = ctx.updateFromTo[NewFrom, NewTo](newSrc).updateConfig {
      _.prepareForRecursiveCall
    }
    deriveTransformationResultExpr(newCtx)
      .logSuccess {
        case DerivedExpr.TotalExpr(expr)   => s"Derived recursively total expression ${Expr.prettyPrint(expr)}"
        case DerivedExpr.PartialExpr(expr) => s"Derived recursively partial expression ${Expr.prettyPrint(expr)}"
      }
  }

  abstract protected class Rule(val name: String) {

    def expand[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[Rule.ExpansionResult[To]]
  }

  protected object Rule {

    sealed trait ExpansionResult[+A]

    object ExpansionResult {
      // successfully expanded transformation expr
      case class Expanded[A](transformationExpr: DerivedExpr[A]) extends ExpansionResult[A]
      // continue expansion with another rule on the list
      case object Continue extends ExpansionResult[Nothing]
    }

    def expandRules[From, To](
        rules: List[Rule]
    )(implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] = {
      rules match {
        case Nil =>
          DerivationResult.notSupportedTransformerDerivation
        case rule :: nextRules =>
          DerivationResult
            .namedScope(s"Attempting expansion of rule ${rule.name}")(
              rule.expand[From, To].logFailure { errors => errors.prettyPrint }
            )
            .flatMap {
              case ExpansionResult.Expanded(transformationExpr) =>
                DerivationResult
                  .log(s"Rule ${rule.name} expanded successfully")
                  .as(transformationExpr.asInstanceOf[DerivedExpr[To]])
              case ExpansionResult.Continue =>
                DerivationResult.log(s"Rule ${rule.name} decided to continue expansion") >>
                  expandRules[From, To](nextRules)
            }
      }
    }
  }

  // TODO: rename to TransformationExpr
  sealed protected trait DerivedExpr[A] extends Product with Serializable {

    def toEither: Either[Expr[A], Expr[partial.Result[A]]] = this match {
      case DerivedExpr.TotalExpr(expr)   => Left(expr)
      case DerivedExpr.PartialExpr(expr) => Right(expr)
    }

    def ensurePartial: Expr[partial.Result[A]] = this match {
      case DerivedExpr.TotalExpr(expr) =>
        implicit val A: Type[A] = Expr.typeOf(expr)
        ChimneyExpr.PartialResult.Value(expr).upcastExpr[partial.Result[A]]
      case DerivedExpr.PartialExpr(expr) => expr
    }
  }

  protected object DerivedExpr {
    def total[A](expr: Expr[A]): DerivedExpr[A] = TotalExpr(expr)
    def partial[A](expr: Expr[io.scalaland.chimney.partial.Result[A]]): DerivedExpr[A] = PartialExpr(expr)

    final case class TotalExpr[A](expr: Expr[A]) extends DerivedExpr[A]
    final case class PartialExpr[A](expr: Expr[io.scalaland.chimney.partial.Result[A]]) extends DerivedExpr[A]
  }

  protected val rulesAvailableForPlatform: List[Rule]
}
