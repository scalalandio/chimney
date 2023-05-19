package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}
import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Derivation extends Definitions with ResultOps with ImplicitSummoning {

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

  // proposed initial version of the API, we might make it more generic later on
  // idea:
  //   1. we derive the Expr[To] OR Expr[partial.Result[To]] because we don't know what we'll get until we try
  //   2. THEN we create the code which will initialize Expr[From] because that might depend on the result of derivation
  //   3. we also propagate the errors and Rule.ExpansionResult
  protected def deriveFold[NewFrom: Type, NewTo: Type, To: Type](
      deriveFromSrc: Expr[NewFrom] => DerivationResult[Rule.ExpansionResult[NewTo]]
  )(
      provideSrcForTotal: (Expr[NewFrom] => Expr[NewTo]) => DerivedExpr[To]
  )(
      provideSrcForPartial: (Expr[NewFrom] => Expr[partial.Result[NewTo]]) => DerivedExpr[To]
  ): DerivationResult[Rule.ExpansionResult[To]]

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

  sealed protected trait DerivedExpr[A] // TODO: rename to TransformationExpr

  protected object DerivedExpr {
    final case class TotalExpr[A](expr: Expr[A]) extends DerivedExpr[A]
    final case class PartialExpr[A](expr: Expr[partial.Result[A]]) extends DerivedExpr[A]
  }

  protected val rulesAvailableForPlatform: List[Rule]
}
