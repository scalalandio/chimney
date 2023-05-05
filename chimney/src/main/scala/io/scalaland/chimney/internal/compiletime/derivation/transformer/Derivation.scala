package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[derivation] trait Derivation { this: Definitions =>

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
      Rule[From, To](rulesAvailableForPlatform*).logSuccess {
        case DerivedExpr.TotalExpr(expr)   => s"Derived total expression ${Expr.prettyPrint(expr)}"
        case DerivedExpr.PartialExpr(expr) => s"Derived partial expression ${Expr.prettyPrint(expr)}"
      }
    }

  abstract protected class Rule {

    def isApplicableTo[From, To](implicit ctx: TransformerContext[From, To]): Boolean

    def apply[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]]
  }
  protected object Rule {

    def apply[From, To](
        rules: Rule*
    )(implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] =
      rules.find(_.isApplicableTo(ctx)) match {
        case Some(rule) => rule[From, To]
        case None       => DerivationResult.notSupportedTransformerDerivation
      }
  }

  protected val rulesAvailableForPlatform: Seq[Rule]
}
