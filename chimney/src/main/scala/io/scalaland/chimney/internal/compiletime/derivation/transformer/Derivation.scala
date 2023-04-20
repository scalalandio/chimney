package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[derivation] trait Derivation { this: Definitions =>

  /** Intended use case: recursive derivation */
  final protected def deriveTransformationResultExpr[From, To](implicit
      ctx: TransformerContext[From, To]
  ): DerivationResult[DerivedExpr[To]] = Rule[From, To](rulesAvailableForPlatform)

  abstract protected class Rule {

    def isApplicableTo[From, To](ctx: TransformerContext[From, To]): Boolean

    def apply[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]]
  }
  protected object Rule {

    def apply[From, To](
        rules: Seq[Rule]
    )(implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] =
      rules.find(_.isApplicableTo(ctx)) match {
        case Some(rule) => rule[From, To]
        case None       => DerivationResult.notSupportedTransformerDerivation
      }
  }

  protected val rulesAvailableForPlatform: Seq[Rule]
}
