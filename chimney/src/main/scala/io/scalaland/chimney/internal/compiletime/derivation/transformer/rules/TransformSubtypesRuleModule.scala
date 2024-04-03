package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformSubtypesRuleModule { this: Derivation =>

  protected object TransformSubtypesRule extends Rule("Subtypes") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if (Type[From] <:< Type[To]) {
        if (ctx.config.areOverridesEmptyForCurrent) transformByUpcasting[From, To]
        else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
      } else DerivationResult.attemptNextRule

    private def transformByUpcasting[From, To](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ src } : $To } }
      DerivationResult.expandedTotal(ctx.src.upcastExpr[To])
  }
}
