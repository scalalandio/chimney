package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation

private[compiletime] trait PatchSubtypeRuleModule { this: Derivation =>

  protected object PatchSubtypeRuleModule extends Rule("SubtypesPatch") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if (Type[From] <:< Type[To]) {
        if (ctx.config.areLocalFlagsAndOverridesEmpty && ctx.config.filterCurrentOverridesForFallbacks.isEmpty)
          transformByUpcasting[From, To]
        else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
      } else DerivationResult.attemptNextRule

    private def transformByUpcasting[From, To](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ src } : $To } }
      DerivationResult.expandedTotal(ctx.src.upcastToExprOf[To])
  }
}
