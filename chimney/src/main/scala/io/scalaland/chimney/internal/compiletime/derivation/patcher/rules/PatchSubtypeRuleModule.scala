package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation

private[compiletime] trait PatchSubtypeRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object PatchSubtypeRuleModule extends Rule("SubtypesPatch") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      if (Type[From] <:< Type[To]) {
        if (ctx.config.areLocalFlagsAndOverridesEmpty && ctx.config.filterCurrentOverridesForFallbacks.isEmpty)
          transformByUpcasting[From, To]
        else attemptNextRuleBecause("Configuration has defined overrides")
      } else attemptNextRule

    private def transformByUpcasting[From, To](implicit
        ctx: TransformationContext[From, To]
    ): MIO[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ src } : $To } }
      expandedTotal(ctx.src.upcast[To])
  }
}
