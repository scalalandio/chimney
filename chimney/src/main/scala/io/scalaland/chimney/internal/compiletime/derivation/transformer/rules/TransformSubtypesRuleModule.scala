package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformSubtypesRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object TransformSubtypesRule extends Rule("Subtypes") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      if (Type[From] <:< Type[To]) {
        if (ctx.config.areLocalFlagsAndOverridesEmpty) transformByUpcasting[From, To]
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
