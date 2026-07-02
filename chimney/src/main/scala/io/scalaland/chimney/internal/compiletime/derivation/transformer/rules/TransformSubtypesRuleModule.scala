package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformSubtypesRuleModule` - 1:1 copy
  * (`upcastToExprOf[To]` becomes Hearth's `upcast[To]`, same compile-time-checked semantics).
  */
private[compiletime] trait TransformSubtypesRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object TransformSubtypesRule extends Rule("Subtypes") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if (Type[From] <:< Type[To]) {
        if (ctx.config.areLocalFlagsAndOverridesEmpty) transformByUpcasting[From, To]
        else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
      } else DerivationResult.attemptNextRule

    private def transformByUpcasting[From, To](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ src } : $To } }
      DerivationResult.expandedTotal(ctx.src.upcast[To])
  }
}
