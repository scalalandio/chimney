package io.scalaland.chimney.internal.compiletime2.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.transformer.Derivation

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformImplicitPartialFallbackToTotalRuleModule`
  * - 1:1 copy.
  */
private[compiletime2] trait TransformImplicitPartialFallbackToTotalRuleModule {
  this: Derivation & hearth.MacroCommons =>

  protected object TransformImplicitPartialFallbackToTotalRule extends Rule("PartialFallbackToTotal") {
    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if (ctx.config.areLocalFlagsAndOverridesEmpty) transformWithImplicitIfAvailable[From, To]
      else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")

    private def transformWithImplicitIfAvailable[From, To](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] = ctx match {
      case TransformationContext.ForTotal(_)        => DerivationResult.attemptNextRule
      case TransformationContext.ForPartial(src, _) =>
        summonTransformerUnchecked[From, To].fold(DerivationResult.attemptNextRule[To]) { totalTransformer =>
          // We're constructing:
          // '{ ${ totalTransformer }.transform(${ src }) } }
          DerivationResult.expandedTotal(totalTransformer.transform(src))
        }
    }
  }
}
