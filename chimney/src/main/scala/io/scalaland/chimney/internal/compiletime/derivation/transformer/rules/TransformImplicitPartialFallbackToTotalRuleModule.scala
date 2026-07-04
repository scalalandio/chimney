package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformImplicitPartialFallbackToTotalRuleModule {
  this: Derivation & hearth.MacroCommons =>

  protected object TransformImplicitPartialFallbackToTotalRule extends Rule("PartialFallbackToTotal") {
    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      if (ctx.config.areLocalFlagsAndOverridesEmpty) transformWithImplicitIfAvailable[From, To]
      else attemptNextRuleBecause("Configuration has defined overrides")

    private def transformWithImplicitIfAvailable[From, To](implicit
        ctx: TransformationContext[From, To]
    ): MIO[Rule.ExpansionResult[To]] = ctx match {
      case TransformationContext.ForTotal(_)        => attemptNextRule
      case TransformationContext.ForPartial(src, _) =>
        summonTransformerUnchecked[From, To].fold(attemptNextRule[To]) { totalTransformer =>
          // We're constructing:
          // '{ ${ totalTransformer }.transform(${ src }) } }
          expandedTotal(totalTransformer.transform(src))
        }
    }
  }
}
