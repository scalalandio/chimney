package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.dsl.{PreferPartialTransformer, PreferTotalTransformer}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformImplicitRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object TransformImplicitRule extends Rule("Implicit") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      if (ctx.config.areLocalFlagsAndOverridesEmpty) transformWithImplicitIfAvailable[From, To]
      else attemptNextRuleBecause("Configuration has defined overrides")

    private def transformWithImplicitIfAvailable[From, To](implicit
        ctx: TransformationContext[From, To]
    ): MIO[Rule.ExpansionResult[To]] = ctx match {
      case TransformationContext.ForTotal(src) =>
        summonTransformerSafe[From, To].fold(attemptNextRule[To]) { totalTransformer =>
          // We're constructing:
          // '{ ${ totalTransformer }.transform(${ src }) } }
          expandedTotal(totalTransformer.transform(src))
        }
      case TransformationContext.ForPartial(src, failFast) =>
        import ctx.config.flags.implicitConflictResolution
        (summonTransformerSafe[From, To], summonPartialTransformerSafe[From, To]) match {
          case (Some(total), Some(partial)) if implicitConflictResolution.isEmpty =>
            ambiguousImplicitPriority(total, partial)
          case (Some(totalTransformer), partialTransformerOpt)
              if partialTransformerOpt.isEmpty || implicitConflictResolution.contains(PreferTotalTransformer) =>
            // We're constructing:
            // '{ ${ totalTransformer }.transform(${ src }) } }
            expandedTotal(totalTransformer.transform(src))
          case (totalTransformerOpt, Some(partialTransformer))
              if totalTransformerOpt.isEmpty || implicitConflictResolution.contains(PreferPartialTransformer) =>
            // We're constructing:
            // '{ ${ partialTransformer }.transform(${ src }, ${ failFast }) } }
            expandedPartial(partialTransformer.transform(src, failFast))
          case _ => attemptNextRule
        }
    }
  }
}
