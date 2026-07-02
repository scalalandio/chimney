package io.scalaland.chimney.internal.compiletime2.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.transformer.Derivation

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformImplicitConversionRuleModule`.
  *
  * Differences vs the old version:
  *   - `Expr.summonImplicit[From => To]` (macro-commons `Option`-returning) goes through [[summonImplicitConversion]],
  *     a helper with its own `[From: Type, To: Type]` parameters that also provides the `Type[From => To]` instance
  *     (old code got it ambiently from `Type.Implicits`),
  *   - `ev.apply(src)` uses the `ScalaFunction1ExprOps` compat ops (macro-commons `Function1Ops`).
  */
private[compiletime2] trait TransformImplicitConversionRuleModule { this: Derivation & hearth.MacroCommons =>

  import ScalaType.Implicits.*

  protected object TransformImplicitConversionRule extends Rule("ImplicitConversion") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if (ctx.config.areLocalFlagsAndOverridesEmpty) {
        if (ctx.config.flags.implicitConversions) {
          summonImplicitConversion[From, To] match {
            case Some(ev) => transformWithConversion[From, To](ev)
            case None     => DerivationResult.attemptNextRule
          }
        } else DerivationResult.attemptNextRuleBecause("Implicit conversions are disabled")
      } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")

    private def summonImplicitConversion[From: Type, To: Type]: Option[Expr[From => To]] =
      summonImplicitOptionOf[From => To]

    private def transformWithConversion[From, To](ev: Expr[From => To])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ ev }.apply(${ src }) }
      DerivationResult.expandedTotal(ev.apply(ctx.src))
  }
}
