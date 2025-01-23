package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformImplicitConversionRuleModule { this: Derivation =>

  import Type.Implicits.*

  protected object TransformImplicitConversionRule extends Rule("ImplicitConversion") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if (ctx.config.areLocalFlagsAndOverridesEmpty) {
        if (ctx.config.flags.implicitConversions) {
          Expr.summonImplicit[From => To] match {
            case Some(ev) => transformWithConversion[From, To](ev)
            case None     => DerivationResult.attemptNextRule
          }
        } else DerivationResult.attemptNextRuleBecause("Implicit conversions are disabled")
      } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")

    private def transformWithConversion[From, To](ev: Expr[From => To])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ ev }.apply(${ src }) }
      DerivationResult.expandedTotal(ev.apply(ctx.src))
  }
}
