package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformImplicitConversionRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object TransformImplicitConversionRule extends Rule("ImplicitConversion") {

    // Cross-quotes helper in a method with regular type parameters (the cross-quotes helper-def pattern).
    private def applyFnCompat[A: Type, B: Type](fn: Expr[A => B], a: Expr[A]): Expr[B] = Expr.quote {
      Expr.splice(fn).apply(Expr.splice(a))
    }

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if (ctx.config.areLocalFlagsAndOverridesEmpty) {
        if (ctx.config.flags.implicitConversions) {
          summonImplicitConversion[From, To] match {
            case Some(ev) => transformWithConversion[From, To](ev)
            case None     => DerivationResult.attemptNextRule
          }
        } else DerivationResult.attemptNextRuleBecause("Implicit conversions are disabled")
      } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")

    private def summonImplicitConversion[From: Type, To: Type]: Option[Expr[From => To]] = {
      implicit val FnFromToType: Type[From => To] = Type.of[From => To]
      summonImplicitOptionOf[From => To]
    }

    private def transformWithConversion[From, To](ev: Expr[From => To])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ ev }.apply(${ src }) }
      DerivationResult.expandedTotal(applyFnCompat(ev, ctx.src))
  }
}
