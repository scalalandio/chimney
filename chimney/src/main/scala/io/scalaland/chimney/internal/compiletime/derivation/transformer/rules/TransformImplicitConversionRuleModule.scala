package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformImplicitConversionRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object TransformImplicitConversionRule extends Rule("ImplicitConversion") {

    // Cross-quotes helper in a method with regular type parameters (the cross-quotes helper-def pattern).
    private def applyFnCompat[A: Type, B: Type](fn: Expr[A => B], a: Expr[A]): Expr[B] = Expr.quote {
      Expr.splice(fn).apply(Expr.splice(a))
    }

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      if (ctx.config.areLocalFlagsAndOverridesEmpty) {
        if (ctx.config.flags.implicitConversions) {
          summonImplicitConversion[From, To] match {
            case Some(ev) => transformWithConversion[From, To](ev)
            case None     => attemptNextRule
          }
        } else attemptNextRuleBecause("Implicit conversions are disabled")
      } else attemptNextRuleBecause("Configuration has defined overrides")

    private def summonImplicitConversion[From: Type, To: Type]: Option[Expr[From => To]] = {
      implicit val FnFromToType: Type[From => To] = Type.of[From => To]
      summonImplicitOptionOf[From => To]
    }

    private def transformWithConversion[From, To](ev: Expr[From => To])(implicit
        ctx: TransformationContext[From, To]
    ): MIO[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ ev }.apply(${ src }) }
      expandedTotal(applyFnCompat(ev, ctx.src))
  }
}
