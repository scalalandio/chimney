package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformTypeConstraintRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object TransformTypeConstraintRule extends Rule("TypeConstraint") {

    // Cross-quotes helper in a method with regular type parameters (the cross-quotes helper-def pattern).
    private def applyFnCompat[A: Type, B: Type](fn: Expr[A => B], a: Expr[A]): Expr[B] = Expr.quote {
      Expr.splice(fn).apply(Expr.splice(a))
    }

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if (ctx.config.areLocalFlagsAndOverridesEmpty) {
        if (ctx.config.flags.typeConstraintEvidence && !(Type[From] <:< Type[To])) {
          summonEvidence[From, To] match {
            case Some(ev) => transformWithEvidence[From, To](ev)
            case None     => DerivationResult.attemptNextRule
          }
        } else DerivationResult.attemptNextRuleBecause("<:< evidence is disabled")
      } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")

    private def summonEvidence[From: Type, To: Type]: Option[Expr[From <:< To]] = {
      implicit val EvidenceType: Type[From <:< To] = Type.of[From <:< To]
      summonImplicitOptionOf[From <:< To]
    }

    private def transformWithEvidence[From: Type, To: Type](ev: Expr[From <:< To])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] = {
      implicit val EvidenceType: Type[From <:< To] = Type.of[From <:< To]
      implicit val FnFromToType: Type[From => To] = Type.of[From => To]
      // We're constructing:
      // '{ ${ ev }.apply(${ src }) }
      DerivationResult.expandedTotal(applyFnCompat(ev.upcast[From => To], ctx.src))
    }
  }
}
