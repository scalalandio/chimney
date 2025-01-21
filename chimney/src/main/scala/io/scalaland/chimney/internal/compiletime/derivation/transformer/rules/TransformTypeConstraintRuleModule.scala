package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformTypeConstraintRuleModule { this: Derivation =>

  import Type.Implicits.*

  protected object TransformTypeConstraintRule extends Rule("TypeConstraint") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if (ctx.config.areOverridesEmpty) {
        if (ctx.config.flags.typeConstraintEvidence) {
          Expr.summonImplicit[From <:< To] match {
            case Some(ev) => transformWithEvidence[From, To](ev)
            case None     => DerivationResult.attemptNextRule
          }
        } else DerivationResult.attemptNextRuleBecause("<:< evidence is disabled")
      } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")

    private def transformWithEvidence[From, To](ev: Expr[From <:< To])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ ev }.apply(${ src }) }
      DerivationResult.expandedTotal(ev.upcastToExprOf[From => To].apply(ctx.src))
  }
}
