package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformTypeConstraintRuleModule`.
  *
  * Differences vs the old version:
  *   - `Type[From <:< To]`/`Type[From => To]` instances are created locally with cross-quotes `Type.of` in helpers with
  *     their own `[From: Type, To: Type]` parameters (old code got them ambiently from `Type.Implicits`),
  *   - `ev.upcastToExprOf[From => To].apply(src)` becomes `ev.upcast[From => To].apply(src)` (Hearth upcast + the
  *     `ScalaFunction1ExprOps` compat ops).
  */
private[compiletime] trait TransformTypeConstraintRuleModule { this: Derivation & hearth.MacroCommons =>

  import ScalaType.Implicits.*

  protected object TransformTypeConstraintRule extends Rule("TypeConstraint") {

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
      // We're constructing:
      // '{ ${ ev }.apply(${ src }) }
      DerivationResult.expandedTotal(ev.upcast[From => To].apply(ctx.src))
    }
  }
}
