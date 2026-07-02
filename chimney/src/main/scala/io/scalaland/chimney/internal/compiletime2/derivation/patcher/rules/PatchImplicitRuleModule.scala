package io.scalaland.chimney.internal.compiletime2.derivation.patcher.rules

import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.patcher.Derivation

/** Hearth-based port of `...compiletime.derivation.patcher.rules.PatchImplicitRuleModule` - 1:1 copy. */
private[compiletime2] trait PatchImplicitRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object PatchImplicitRule extends Rule("ImplicitPatcher") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): DerivationResult[Rule.ExpansionResult[A]] =
      ctx match {
        case Patched(obj) =>
          if (ctx.config.areLocalFlagsAndOverridesEmpty) transformWithImplicitIfAvailable[Patch, A](obj)
          else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
        case _ =>
          DerivationResult.attemptNextRuleBecause("Fallback filtered out - Patcher not applicable, only Transformer")
      }

    private def transformWithImplicitIfAvailable[Patch, A](obj: Expr[A])(implicit
        ctx: TransformationContext[Patch, A]
    ): DerivationResult[Rule.ExpansionResult[A]] = summonPatcherSafe[A, Patch] match {
      case Some(patcher) =>
        // We're constructing:
        // '{ ${ patcher }.patch(${ obj }, ${ patch }) } }
        DerivationResult.expandedTotal(patcher.patch(obj, ctx.src))
      case None => DerivationResult.attemptNextRule
    }
  }
}
