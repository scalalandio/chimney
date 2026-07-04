package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation

private[compiletime] trait PatchImplicitRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object PatchImplicitRule extends Rule("ImplicitPatcher") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): MIO[Rule.ExpansionResult[A]] =
      ctx match {
        case Patched(obj) =>
          if (ctx.config.areLocalFlagsAndOverridesEmpty) transformWithImplicitIfAvailable[Patch, A](obj)
          else attemptNextRuleBecause("Configuration has defined overrides")
        case _ =>
          attemptNextRuleBecause("Fallback filtered out - Patcher not applicable, only Transformer")
      }

    private def transformWithImplicitIfAvailable[Patch, A](obj: Expr[A])(implicit
        ctx: TransformationContext[Patch, A]
    ): MIO[Rule.ExpansionResult[A]] = summonPatcherSafe[A, Patch] match {
      case Some(patcher) =>
        // We're constructing:
        // '{ ${ patcher }.patch(${ obj }, ${ patch }) } }
        expandedTotal(patcher.patch(obj, ctx.src))
      case None => attemptNextRule
    }
  }
}
