package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation
import io.scalaland.chimney.internal.compiletime.NotSupportedPatcherDerivation

private[compiletime] trait PatchNotMatchedRuleModule { this: Derivation =>

  protected object PatchNotMatchedRule extends Rule("PatchNotMatched") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): DerivationResult[Rule.ExpansionResult[A]] =
      ctx match {
        case Patched(_) =>
          DerivationResult.patcherError(
            NotSupportedPatcherDerivation(Type.prettyPrint[A], Type.prettyPrint[Patch])
          )
        case _ => DerivationResult.attemptNextRule
      }
  }
}
