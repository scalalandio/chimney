package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation
import io.scalaland.chimney.internal.compiletime.{DerivationError, NotSupportedPatcherDerivation}

private[compiletime] trait PatchNotMatchedRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object PatchNotMatchedRule extends Rule("PatchNotMatched") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): MIO[Rule.ExpansionResult[A]] =
      ctx match {
        case Patched(_) =>
          MIO.fail(
            DerivationError.PatcherError(NotSupportedPatcherDerivation(Type.prettyPrint[A], Type.prettyPrint[Patch]))
          )
        case _ => attemptNextRule
      }
  }
}
