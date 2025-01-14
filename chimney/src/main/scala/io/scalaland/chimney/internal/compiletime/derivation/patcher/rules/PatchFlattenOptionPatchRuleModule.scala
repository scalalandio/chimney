package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation

private[compiletime] trait PatchFlattenOptionPatchRuleModule { this: Derivation =>

  protected object PatchFlattenOptionPatchRule extends Rule("FlattenOptionPatch") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): DerivationResult[Rule.ExpansionResult[A]] =
      (Type[Patch], ctx) match {
        case (OptionalValue(patchOption), Patched(_)) =>
          import patchOption.Underlying as InnerPatch
          Type[InnerPatch] match {
            case OptionalValue(innerPatchOption) =>
              DerivationResult.namedScope(s"Flattening Optional values on Patch-side") {
                deriveRecursiveTransformationExpr[InnerPatch, A](
                  patchOption.value.getOrElse(ctx.src, innerPatchOption.value.empty)
                ).flatMap(DerivationResult.expanded)
              }
            case _ => DerivationResult.attemptNextRule
          }

        case _ => DerivationResult.attemptNextRule
      }
  }
}
