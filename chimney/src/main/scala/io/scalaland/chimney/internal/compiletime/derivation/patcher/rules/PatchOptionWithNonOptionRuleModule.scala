package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation

private[compiletime] trait PatchOptionWithNonOptionRuleModule { this: Derivation =>

  protected object PatchOptionWithNonOptionRule extends Rule("PatchOptionWithNonOption") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): DerivationResult[Rule.ExpansionResult[A]] =
      (Type[A], Type[Patch], ctx) match {
        case (OptionalValue(_), _, _)                      => DerivationResult.attemptNextRule
        case (_, OptionalValue(patchOption), Patched(obj)) =>
          import patchOption.Underlying as InnerPatch
          patchNonOptionWithOption[A, Patch, InnerPatch](obj, patchOption.value)
        case _ => DerivationResult.attemptNextRule
      }

    private def patchNonOptionWithOption[A, Patch, InnerPatch: Type](
        obj: Expr[A],
        patchOption: OptionalValue[Patch, InnerPatch]
    )(implicit ctx: TransformationContext[Patch, A]): DerivationResult[Rule.ExpansionResult[A]] =
      DerivationResult
        .direct[Expr[A], Expr[A]] { await =>
          patchOption.fold(
            ctx.src,
            obj,
            Expr.Function1.instance(expr =>
              await(
                deriveRecursiveTransformationExpr[InnerPatch, A](expr, updateFallbacks = _ => Vector.empty)
                  .map(_.ensureTotal)
              )
            )
          )
        }
        .flatMap(DerivationResult.expandedTotal)
  }
}
