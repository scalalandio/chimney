package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import hearth.fp.syntax.*
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation

private[compiletime] trait PatchOptionWithNonOptionRuleModule { this: Derivation & hearth.MacroCommons =>

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
      LambdaBuilder
        .of1[InnerPatch]()
        .traverse { (expr: Expr[InnerPatch]) =>
          deriveRecursiveTransformationExpr[InnerPatch, A](expr, updateFallbacks = _ => Vector.empty)
            .map(_.ensureTotal)
        }
        .flatMap { builder =>
          DerivationResult.expandedTotal(
            patchOption.fold(
              ctx.src,
              obj,
              builder.build[A]
            )
          )
        }
  }
}
