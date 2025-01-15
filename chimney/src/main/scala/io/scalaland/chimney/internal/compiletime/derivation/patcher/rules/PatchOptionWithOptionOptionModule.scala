package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation
import io.scalaland.chimney.internal.compiletime.derivation.transformer.rules.TransformOptionToOptionRuleModule

private[compiletime] trait PatchOptionWithOptionOptionModule { this: Derivation & TransformOptionToOptionRuleModule =>

  import Type.Implicits.*

  protected object PatchOptionWithOptionOptionRule extends Rule("PatchOptionWithOptionOption") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): DerivationResult[Rule.ExpansionResult[A]] =
      (Type[A], Type[Patch], ctx) match {
        case (OptionalValue(_), OptionalValue(patchOption), Patched(obj)) =>
          import patchOption.Underlying as InnerPatch
          Type[InnerPatch] match {
            case OptionalValue(_) =>
              DerivationResult.namedScope(s"Special handling of patching Option[A] with Option[Option[Patch]]") {
                ignoreNonePatchWithSomeOption[A, Patch, InnerPatch](obj, patchOption.value)
              }
            case _ => DerivationResult.attemptNextRule
          }

        case _ => DerivationResult.attemptNextRule
      }

    private def ignoreNonePatchWithSomeOption[OptionA, OptionOptionPatch, OptionPatch: Type](
        obj: Expr[OptionA],
        optionOptionPatch: OptionalValue[OptionOptionPatch, OptionPatch]
    )(implicit
        ctx: TransformationContext[OptionOptionPatch, OptionA]
    ): DerivationResult[Rule.ExpansionResult[OptionA]] =
      DerivationResult
        .direct[Expr[OptionA], Expr[OptionA]] { await =>
          // We're constructing:
          // '{ ${ src }.fold(${ obj })(optionPatch => optionA ) }
          // '{ ${ src }.map(optionPatchToOptionA).getOrElse(${ obj }) } }
          optionOptionPatch.fold[OptionA](
            ctx.src,
            obj,
            Expr.Function1.instance[OptionPatch, OptionA] { expr =>
              await(
                deriveRecursiveTransformationExpr[OptionPatch, OptionA](
                  expr,
                  followFrom = Path(_.matching[Some[OptionPatch]].select("value")),
                  updateFallbacks = _ => Vector.empty
                ).map(_.ensureTotal)
              )
            }
          )
        }
        .flatMap(DerivationResult.expandedTotal)
  }
}
