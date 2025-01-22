package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation

private[compiletime] trait PatchCollectionWithOptionCollectionRuleModule { this: Derivation =>

  import Type.Implicits.*

  protected object PatchCollectionWithOptionCollectionRule extends Rule("PatchCollectionWithOptionCollection") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): DerivationResult[Rule.ExpansionResult[A]] =
      (Type[A], Type[Patch], ctx) match {
        case (TotallyOrPartiallyBuildIterable(_), OptionalValue(patchOption), Patched(obj)) =>
          import patchOption.Underlying as InnerPatch
          Type[InnerPatch] match {
            case TotallyOrPartiallyBuildIterable(_) =>
              DerivationResult.namedScope(s"Special handling of patching Collection[A] with Option[Collection[Patch]]") {
                ignoreNonePatchWithSomeCollection[A, Patch, InnerPatch](obj, patchOption.value)
              }
            case _ => DerivationResult.attemptNextRule
          }

        case _ => DerivationResult.attemptNextRule
      }

    private def ignoreNonePatchWithSomeCollection[OptionA, OptionOptionPatch, OptionPatch: Type](
        obj: Expr[OptionA],
        optionOptionPatch: OptionalValue[OptionOptionPatch, OptionPatch]
    )(implicit
        ctx: TransformationContext[OptionOptionPatch, OptionA]
    ): DerivationResult[Rule.ExpansionResult[OptionA]] =
      DerivationResult
        .direct[Expr[OptionA], Expr[OptionA]] { await =>
          // We're constructing:
          // '{ ${ src }.fold(${ obj })(optionPatch => collectionA ) }
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
