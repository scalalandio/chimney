package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation

private[compiletime] trait PatchEitherWithOptionEitherRuleModule { this: Derivation =>

  import Type.Implicits.*

  protected object PatchEitherWithOptionEitherRule extends Rule("PatchEitherWithOptionEither") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): DerivationResult[Rule.ExpansionResult[A]] =
      (Type[A], Type[Patch], ctx) match {
        case (Type.Either(_, _), OptionalValue(patchOption), Patched(obj)) =>
          import patchOption.Underlying as InnerPatch
          Type[InnerPatch] match {
            case Type.Either(_, _) =>
              DerivationResult.namedScope(s"Special handling of patching Either[K, V] with Option[Either[K2, V2]]") {
                ignoreNonePatchWithSomeEither[A, Patch, InnerPatch](obj, patchOption.value)
              }
            case _ => DerivationResult.attemptNextRule
          }

        case _ => DerivationResult.attemptNextRule
      }

    private def ignoreNonePatchWithSomeEither[OptionA, OptionEitherPatch, OptionPatch: Type](
        obj: Expr[OptionA],
        optionEitherPatch: OptionalValue[OptionEitherPatch, OptionPatch]
    )(implicit
        ctx: TransformationContext[OptionEitherPatch, OptionA]
    ): DerivationResult[Rule.ExpansionResult[OptionA]] =
      DerivationResult
        .direct[Expr[OptionA], Expr[OptionA]] { await =>
          // We're constructing:
          // '{ ${ src }.fold(${ obj })(optionPatch => eitherA ) }
          optionEitherPatch.fold[OptionA](
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
