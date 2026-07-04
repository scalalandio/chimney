package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import hearth.fp.effect.{Log, MIO}
import hearth.fp.syntax.*
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation

private[compiletime] trait PatchCollectionWithOptionCollectionRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object PatchCollectionWithOptionCollectionRule extends Rule("PatchCollectionWithOptionCollection") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): MIO[Rule.ExpansionResult[A]] =
      (Type[A], Type[Patch], ctx) match {
        case (TotallyOrPartiallyBuildIterable(_), OptionalValue(patchOption), Patched(obj)) =>
          import patchOption.Underlying as InnerPatch
          Type[InnerPatch] match {
            case TotallyOrPartiallyBuildIterable(_) =>
              Log.namedScope(s"Special handling of patching Collection[A] with Option[Collection[Patch]]") {
                ignoreNonePatchWithSomeCollection[A, Patch, InnerPatch](obj, patchOption.value)
              }
            case _ => attemptNextRule
          }

        case _ => attemptNextRule
      }

    private def ignoreNonePatchWithSomeCollection[OptionA, OptionOptionPatch, OptionPatch: Type](
        obj: Expr[OptionA],
        optionOptionPatch: OptionalValue[OptionOptionPatch, OptionPatch]
    )(implicit
        ctx: TransformationContext[OptionOptionPatch, OptionA]
    ): MIO[Rule.ExpansionResult[OptionA]] = {
      implicit val SomeOptionPatchType: Type[Some[OptionPatch]] = Type.of[Some[OptionPatch]]
      LambdaBuilder
        .of1[OptionPatch]()
        .traverse { (expr: Expr[OptionPatch]) =>
          deriveRecursiveTransformationExpr[OptionPatch, OptionA](
            expr,
            followFrom = Path(_.matching[Some[OptionPatch]].select("value")),
            updateFallbacks = _ => Vector.empty
          ).map(_.ensureTotal)
        }
        .flatMap { builder =>
          // We're constructing:
          // '{ ${ src }.fold(${ obj })(optionPatch => collectionA ) }
          expandedTotal(
            optionOptionPatch.fold[OptionA](
              ctx.src,
              obj,
              builder.build[OptionA]
            )
          )
        }
    }
  }
}
