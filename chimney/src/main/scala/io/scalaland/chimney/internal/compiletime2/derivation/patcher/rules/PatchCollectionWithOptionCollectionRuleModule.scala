package io.scalaland.chimney.internal.compiletime2.derivation.patcher.rules

import hearth.fp.syntax.*
import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.patcher.Derivation

/** Hearth-based port of `...compiletime.derivation.patcher.rules.PatchCollectionWithOptionCollectionRuleModule`.
  *
  * Differences vs the old version: the `DerivationResult.direct` + `Expr.Function1.instance` + `await(...)` protocol
  * becomes `LambdaBuilder.of1[OptionPatch]().traverse(...)` + `.build` (see [[PatchOptionWithNonOptionRuleModule]] for
  * the rationale).
  */
private[compiletime2] trait PatchCollectionWithOptionCollectionRuleModule { this: Derivation & hearth.MacroCommons =>

  import ScalaType.Implicits.*

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
          DerivationResult.expandedTotal(
            optionOptionPatch.fold[OptionA](
              ctx.src,
              obj,
              builder.build[OptionA]
            )
          )
        }
  }
}
