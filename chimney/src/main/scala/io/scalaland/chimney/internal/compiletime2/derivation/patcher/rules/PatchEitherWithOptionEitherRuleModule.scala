package io.scalaland.chimney.internal.compiletime2.derivation.patcher.rules

import hearth.fp.syntax.*
import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.patcher.Derivation

/** Hearth-based port of `...compiletime.derivation.patcher.rules.PatchEitherWithOptionEitherRuleModule`.
  *
  * Differences vs the old version: `Type.Either(_, _)` becomes `ScalaType.Either(_, _)` (`Ctor2.fromUntyped`-based,
  * baseType-aware like macro-commons), and the `DerivationResult.direct` + `Expr.Function1.instance` + `await(...)`
  * protocol becomes `LambdaBuilder.of1[OptionPatch]().traverse(...)` + `.build` (see
  * [[PatchOptionWithNonOptionRuleModule]] for the rationale).
  */
private[compiletime2] trait PatchEitherWithOptionEitherRuleModule { this: Derivation & hearth.MacroCommons =>

  import ScalaType.Implicits.*

  protected object PatchEitherWithOptionEitherRule extends Rule("PatchEitherWithOptionEither") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): DerivationResult[Rule.ExpansionResult[A]] =
      (Type[A], Type[Patch], ctx) match {
        case (ScalaType.Either(_, _), OptionalValue(patchOption), Patched(obj)) =>
          import patchOption.Underlying as InnerPatch
          Type[InnerPatch] match {
            case ScalaType.Either(_, _) =>
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
          // '{ ${ src }.fold(${ obj })(optionPatch => eitherA ) }
          DerivationResult.expandedTotal(
            optionEitherPatch.fold[OptionA](
              ctx.src,
              obj,
              builder.build[OptionA]
            )
          )
        }
  }
}
