package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import hearth.fp.effect.{Log, MIO}
import hearth.fp.syntax.*
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation

private[compiletime] trait PatchEitherWithOptionEitherRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object PatchEitherWithOptionEitherRule extends Rule("PatchEitherWithOptionEither") {

    private lazy val EitherCtor: Type.Ctor2[Either] = Type.Ctor2.of[Either]

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): MIO[Rule.ExpansionResult[A]] =
      (Type[A], Type[Patch], ctx) match {
        case (EitherCtor(_, _), OptionalValue(patchOption), Patched(obj)) =>
          import patchOption.Underlying as InnerPatch
          Type[InnerPatch] match {
            case EitherCtor(_, _) =>
              Log.namedScope(s"Special handling of patching Either[K, V] with Option[Either[K2, V2]]") {
                ignoreNonePatchWithSomeEither[A, Patch, InnerPatch](obj, patchOption.value)
              }
            case _ => attemptNextRule
          }

        case _ => attemptNextRule
      }

    private def ignoreNonePatchWithSomeEither[OptionA, OptionEitherPatch, OptionPatch: Type](
        obj: Expr[OptionA],
        optionEitherPatch: OptionalValue[OptionEitherPatch, OptionPatch]
    )(implicit
        ctx: TransformationContext[OptionEitherPatch, OptionA]
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
          // '{ ${ src }.fold(${ obj })(optionPatch => eitherA ) }
          expandedTotal(
            optionEitherPatch.fold[OptionA](
              ctx.src,
              obj,
              builder.build[OptionA]
            )
          )
        }
    }
  }
}
