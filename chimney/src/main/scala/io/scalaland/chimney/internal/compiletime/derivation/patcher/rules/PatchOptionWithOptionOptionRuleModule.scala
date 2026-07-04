package io.scalaland.chimney.internal.compiletime.derivation.patcher.rules

import hearth.fp.effect.{Log, MIO}
import hearth.fp.syntax.*
import io.scalaland.chimney.internal.compiletime.derivation.patcher.Derivation

private[compiletime] trait PatchOptionWithOptionOptionRuleModule { this: Derivation & hearth.MacroCommons =>

  protected object PatchOptionWithOptionOptionRule extends Rule("PatchOptionWithOptionOption") {

    def expand[Patch, A](implicit ctx: TransformationContext[Patch, A]): MIO[Rule.ExpansionResult[A]] =
      (Type[A], Type[Patch], ctx) match {
        case (OptionalValue(_), OptionalValue(patchOption), Patched(obj)) =>
          import patchOption.Underlying as InnerPatch
          Type[InnerPatch] match {
            case OptionalValue(_) =>
              Log.namedScope(s"Special handling of patching Option[A] with Option[Option[Patch]]") {
                ignoreNonePatchWithSomeOption[A, Patch, InnerPatch](obj, patchOption.value)
              }
            case _ => attemptNextRule
          }

        case _ => attemptNextRule
      }

    private def ignoreNonePatchWithSomeOption[OptionA, OptionOptionPatch, OptionPatch: Type](
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
          // '{ ${ src }.fold(${ obj })(optionPatch => optionA ) }
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
