package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.{Log, MIO}
import hearth.fp.syntax.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformPartialOptionToNonOptionRuleModule { this: Derivation & hearth.MacroCommons =>

  import ChimneyType.Implicits.*

  protected object TransformPartialOptionToNonOptionRule extends Rule("PartialOptionToNonOption") {

    private lazy val OptionOfAnyType: Type[Option[Any]] = Type.of[Option[Any]]

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      Type[From] match {
        case (OptionalValue(from2)) if !(Type[To] <:< OptionOfAnyType) =>
          ctx match {
            case TransformationContext.ForPartial(_, _) =>
              if (ctx.config.flags.partialUnwrapsOption) {
                import from2.{Underlying as InnerFrom, value as optionalValue}
                Log.info(s"Resolved ${Type.prettyPrint[From]} (${from2.value}) as optional type") >>
                  mapOptionToPartial[From, To, InnerFrom](optionalValue)
              } else {
                attemptNextRuleBecause(
                  "Safe Option unwrapping was disabled by a flag"
                )
              }
            case _ =>
              attemptNextRuleBecause(
                "Safe Option unwrapping is available only for PartialTransformers"
              )
          }
        case _ => attemptNextRule
      }

    private def mapOptionToPartial[From, To, InnerFrom: Type](optionalValue: OptionalValue[From, InnerFrom])(implicit
        ctx: TransformationContext[From, To]
    ): MIO[Rule.ExpansionResult[To]] = {
      implicit val SomeInnerFromType: Type[Some[InnerFrom]] = Type.of[Some[InnerFrom]]
      LambdaBuilder
        .of1[InnerFrom]()
        .traverse { (from2Expr: Expr[InnerFrom]) =>
          deriveRecursiveTransformationExpr[InnerFrom, To](
            from2Expr,
            followFrom = Path(_.matching[Some[InnerFrom]].select("value"))
          ).map(_.ensurePartial)
        }
        .flatMap { (builder: LambdaBuilder[InnerFrom => *, Expr[partial.Result[To]]]) =>
          // We're constructing:
          // ${ src }.fold[partial.Result[$To]](partial.Result.empty, { innerFrom: $InnerFrom =>
          //   ${ derivedResultTo } // wrap if needed
          // })
          // but working with every OptionalValue
          expandedPartial(
            optionalValue.fold[partial.Result[To]](
              ctx.src,
              ChimneyExpr.PartialResult.fromEmpty[To],
              builder.build[partial.Result[To]]
            )
          )
        }
    }
  }
}
