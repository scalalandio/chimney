package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformPartialOptionToNonOptionRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformPartialOptionToNonOptionRule extends Rule("PartialOptionToNonOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[From] match {
        case (OptionalValue(from2)) if !Type[To].isOption =>
          ctx match {
            case TransformationContext.ForPartial(_, _) =>
              if (ctx.config.flags.partialUnwrapsOption) {
                import from2.{Underlying as InnerFrom, value as optionalValue}
                mapOptionToPartial[From, To, InnerFrom](optionalValue)
              } else {
                DerivationResult.attemptNextRuleBecause(
                  "Safe Option unwrapping was disabled by a flag"
                )
              }
            case _ =>
              DerivationResult.attemptNextRuleBecause(
                "Safe Option unwrapping is available only for PartialTransformers"
              )
          }
        case _ => DerivationResult.attemptNextRule
      }

    private def mapOptionToPartial[From, To, InnerFrom: Type](optionalValue: OptionalValue[From, InnerFrom])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult
        .direct { (await: DerivationResult.Await[TransformationExpr[To]]) =>
          // We're constructing:
          // ${ src }.fold[partial.Result[$To]](partial.Result.empty, { innerFrom: $InnerFrom =>
          //   ${ derivedResultTo } // wrap if needed
          // })
          // but working with every OptionalValue
          optionalValue.fold[partial.Result[To]](
            ctx.src,
            ChimneyExpr.PartialResult.fromEmpty[To],
            Expr.Function1.instance[InnerFrom, partial.Result[To]] { (from2Expr: Expr[InnerFrom]) =>
              await(deriveRecursiveTransformationExpr[InnerFrom, To](from2Expr, Path.Root)).ensurePartial
            }
          )
        }
        .flatMap(DerivationResult.expandedPartial(_))
  }
}
