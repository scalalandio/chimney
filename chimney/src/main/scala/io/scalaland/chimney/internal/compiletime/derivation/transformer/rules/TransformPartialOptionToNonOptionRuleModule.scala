package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformPartialOptionToNonOptionRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformPartialOptionToNonOptionRule extends Rule("PartialOptionToNonOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[From] match {
        case (Type.Option(from2)) if !Type[To].isOption =>
          ctx match {
            case TransformationContext.ForPartial(_, _) =>
              if (ctx.config.flags.partialUnwrapsOption) {
                import from2.Underlying as InnerFrom
                mapOptionToPartial[From, To, InnerFrom]
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

    private def mapOptionToPartial[From, To, InnerFrom: Type](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult
        .direct { (await: DerivationResult.Await[TransformationExpr[To]]) =>
          // We're constructing:
          // ${ src }.map[partial.Result[$To]] { innerFrom: $InnerFrom =>
          //   ${ derivedResultTo } // wrap if needed
          // }.getOrElse(partial.Result.empty)
          ctx.src
            .upcastToExprOf[Option[InnerFrom]]
            .map(Expr.Function1.instance[InnerFrom, partial.Result[To]] { (from2Expr: Expr[InnerFrom]) =>
              await(deriveRecursiveTransformationExpr[InnerFrom, To](from2Expr, Path.Root)).ensurePartial
            })
            .getOrElse(ChimneyExpr.PartialResult.fromEmpty[To])
        }
        .flatMap(DerivationResult.expandedPartial(_))
  }
}
