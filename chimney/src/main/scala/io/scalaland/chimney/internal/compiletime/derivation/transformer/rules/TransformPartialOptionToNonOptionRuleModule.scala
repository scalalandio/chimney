package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.syntax.*
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformPartialOptionToNonOptionRuleModule`.
  *
  * Differences vs the old version:
  *   - the old `DerivationResult.direct` + `Expr.Function1.instance` + `await(...)` protocol becomes
  *     `LambdaBuilder.of1[InnerFrom]().traverse(...)` + `.build` - the recursive derivation runs once, outside the
  *     lambda body, with identical error/log propagation (the lambda is passed to the runtime `OptionalValue.fold`
  *     iteration helper - a legitimate `LambdaBuilder` use),
  *   - `.log` becomes `.logInfo`, `Type[To].isOption` comes from the `ScalaStdTypeOps` compat ops.
  */
private[compiletime] trait TransformPartialOptionToNonOptionRuleModule { this: Derivation & hearth.MacroCommons =>

  import ChimneyType.Implicits.*, ScalaType.Implicits.*

  protected object TransformPartialOptionToNonOptionRule extends Rule("PartialOptionToNonOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[From] match {
        case (OptionalValue(from2)) if !Type[To].isOption =>
          ctx match {
            case TransformationContext.ForPartial(_, _) =>
              if (ctx.config.flags.partialUnwrapsOption) {
                import from2.{Underlying as InnerFrom, value as optionalValue}
                DerivationResult.log(s"Resolved ${Type.prettyPrint[From]} (${from2.value}) as optional type") >>
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
          DerivationResult.expandedPartial(
            optionalValue.fold[partial.Result[To]](
              ctx.src,
              ChimneyExpr.PartialResult.fromEmpty[To],
              builder.build[partial.Result[To]]
            )
          )
        }
  }
}
