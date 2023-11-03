package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformValueClassToTypeRuleModule {
  this: Derivation with TransformProductToProductRuleModule =>

  protected object TransformValueClassToTypeRule extends Rule("ValueClassToType") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[From] match {
        case ValueClassType(from2) =>
          if (ctx.config.areOverridesEmptyForCurrent[From, To]) {
            import from2.{Underlying as InnerFrom, value as valueFrom}
            unwrapAndTransform[From, To, InnerFrom](valueFrom)
          } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
        case _ => DerivationResult.attemptNextRule
      }

    private def unwrapAndTransform[From, To, InnerFrom: Type](
        valueFrom: ValueClass[From, InnerFrom]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ derivedTo } /* using ${ src }.from internally */ }
      deriveRecursiveTransformationExpr[InnerFrom, To](valueFrom.unwrap(ctx.src), KeepFieldOverrides)
        .flatMap(DerivationResult.expanded)
        // fall back to case classes expansion; see https://github.com/scalalandio/chimney/issues/297 for more info
        .orElse(TransformProductToProductRule.expand(ctx))
        .orElse(
          DerivationResult
            .notSupportedTransformerDerivationForField(valueFrom.fieldName)(ctx)
            .log(
              s"Failed to resolve derivation from ${Type.prettyPrint[InnerFrom]} (wrapped by ${Type
                  .prettyPrint[From]}) to ${Type.prettyPrint[To]}"
            )
        )
  }
}
