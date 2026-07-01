package io.scalaland.chimney.internal.compiletime2.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.transformer.Derivation

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformValueClassToTypeRuleModule` - 1:1 copy
  * (`.log` becomes `.logInfo`; the `TransformProductToProductRule.expand` fallback currently hits the not-yet-ported
  * heavy rule's stub, see [[TransformProductToProductRuleModule]]).
  */
private[compiletime2] trait TransformValueClassToTypeRuleModule {
  this: Derivation & TransformProductToProductRuleModule & TransformValueClassToValueClassRuleModule &
    hearth.MacroCommons =>

  protected object TransformValueClassToTypeRule extends Rule("ValueClassToType") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[From] match {
        case ValueClassType(from2) =>
          if (ctx.config.areOverridesEmpty) {
            import from2.{Underlying as InnerFrom, value as valueFrom}
            unwrapAndTransform[From, To, InnerFrom](valueFrom.fieldName, valueFrom.unwrap)
          } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
        case WrapperClassType(from2) =>
          if (ctx.config.areOverridesEmpty) {
            if (ctx.config.flags.nonAnyValWrappers) {
              import from2.{Underlying as InnerFrom, value as valueFrom}
              unwrapAndTransform[From, To, InnerFrom](valueFrom.fieldName, valueFrom.unwrap)
            } else
              DerivationResult.attemptNextRuleBecause("Unwrapping from non-AnyVal wrapper types was disabled by a flag")
          } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
        case _ => DerivationResult.attemptNextRule
      }

    private def unwrapAndTransform[From, To, InnerFrom: Type](
        innerFromFieldName: String,
        unwrapFromIntoInnerFrom: Expr[From] => Expr[InnerFrom]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ derivedTo } /* using ${ src }.from internally */ }
      deriveRecursiveTransformationExpr[InnerFrom, To](
        unwrapFromIntoInnerFrom(ctx.src),
        followFrom = Path(_.select(innerFromFieldName)),
        updateFallbacks = TransformValueClassToValueClassRule.unwrapFallbacksWherePossible[From, To]
      )
        .flatMap(DerivationResult.expanded)
        // fall back to case classes expansion; see https://github.com/scalalandio/chimney/issues/297 for more info
        .orElse(TransformProductToProductRule.expand(ctx))
        .orElse(
          DerivationResult
            .notSupportedTransformerDerivationForField(innerFromFieldName)(ctx)
            .logInfo(
              s"Failed to resolve derivation from ${Type.prettyPrint[InnerFrom]} (wrapped by ${Type
                  .prettyPrint[From]}) to ${Type.prettyPrint[To]}"
            )
        )
  }
}
