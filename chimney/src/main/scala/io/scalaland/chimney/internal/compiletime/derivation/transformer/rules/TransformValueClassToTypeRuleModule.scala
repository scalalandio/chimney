package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformValueClassToTypeRuleModule` - 1:1 copy
  * (`.log` becomes `.logInfo`; the `TransformProductToProductRule.expand` fallback currently hits the not-yet-ported
  * heavy rule's stub, see [[TransformProductToProductRuleModule]]).
  *
  * Phase 5 addition: SMART-CONSTRUCTOR value types (`datatypes.ValueClasses.PartialWrapperClassType`) can be UNWRAPPED
  * as sources in both total and partial derivation (only their CONSTRUCTION is partial-only - see
  * [[TransformTypeToValueClassRuleModule]]). Gated behind the `nonAnyValWrappers` flag like every other non-AnyVal
  * wrapper.
  */
private[compiletime] trait TransformValueClassToTypeRuleModule {
  this: Derivation & TransformProductToProductRuleModule & TransformValueClassToValueClassRuleModule &
    hearth.MacroCommons =>

  protected object TransformValueClassToTypeRule extends Rule("ValueClassToType") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[From] match {
        case ValueClassType(from2) =>
          if (ctx.config.areOverridesEmpty) {
            import from2.{Underlying as InnerFrom, value as valueFrom}
            // Java boxed primitives are NULLABLE - unwrap them only into their exact primitive counterpart (the
            // replaced chimney-java-collections implicits' semantics); anything else must reach the null-SAFE
            // ToOption/OptionToOption rules further down the list (see ValueClasses.isJavaBoxedPrimitive).
            if (isJavaBoxedPrimitive[From] && !(Type[InnerFrom] =:= Type[To]))
              DerivationResult.attemptNextRuleBecause(
                "Java boxed primitives are only unwrapped into their exact primitive counterpart"
              )
            else unwrapAndTransform[From, To, InnerFrom](valueFrom.fieldName, valueFrom.unwrap)
          } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
        case WrapperClassType(from2) =>
          if (ctx.config.areOverridesEmpty) {
            if (ctx.config.flags.nonAnyValWrappers) {
              import from2.{Underlying as InnerFrom, value as valueFrom}
              unwrapAndTransform[From, To, InnerFrom](valueFrom.fieldName, valueFrom.unwrap)
            } else
              DerivationResult.attemptNextRuleBecause("Unwrapping from non-AnyVal wrapper types was disabled by a flag")
          } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
        case PartialWrapperClassType(from2) =>
          // Smart-constructor value types: only their CONSTRUCTION is partial; unwrapping is total like any wrapper.
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
