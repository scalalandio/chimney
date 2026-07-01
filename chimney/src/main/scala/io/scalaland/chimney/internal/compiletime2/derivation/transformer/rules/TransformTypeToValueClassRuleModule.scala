package io.scalaland.chimney.internal.compiletime2.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.transformer.Derivation

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformTypeToValueClassRuleModule` - 1:1 copy
  * (`.log` becomes `.logInfo`; the `TransformProductToProductRule.expand` fallback currently hits the not-yet-ported
  * heavy rule's stub, see [[TransformProductToProductRuleModule]]).
  */
private[compiletime2] trait TransformTypeToValueClassRuleModule {
  this: Derivation & TransformProductToProductRuleModule & hearth.MacroCommons =>

  protected object TransformTypeToValueClassRule extends Rule("TypeToValueClass") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case ValueClassType(to2) =>
          if (ctx.config.areOverridesEmpty) {
            import to2.{Underlying as InnerTo, value as valueTo}
            transformToInnerToAndWrap[From, To, InnerTo](valueTo.fieldName, valueTo.wrap)
          } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
        case WrapperClassType(to2) =>
          if (ctx.config.areOverridesEmpty) {
            if (ctx.config.flags.nonAnyValWrappers) {
              import to2.{Underlying as InnerTo, value as valueTo}
              transformToInnerToAndWrap[From, To, InnerTo](valueTo.fieldName, valueTo.wrap)
            } else
              DerivationResult.attemptNextRuleBecause("Wrapping in non-AnyVal wrapper types was disabled by a flag")
          } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
        case _ => DerivationResult.attemptNextRule
      }
  }

  private def transformToInnerToAndWrap[From, To, InnerTo: Type](
      innerToFieldName: String,
      wrapInnerToIntoTo: Expr[InnerTo] => Expr[To]
  )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
    deriveRecursiveTransformationExpr[From, InnerTo](
      ctx.src,
      followTo = Path(_.select(innerToFieldName))
    )
      .flatMap { derivedInnerTo =>
        // We're constructing:
        // '{ new $To(${ derivedInnerTo }) }
        DerivationResult.expanded(derivedInnerTo.map(wrapInnerToIntoTo))
      }
      // fall back to case classes expansion; see https://github.com/scalalandio/chimney/issues/297 for more info
      .orElse(TransformProductToProductRule.expand(ctx))
      .orElse(
        DerivationResult
          .notSupportedTransformerDerivationForField(innerToFieldName)(ctx)
          .logInfo(
            s"Failed to resolve derivation from ${Type.prettyPrint[From]} to ${Type
                .prettyPrint[InnerTo]} (wrapped by ${Type.prettyPrint[To]})"
          )
      )
}
