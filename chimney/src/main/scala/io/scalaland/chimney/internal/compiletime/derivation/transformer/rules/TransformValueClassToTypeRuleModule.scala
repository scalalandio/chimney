package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

/** SMART-CONSTRUCTOR value types (`datatypes.ValueClasses.PartialWrapperClassType`) can be UNWRAPPED as sources in both
  * total and partial derivation (only their CONSTRUCTION is partial-only - see
  * [[TransformTypeToValueClassRuleModule]]).
  *
  * Flag gating (see `ValueClasses.WrapperClass.fromStdExtension`): only STRUCTURALLY matched (Method-based) wrappers
  * require the `nonAnyValWrappers` flag; extension-provided value types (Hearth `IsValueType` providers - both the
  * total and the smart-constructor ones) skip it, like the `integrations` implicits they replace.
  */
private[compiletime] trait TransformValueClassToTypeRuleModule {
  this: Derivation & TransformProductToProductRuleModule & TransformValueClassToValueClassRuleModule &
    hearth.MacroCommons =>

  protected object TransformValueClassToTypeRule extends Rule("ValueClassToType") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      Type[From] match {
        case ValueClassType(from2) =>
          if (ctx.config.areOverridesEmpty) {
            import from2.{Underlying as InnerFrom, value as valueFrom}
            // Java boxed primitives are NULLABLE - unwrap them only into their exact primitive counterpart (the
            // replaced chimney-java-collections implicits' semantics); anything else must reach the null-SAFE
            // ToOption/OptionToOption rules further down the list (see ValueClasses.isJavaBoxedPrimitive).
            if (isJavaBoxedPrimitive[From] && !(Type[InnerFrom] =:= Type[To]))
              attemptNextRuleBecause(
                "Java boxed primitives are only unwrapped into their exact primitive counterpart"
              )
            else unwrapAndTransform[From, To, InnerFrom](valueFrom.fieldName, valueFrom.unwrap)
          } else attemptNextRuleBecause("Configuration has defined overrides")
        case WrapperClassType(from2) =>
          if (ctx.config.areOverridesEmpty) {
            import from2.{Underlying as InnerFrom, value as valueFrom}
            // Extension-provided value types skip the flag - see ValueClasses.WrapperClass.fromStdExtension.
            if (ctx.config.flags.nonAnyValWrappers || valueFrom.fromStdExtension) {
              unwrapAndTransform[From, To, InnerFrom](valueFrom.fieldName, valueFrom.unwrap)
            } else
              attemptNextRuleBecause("Unwrapping from non-AnyVal wrapper types was disabled by a flag")
          } else attemptNextRuleBecause("Configuration has defined overrides")
        case PartialWrapperClassType(from2) =>
          // Smart-constructor value types: only their CONSTRUCTION is partial; unwrapping is total like any wrapper.
          // They are by construction ALWAYS extension-provided, so they are never gated behind the nonAnyValWrappers
          // flag - see ValueClasses.WrapperClass.fromStdExtension for the rationale.
          if (ctx.config.areOverridesEmpty) {
            import from2.{Underlying as InnerFrom, value as valueFrom}
            unwrapAndTransform[From, To, InnerFrom](valueFrom.fieldName, valueFrom.unwrap)
          } else attemptNextRuleBecause("Configuration has defined overrides")
        case _ => attemptNextRule
      }

    private def unwrapAndTransform[From, To, InnerFrom: Type](
        innerFromFieldName: String,
        unwrapFromIntoInnerFrom: Expr[From] => Expr[InnerFrom]
    )(implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ derivedTo } /* using ${ src }.from internally */ }
      deriveRecursiveTransformationExpr[InnerFrom, To](
        unwrapFromIntoInnerFrom(ctx.src),
        followFrom = Path(_.select(innerFromFieldName)),
        updateFallbacks = TransformValueClassToValueClassRule.unwrapFallbacksWherePossible[From, To]
      )
        .flatMap(expanded)
        // fall back to case classes expansion; see https://github.com/scalalandio/chimney/issues/297 for more info
        .orElse(TransformProductToProductRule.expand(ctx))
        .orElse(
          notSupportedTransformerDerivationForField(innerFromFieldName)(ctx)
            .logInfo(
              s"Failed to resolve derivation from ${Type.prettyPrint[InnerFrom]} (wrapped by ${Type
                  .prettyPrint[From]}) to ${Type.prettyPrint[To]}"
            )
        )
  }
}
