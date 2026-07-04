package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

/** SMART-CONSTRUCTOR value types (Hearth `IsValueType` extensions whose `wrap` is a `CtorLikeOf.Either*OrValue` - see
  * `datatypes.ValueClasses.PartialWrapperClassType`) are supported in PARTIAL derivation: the inner value is derived
  * recursively (like for any wrapper) and then passed through the provider's validating constructor, whose error
  * channel maps onto `partial.Result` ([[io.scalaland.chimney.internal.compiletime.CtorLikeExprs]]). In a TOTAL context
  * the rule yields with an attempt-next reason, so Total derivation fails with the usual meaningful "Chimney can't
  * derive ..." error.
  *
  * Flag gating (see `ValueClasses.WrapperClass.fromStdExtension`): only STRUCTURALLY matched (Method-based) wrappers
  * require the `nonAnyValWrappers` flag; extension-provided value types (Hearth `IsValueType` providers - both the
  * total and the smart-constructor ones) skip it, like the `integrations` implicits they replace.
  */
private[compiletime] trait TransformTypeToValueClassRuleModule {
  this: Derivation & TransformProductToProductRuleModule & hearth.MacroCommons =>

  protected object TransformTypeToValueClassRule extends Rule("TypeToValueClass") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      Type[To] match {
        case ValueClassType(to2) =>
          if (ctx.config.areOverridesEmpty) {
            import to2.{Underlying as InnerTo, value as valueTo}
            // Java boxed primitives are only wrapped from their exact primitive counterpart - the mirror of the
            // ValueClassToType restriction (see ValueClasses.isJavaBoxedPrimitive for the rationale).
            if (isJavaBoxedPrimitive[To] && !(Type[From] =:= Type[InnerTo]))
              attemptNextRuleBecause(
                "Java boxed primitives are only wrapped from their exact primitive counterpart"
              )
            else transformToInnerToAndWrap[From, To, InnerTo](valueTo.fieldName, valueTo.wrap)
          } else attemptNextRuleBecause("Configuration has defined overrides")
        case WrapperClassType(to2) =>
          if (ctx.config.areOverridesEmpty) {
            import to2.{Underlying as InnerTo, value as valueTo}
            // Extension-provided value types skip the flag - see ValueClasses.WrapperClass.fromStdExtension.
            if (ctx.config.flags.nonAnyValWrappers || valueTo.fromStdExtension) {
              transformToInnerToAndWrap[From, To, InnerTo](valueTo.fieldName, valueTo.wrap)
            } else
              attemptNextRuleBecause("Wrapping in non-AnyVal wrapper types was disabled by a flag")
          } else attemptNextRuleBecause("Configuration has defined overrides")
        case PartialWrapperClassType(to2) =>
          // Smart-constructor value types are by construction ALWAYS extension-provided (only Hearth IsValueType
          // providers can supply a validating CtorLike), so they are never gated behind the nonAnyValWrappers flag -
          // see ValueClasses.WrapperClass.fromStdExtension for the rationale.
          if (ctx.config.areOverridesEmpty) {
            ctx match {
              case TransformationContext.ForPartial(_, _) =>
                import to2.{Underlying as InnerTo, value as valueTo}
                transformToInnerToAndWrapPartially[From, To, InnerTo](valueTo.fieldName, valueTo.partialWrap)
              case TransformationContext.ForTotal(_) =>
                attemptNextRuleBecause(
                  s"Only smart-constructor (partial) wrapping available for ${Type.prettyPrint[To]}, in total context"
                )
            }
          } else attemptNextRuleBecause("Configuration has defined overrides")
        case _ => attemptNextRule
      }
  }

  private def transformToInnerToAndWrap[From, To, InnerTo: Type](
      innerToFieldName: String,
      wrapInnerToIntoTo: Expr[InnerTo] => Expr[To]
  )(implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
    deriveRecursiveTransformationExpr[From, InnerTo](
      ctx.src,
      followTo = Path(_.select(innerToFieldName))
    )
      .flatMap { derivedInnerTo =>
        // We're constructing:
        // '{ new $To(${ derivedInnerTo }) }
        expanded(derivedInnerTo.map(wrapInnerToIntoTo))
      }
      // fall back to case classes expansion; see https://github.com/scalalandio/chimney/issues/297 for more info
      .orElse(TransformProductToProductRule.expand(ctx))
      .orElse(
        notSupportedTransformerDerivationForField(innerToFieldName)(ctx)
          .logInfo(
            s"Failed to resolve derivation from ${Type.prettyPrint[From]} to ${Type
                .prettyPrint[InnerTo]} (wrapped by ${Type.prettyPrint[To]})"
          )
      )

  private def transformToInnerToAndWrapPartially[From, To, InnerTo: Type](
      innerToFieldName: String,
      wrapInnerToIntoPartialTo: Expr[InnerTo] => Expr[io.scalaland.chimney.partial.Result[To]]
  )(implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
    deriveRecursiveTransformationExpr[From, InnerTo](
      ctx.src,
      followTo = Path(_.select(innerToFieldName))
    )
      .flatMap { derivedInnerTo =>
        // We're constructing:
        // '{ partial.Result.fromEither*(${ smartCtor }(${ derivedInnerTo })) }
        // (flatMapped into the inner partial.Result when the inner derivation itself was partial)
        expanded(
          derivedInnerTo.flatMap(innerTo => TransformationExpr.fromPartial(wrapInnerToIntoPartialTo(innerTo)))
        )
      }
      // fall back to case classes expansion; see https://github.com/scalalandio/chimney/issues/297 for more info
      .orElse(TransformProductToProductRule.expand(ctx))
      .orElse(
        notSupportedTransformerDerivationForField(innerToFieldName)(ctx)
          .logInfo(
            s"Failed to resolve derivation from ${Type.prettyPrint[From]} to ${Type
                .prettyPrint[InnerTo]} (wrapped by smart constructor of ${Type.prettyPrint[To]})"
          )
      )
}
