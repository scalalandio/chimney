package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformValueClassToValueClassRuleModule { this: Derivation =>

  protected object TransformValueClassToValueClassRule extends Rule("ValueClassToValueClass") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (ValueClassType(from2), ValueClassType(to2)) =>
          if (ctx.config.areOverridesEmpty) {
            import from2.{Underlying as InnerFrom, value as valueFrom}, to2.{Underlying as InnerTo, value as valueTo}
            unwrapTransformAndWrapAgain[From, To, InnerFrom, InnerTo](
              valueFrom.fieldName,
              valueFrom.unwrap,
              valueTo.fieldName,
              valueTo.wrap
            )
          } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
        case (WrapperClassType(from2), WrapperClassType(to2)) =>
          if (ctx.config.areOverridesEmpty) {
            if (ctx.config.flags.nonAnyValWrappers) {
              import from2.{Underlying as InnerFrom, value as valueFrom}, to2.{Underlying as InnerTo, value as valueTo}
              unwrapTransformAndWrapAgain[From, To, InnerFrom, InnerTo](
                valueFrom.fieldName,
                valueFrom.unwrap,
                valueTo.fieldName,
                valueTo.wrap
              )
            } else
              DerivationResult.attemptNextRuleBecause("Rewrapping non-AnyVal wrapper types was disabled by a flag")
          } else DerivationResult.attemptNextRuleBecause("Configuration has defined overrides")
        case _ => DerivationResult.attemptNextRule
      }

    // Exposed for TransformValueClassToTypeRuleModule
    def unwrapFallbacksWherePossible[From, To](implicit
        ctx: TransformationContext[From, To]
    ): TransformerOverride.ForFallback => Vector[TransformerOverride.ForFallback] = {
      val updates = ctx.config.filterCurrentOverridesForFallbacks.view.map {
        case original @ TransformerOverride.Fallback(fallback) =>
          import fallback.{Underlying as Fallback, value as fallbackExpr}

          val key: TransformerOverride.ForFallback = original
          val value: TransformerOverride.ForFallback = TransformerOverride.Fallback(Type[Fallback] match {
            case ValueClassType(fallback2) =>
              import fallback2.Underlying as InnerFallback
              fallback2.value.unwrap(fallbackExpr).as_??
            case WrapperClassType(fallback2) if ctx.config.flags.nonAnyValWrappers =>
              import fallback2.Underlying as InnerFallback
              fallback2.value.unwrap(fallbackExpr).as_??
            case _ =>
              fallback
          })

          key -> Vector(key, value).distinct
      }.toMap
      key => updates.getOrElse(key, Vector(key))
    }

    private def unwrapTransformAndWrapAgain[From, To, InnerFrom: Type, InnerTo: Type](
        innerFromFieldName: String,
        unwrapFromIntoInnerFrom: Expr[From] => Expr[InnerFrom],
        innerToFieldName: String,
        wrapInnerToIntoIo: Expr[InnerTo] => Expr[To]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      deriveRecursiveTransformationExpr[InnerFrom, InnerTo](
        unwrapFromIntoInnerFrom(ctx.src),
        followTo = Path(_.select(innerFromFieldName)),
        followFrom = Path(_.select(innerToFieldName)),
        updateFallbacks = unwrapFallbacksWherePossible[From, To]
      ).flatMap { (derivedInnerTo: TransformationExpr[InnerTo]) =>
        // We're constructing:
        // '{ ${ new $To(${ derivedInnerTo }) } /* using ${ src }.$from internally */ }
        DerivationResult.expanded(derivedInnerTo.map(wrapInnerToIntoIo))
      }
  }
}
