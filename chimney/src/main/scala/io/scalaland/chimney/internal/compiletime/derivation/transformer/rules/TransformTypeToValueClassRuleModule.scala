package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformTypeToValueClassRuleModule {
  this: Derivation & TransformProductToProductRuleModule =>

  protected object TransformTypeToValueClassRule extends Rule("TypeToValueClass") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case ValueClassType(to2) =>
          Existential.use(to2) { implicit To2: Type[to2.Underlying] => (valueTo: ValueClass[To, to2.Underlying]) =>
            deriveRecursiveTransformationExpr[From, to2.Underlying](ctx.src)
              .flatMap { derivedTo2 =>
                // We're constructing:
                // '{ new $To(${ derivedTo2 }) }
                DerivationResult.expanded(derivedTo2.map(valueTo.wrap))
              }
              // fall back to case classes expansion; see https://github.com/scalalandio/chimney/issues/297 for more info
              .orElse(TransformProductToProductRule.expand(ctx))
              .orElse(DerivationResult.notSupportedTransformerDerivationForField(valueTo.fieldName)(ctx))
          }
        case _ => DerivationResult.attemptNextRule
      }
  }
}
