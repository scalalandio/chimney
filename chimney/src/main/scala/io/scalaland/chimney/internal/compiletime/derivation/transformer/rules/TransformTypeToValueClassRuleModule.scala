package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformTypeToValueClassRuleModule {
  this: Derivation & TransformProductToProductRuleModule =>

  protected object TransformTypeToValueClassRule extends Rule("TypeToValueClass") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case ValueClass(to) =>
          implicit val InnerTo: Type[to.Inner] = to.Inner
          deriveRecursiveTransformationExpr[From, to.Inner](ctx.src)
            .map { transformationExpr =>
              // We're constructing:
              // '{ new $To(${ derivedTo2 }) }
              Rule.ExpansionResult.Expanded(transformationExpr.map(to.wrap))
            }
            // fall back to case classes expansion; see https://github.com/scalalandio/chimney/issues/297 for more info
            .orElse(TransformProductToProductRule.expand(ctx))
            .orElse(DerivationResult.notSupportedTransformerDerivation[From, To, Rule.ExpansionResult[To]])
        case _ => DerivationResult.continue
      }
  }
}
