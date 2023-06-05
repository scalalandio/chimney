package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformTypeToValueClassRuleModule { this: Derivation =>

  protected object TransformTypeToValueClassRule extends Rule("TypeToValueClass") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case ValueClass(to) =>
          implicit val InnerTo: Type[to.Inner] = to.Inner
          deriveRecursiveTransformationExpr[From, to.Inner](ctx.src)
            .map { transformationExpr =>
              Rule.ExpansionResult.Expanded(transformationExpr.map(to.wrap))
            }
            .orElse {
              // fall back to case classes expansion; see https://github.com/scalalandio/chimney/issues/297 for more info
              // TODO: fallback logging
              // TODO: ProductToProductRule(ctx).orElse {
              DerivationResult.notSupportedTransformerDerivation[From, To, Rule.ExpansionResult[To]]
              // TODO: }
            }
        case _ => DerivationResult.continue
      }
  }
}
