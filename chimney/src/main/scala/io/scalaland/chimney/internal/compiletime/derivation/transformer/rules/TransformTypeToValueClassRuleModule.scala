package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait TransformTypeToValueClassRuleModule { this: Derivation =>

  object TransformTypeToValueClassRule extends Rule("TypeToValueClass") {

    def expand[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case ValueClass(to) =>
          implicit val InnerTo: Type[to.Inner] = to.Inner
          deriveRecursiveTransformationExpr[From, to.Inner](ctx.src)
            .map { derivedExpr =>
              Rule.ExpansionResult.Expanded(derivedExpr.map(to.wrap))
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
