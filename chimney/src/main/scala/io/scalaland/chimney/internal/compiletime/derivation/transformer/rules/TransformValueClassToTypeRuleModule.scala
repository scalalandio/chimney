package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait TransformValueClassToTypeRuleModule { this: Derivation =>

  object TransformValueClassToTypeRule extends Rule("ValueClassToType") {

    def expand[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[From] match {
        case ValueClass(from) =>
          implicit val InnerFrom: Type[from.Inner] = from.Inner
          deriveRecursiveTransformationExpr[from.Inner, To](from.unwrap(ctx.src))
            .map(Rule.ExpansionResult.Expanded(_))
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
