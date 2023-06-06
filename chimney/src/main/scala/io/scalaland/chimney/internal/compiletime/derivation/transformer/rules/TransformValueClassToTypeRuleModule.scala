package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformValueClassToTypeRuleModule {
  this: Derivation & TransformProductToProductRuleModule =>

  protected object TransformValueClassToTypeRule extends Rule("ValueClassToType") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[From] match {
        case ValueClass(from) =>
          implicit val InnerFrom: Type[from.Inner] = from.Inner
          // We're constructing:
          // '{ ${ derivedTo } // using ${ src }.from internally }
          deriveRecursiveTransformationExpr[from.Inner, To](from.unwrap(ctx.src))
            .flatMap(DerivationResult.expanded)
            // fall back to case classes expansion; see https://github.com/scalalandio/chimney/issues/297 for more info
            .orElse(TransformProductToProductRule.expand(ctx))
            .orElse(DerivationResult.notSupportedTransformerDerivation[From, To, Rule.ExpansionResult[To]])
        case _ => DerivationResult.attemptNextRule
      }
  }
}
