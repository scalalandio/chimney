package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait TransformValueClassToValueClassRuleModule { this: Derivation =>

  object TransformValueClassToValueClassRule extends Rule("ValueClassToValueClass") {

    def expand[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (ValueClass(from), ValueClass(to)) =>
          implicit val InnerFrom: Type[from.Inner] = from.Inner
          implicit val InnerTo: Type[to.Inner] = to.Inner
          deriveRecursiveTransformationExpr[from.Inner, to.Inner](from.unwrap(ctx.src)).map { derivedExpr =>
            // TODO: append from2.fieldName to partial.Result
            Rule.ExpansionResult.Expanded(derivedExpr.map(to.wrap))
          }
        case _ => DerivationResult.continue
      }
  }
}
