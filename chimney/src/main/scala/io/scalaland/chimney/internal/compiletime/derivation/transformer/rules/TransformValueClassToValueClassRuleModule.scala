package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformValueClassToValueClassRuleModule { this: Derivation =>

  protected object TransformValueClassToValueClassRule extends Rule("ValueClassToValueClass") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (ValueClass(from), ValueClass(to)) =>
          implicit val InnerFrom: Type[from.Inner] = from.Inner
          implicit val InnerTo: Type[to.Inner] = to.Inner
          deriveRecursiveTransformationExpr[from.Inner, to.Inner](from.unwrap(ctx.src)).map { transformationExpr =>
            // TODO: append from2.fieldName to partial.Result
            Rule.ExpansionResult.Expanded(transformationExpr.map(to.wrap))
          }
        case _ => DerivationResult.continue
      }
  }
}
