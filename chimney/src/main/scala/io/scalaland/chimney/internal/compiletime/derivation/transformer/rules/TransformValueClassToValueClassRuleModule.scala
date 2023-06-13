package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformValueClassToValueClassRuleModule { this: Derivation =>

  protected object TransformValueClassToValueClassRule extends Rule("ValueClassToValueClass") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (ValueClass(from2), ValueClass(to2)) =>
          Existential.use2(from2, to2) {
            implicit From2: Type[from2.Underlying] => implicit To2: Type[to2.Underlying] =>
              (valueFrom: ValueClass[From, from2.Underlying], valueTo: ValueClass[To, to2.Underlying]) =>
                deriveRecursiveTransformationExpr[from2.Underlying, to2.Underlying](valueFrom.unwrap(ctx.src)).flatMap {
                  (derivedTo2: TransformationExpr[to2.Underlying]) =>
                    // TODO: append from2.fieldName to partial.Result ?
                    // We're constructing:
                    // '{ ${ new $To(${ derivedTo2 }) } // using ${ src }.$from internally }
                    DerivationResult.expanded(derivedTo2.map(valueTo.wrap))
                }
          }
        case _ => DerivationResult.attemptNextRule
      }
  }
}
