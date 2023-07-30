package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformValueClassToValueClassRuleModule { this: Derivation =>

  protected object TransformValueClassToValueClassRule extends Rule("ValueClassToValueClass") {

    @scala.annotation.nowarn("msg=Unreachable case")
    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (ValueClassType(from2), ValueClassType(to2)) =>
          import from2.{Underlying as From2, value as valueFrom}, to2.{Underlying as To2, value as valueTo}
          deriveRecursiveTransformationExpr[from2.Underlying, to2.Underlying](valueFrom.unwrap(ctx.src)).flatMap {
            (derivedTo2: TransformationExpr[to2.Underlying]) =>
              // We're constructing:
              // '{ ${ new $To(${ derivedTo2 }) } // using ${ src }.$from internally }
              DerivationResult.expanded(derivedTo2.map(valueTo.wrap))
          }
        case _ => DerivationResult.attemptNextRule
      }
  }
}
