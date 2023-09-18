package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformValueClassToValueClassRuleModule { this: Derivation =>

  protected object TransformValueClassToValueClassRule extends Rule("ValueClassToValueClass") {

    @scala.annotation.nowarn("msg=Unreachable case")
    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (ValueClassType(from2), ValueClassType(to2)) =>
          import from2.{Underlying as InnerFrom, value as valueFrom}, to2.{Underlying as InnerTo, value as valueTo}
          unwrapTransformAndWrapAgain[From, To, InnerFrom, InnerTo](valueFrom.unwrap, valueTo.wrap)
        case _ => DerivationResult.attemptNextRule
      }
  }

  private def unwrapTransformAndWrapAgain[From, To, InnerFrom: Type, InnerTo: Type](
      unwrap: Expr[From] => Expr[InnerFrom],
      wrap: Expr[InnerTo] => Expr[To]
  )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
    deriveRecursiveTransformationExpr[InnerFrom, InnerTo](unwrap(ctx.src)).flatMap {
      (derivedInnerTo: TransformationExpr[InnerTo]) =>
        // We're constructing:
        // '{ ${ new $To(${ derivedInnerTo }) } /* using ${ src }.$from internally */ }
        DerivationResult.expanded(derivedInnerTo.map(wrap))
    }
}
