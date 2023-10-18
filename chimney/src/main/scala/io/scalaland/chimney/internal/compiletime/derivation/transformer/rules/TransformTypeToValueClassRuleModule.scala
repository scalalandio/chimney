package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformTypeToValueClassRuleModule {
  this: Derivation & TransformProductToProductRuleModule =>

  protected object TransformTypeToValueClassRule extends Rule("TypeToValueClass") {

    @scala.annotation.nowarn("msg=Unreachable case")
    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case ValueClassType(to2) =>
          import to2.{Underlying as InnerTo, value as valueTo}
          transformToInnerToAndWrap[From, To, InnerTo](valueTo)
        case _ => DerivationResult.attemptNextRule
      }
  }

  private def transformToInnerToAndWrap[From, To, InnerTo: Type](
      valueTo: ValueClass[To, InnerTo]
  )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
    deriveRecursiveTransformationExpr[From, InnerTo](
      ctx.src,
      OnRecur(fromField = KeepFieldOverrides, toField = DownField(valueTo.fieldName))
    )
      .flatMap { derivedInnerTo =>
        // We're constructing:
        // '{ new $To(${ derivedInnerTo }) }
        DerivationResult.expanded(derivedInnerTo.map(valueTo.wrap))
      }
      // fall back to case classes expansion; see https://github.com/scalalandio/chimney/issues/297 for more info
      .orElse(TransformProductToProductRule.expand(ctx))
      .orElse(
        DerivationResult
          .notSupportedTransformerDerivationForField(valueTo.fieldName)(ctx)
          .log(
            s"Failed to resolve derivation from ${Type.prettyPrint[From]} to ${Type
                .prettyPrint[InnerTo]} (wrapped by ${Type.prettyPrint[To]})"
          )
      )
}
