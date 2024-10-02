package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformOptionToOptionRuleModule {
  this: Derivation & TransformProductToProductRuleModule =>

  import Type.Implicits.*, ChimneyType.Implicits.*, TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformOptionToOptionRule extends Rule("OptionToOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (OptionalValue(_), _) if Type[To] <:< Type[None.type] =>
          DerivationResult
            .notSupportedTransformerDerivation(ctx)
            .log(s"Discovered that target type is ${Type.prettyPrint[None.type]} which we explicitly reject")
        case (OptionalValue(from2), OptionalValue(to2)) =>
          import from2.{Underlying as InnerFrom, value as optionalFrom},
            to2.{Underlying as InnerTo, value as optionalTo}
          DerivationResult.log(
            s"Resolved ${Type.prettyPrint[From]} (${from2.value}) and ${Type.prettyPrint[To]} (${to2.value}) as optional types"
          ) >>
            mapOptions[From, To, InnerFrom, InnerTo](optionalFrom, optionalTo)
        case _ =>
          DerivationResult.attemptNextRule
      }

    private def mapOptions[From, To, InnerFrom: Type, InnerTo: Type](
        optionalFrom: OptionalValue[From, InnerFrom],
        optionalTo: OptionalValue[To, InnerTo]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      ExprPromise
        .promise[InnerFrom](ExprPromise.NameGenerationStrategy.FromType)
        .traverse { (newFromExpr: Expr[InnerFrom]) =>
          useOverrideIfPresentOr("matchingSome", ctx.config.filterCurrentOverridesForSome) {
            deriveRecursiveTransformationExpr[InnerFrom, InnerTo](
              newFromExpr,
              Path.Root.matching[Some[InnerFrom]],
              Path.Root.matching[Some[InnerTo]]
            )
          }
        }
        .flatMap { (derivedToExprPromise: ExprPromise[InnerFrom, TransformationExpr[InnerTo]]) =>
          derivedToExprPromise.foldTransformationExpr { (totalP: ExprPromise[InnerFrom, Expr[InnerTo]]) =>
            // We're constructing:
            // ${ src }.fold[$To](None, innerFrom: $InnerFrom => Some(${ innerFrom }))
            // but working with every OptionalValue
            DerivationResult.expandedTotal(
              optionalFrom.fold[To](
                ctx.src,
                optionalTo.empty,
                totalP.map(optionalTo.of).fulfilAsLambda[To]
              )
            )
          } { (partialP: ExprPromise[InnerFrom, Expr[partial.Result[InnerTo]]]) =>
            // We're constructing:
            // ${ src }.fold[$To](partial.Result.Value(None)) { innerFrom: $InnerFrom =>
            //   ${ derivedResultInnerTo }.map(Option(_))
            // }
            // but working with every OptionalValue
            DerivationResult.expandedPartial(
              optionalFrom.fold[partial.Result[To]](
                ctx.src,
                ChimneyExpr.PartialResult.Value(optionalTo.empty).upcastToExprOf[partial.Result[To]],
                partialP
                  .map { (derivedResultTo2: Expr[partial.Result[InnerTo]]) =>
                    derivedResultTo2.map(Expr.Function1.instance { (param: Expr[InnerTo]) =>
                      optionalTo.of(param)
                    })
                  }
                  .fulfilAsLambda[partial.Result[To]]
              )
            )
          }
        }
  }
}
