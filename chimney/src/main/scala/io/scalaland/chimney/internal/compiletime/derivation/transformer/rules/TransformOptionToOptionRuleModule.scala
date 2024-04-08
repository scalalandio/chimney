package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformOptionToOptionRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformOptionToOptionRule extends Rule("OptionToOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case _ if Type[From].isOption && Type[To] <:< Type[None.type] =>
          DerivationResult
            .notSupportedTransformerDerivation(ctx)
            .log(s"Discovered that target type is ${Type.prettyPrint[None.type]} which we explicitly reject")
        case (Type.Option(from2), Type.Option(to2)) =>
          import from2.Underlying as InnerFrom, to2.Underlying as InnerTo
          mapOptions[From, To, InnerFrom, InnerTo]
        case _ =>
          DerivationResult.attemptNextRule
      }

    private def mapOptions[From, To, InnerFrom: Type, InnerTo: Type](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      ExprPromise
        .promise[InnerFrom](ExprPromise.NameGenerationStrategy.FromType)
        .traverse { (newFromExpr: Expr[InnerFrom]) =>
          deriveRecursiveTransformationExpr[InnerFrom, InnerTo](newFromExpr, Path.Root.matching[Some[InnerTo]])
        }
        .flatMap { (derivedToExprPromise: ExprPromise[InnerFrom, TransformationExpr[InnerTo]]) =>
          derivedToExprPromise.foldTransformationExpr { (totalP: ExprPromise[InnerFrom, Expr[InnerTo]]) =>
            // We're constructing:
            // '{ ${ src }.map(innerFrom: $InnerFrom => ${ derivedInnerTo }) }
            DerivationResult.expandedTotal(
              ctx.src
                .upcastExpr[Option[InnerFrom]]
                .map(totalP.fulfilAsLambda[InnerTo])
                .upcastExpr[To]
            )
          } { (partialP: ExprPromise[InnerFrom, Expr[partial.Result[InnerTo]]]) =>
            // We're constructing:
            // ${ src }.fold[$To](partial.Result.Value(None)) { innerFrom: $InnerFrom =>
            //   ${ derivedResultInnerTo }.map(Option(_))
            // }
            DerivationResult.expandedPartial(
              ctx.src
                .upcastExpr[Option[InnerFrom]]
                .fold(
                  ChimneyExpr.PartialResult
                    .Value(Expr.Option.None)
                    .upcastExpr[partial.Result[Option[InnerTo]]]
                )(
                  partialP
                    .map { (derivedResultTo2: Expr[partial.Result[InnerTo]]) =>
                      derivedResultTo2.map(Expr.Function1.instance { (param: Expr[InnerTo]) =>
                        Expr.Option(param)
                      })
                    }
                    .fulfilAsLambda[partial.Result[Option[InnerTo]]]
                )
                .upcastExpr[partial.Result[To]]
            )
          }
        }
  }
}
