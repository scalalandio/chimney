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
            .log(
              s"Discovered that target type is ${Type.prettyPrint[None.type]} which we explicitly reject"
            )
        case (Type.Option(from2), Type.Option(to2)) =>
          import from2.Underlying as From2, to2.Underlying as To2
          ExprPromise
            .promise[from2.Underlying](ExprPromise.NameGenerationStrategy.FromType)
            .traverse { (newFromExpr: Expr[from2.Underlying]) =>
              deriveRecursiveTransformationExpr[from2.Underlying, to2.Underlying](newFromExpr)
            }
            .flatMap { (derivedToExprPromise: ExprPromise[from2.Underlying, TransformationExpr[to2.Underlying]]) =>
              derivedToExprPromise.foldTransformationExpr {
                (totalP: ExprPromise[from2.Underlying, Expr[to2.Underlying]]) =>
                  // We're constructing:
                  // '{ ${ src }.map(from2: $from2 => ${ derivedTo2 }) }
                  DerivationResult.expandedTotal(
                    ctx.src
                      .upcastExpr[Option[from2.Underlying]]
                      .map(totalP.fulfilAsLambda[to2.Underlying])
                      .upcastExpr[To]
                  )
              } { (partialP: ExprPromise[from2.Underlying, Expr[partial.Result[to2.Underlying]]]) =>
                // We're constructing:
                // ${ src }.fold[$To](partial.Result.Value(None)) { from2: $from2 =>
                //   ${ derivedResultTo2 }.map(Option(_))
                // }
                DerivationResult.expandedPartial(
                  ctx.src
                    .upcastExpr[Option[from2.Underlying]]
                    .fold(
                      ChimneyExpr.PartialResult
                        .Value(Expr.Option.None)
                        .upcastExpr[partial.Result[Option[to2.Underlying]]]
                    )(
                      partialP
                        .map { (derivedResultTo2: Expr[partial.Result[to2.Underlying]]) =>
                          derivedResultTo2.map(Expr.Function1.instance { (param: Expr[to2.Underlying]) =>
                            Expr.Option(param)
                          })
                        }
                        .fulfilAsLambda[partial.Result[Option[to2.Underlying]]]
                    )
                    .upcastExpr[partial.Result[To]]
                )
              }
            }
        case _ =>
          DerivationResult.attemptNextRule
      }
  }
}
