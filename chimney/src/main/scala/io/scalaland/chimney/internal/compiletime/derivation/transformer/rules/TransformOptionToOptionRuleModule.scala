package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformOptionToOptionRuleModule { this: Derivation =>

  import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformOptionToOptionRule extends Rule("OptionToOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (Type.Option(from2), Type.Option(to2)) =>
          ComputedType.use(from2) { implicit InnerFrom: Type[from2.Underlying] =>
            ComputedType.use(to2) { implicit InnerTo: Type[to2.Underlying] =>
              ExprPromise
                .promise[from2.Underlying](ExprPromise.NameGenerationStrategy.FromType)
                .traverse { (newFromExpr: Expr[from2.Underlying]) =>
                  deriveRecursiveTransformationExpr[from2.Underlying, to2.Underlying](newFromExpr)
                }
                .map { (derivedToExprPromise: ExprPromise[from2.Underlying, TransformationExpr[to2.Underlying]]) =>
                  derivedToExprPromise
                    .map(_.toEither)
                    .foldEither { (totalP: ExprPromise[from2.Underlying, Expr[to2.Underlying]]) =>
                      // We're constructing:
                      // '{ ${ src }.map(from2: $from2 => ${ derivedTo2 }) }
                      TransformationExpr.total(
                        totalP
                          .fulfilAsLambda { (lambda: Expr[from2.Underlying => to2.Underlying]) =>
                            Expr.Option.map(ctx.src.upcastExpr[Option[from2.Underlying]])(lambda)
                          }
                          .upcastExpr[To]
                      )
                    } { (partialP: ExprPromise[from2.Underlying, Expr[partial.Result[to2.Underlying]]]) =>
                      // We're constructing:
                      // ${ src }.fold[$To](partial.Result.Value(None)) { from2: $from2 =>
                      //   ${ derivedResultTo2 }.map(Option(_))
                      // }
                      TransformationExpr.partial(
                        partialP.map(ChimneyExpr.PartialResult.map(_)(Expr.Option.wrap)).fulfilAsLambda {
                          (lambda: Expr[from2.Underlying => partial.Result[Option[to2.Underlying]]]) =>
                            Expr.Option
                              .fold(ctx.src.upcastExpr[Option[from2.Underlying]])(
                                ChimneyExpr.PartialResult
                                  .Value(Expr.Option.None)
                                  .upcastExpr[partial.Result[Option[to2.Underlying]]]
                              )(
                                lambda
                              )
                              .upcastExpr[partial.Result[To]]
                        }
                      )
                    }
                }
                .map(Rule.ExpansionResult.Expanded(_))
            }
          }
        case _ =>
          DerivationResult.continue
      }
  }
}
