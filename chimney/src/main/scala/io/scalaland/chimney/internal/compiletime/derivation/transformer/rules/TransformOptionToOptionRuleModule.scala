package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial
import io.scalaland.chimney.partial.Result

private[compiletime] trait TransformOptionToOptionRuleModule { this: Derivation =>

  import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformOptionToOptionRule extends Rule("OptionToOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (Type.Option(from2), Type.Option(to2)) =>
          ComputedType.use2(from2, to2) {
            implicit From2: Type[from2.Underlying] => implicit To2: Type[to2.Underlying] =>
              ExprPromise
                .promise[from2.Underlying](ExprPromise.NameGenerationStrategy.FromType)
                .traverse { (newFromExpr: Expr[from2.Underlying]) =>
                  deriveRecursiveTransformationExpr[from2.Underlying, to2.Underlying](newFromExpr)
                }
                .flatMap { (derivedToExprPromise: ExprPromise[from2.Underlying, TransformationExpr[to2.Underlying]]) =>
                  derivedToExprPromise
                    .map(_.toEither)
                    .foldEither { (totalP: ExprPromise[from2.Underlying, Expr[to2.Underlying]]) =>
                      // We're constructing:
                      // '{ ${ src }.map(from2: $from2 => ${ derivedTo2 }) }
                      DerivationResult.expandedTotal(
                        totalP
                          .fulfilAsLambda { (lambda: Expr[from2.Underlying => to2.Underlying]) =>
                            ctx.src.upcastExpr[Option[from2.Underlying]].map(lambda)
                          }
                          .upcastExpr[To]
                      )
                    } { (partialP: ExprPromise[from2.Underlying, Expr[partial.Result[to2.Underlying]]]) =>
                      // We're constructing:
                      // ${ src }.fold[$To](partial.Result.Value(None)) { from2: $from2 =>
                      //   ${ derivedResultTo2 }.map(Option(_))
                      // }
                      DerivationResult.expandedPartial(
                        partialP
                          .map { (derivedResultTo2: Expr[Result[to2.Underlying]]) =>
                            derivedResultTo2.map(Expr.Option.wrap)
                          }
                          .fulfilAsLambda {
                            (lambda: Expr[from2.Underlying => partial.Result[Option[to2.Underlying]]]) =>
                              ctx.src
                                .upcastExpr[Option[from2.Underlying]]
                                .fold(
                                  ChimneyExpr.PartialResult
                                    .Value(Expr.Option.None)
                                    .upcastExpr[partial.Result[Option[to2.Underlying]]]
                                )(lambda)
                                .upcastExpr[partial.Result[To]]
                          }
                      )
                    }
                }
          }
        case _ =>
          DerivationResult.attemptNextRule
      }
  }
}
