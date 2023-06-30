package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformEitherToEitherRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformEitherToEitherRule extends Rule("EitherToEither") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (Type.Either.Left(fromL, fromR), Type.Either(toL, toR)) if !Type[To].isRight =>
          ExistentialType.use4(fromL, fromR, toL, toR) {
            implicit FromL: Type[fromL.Underlying] => implicit FromR: Type[fromR.Underlying] =>
              implicit ToL: Type[toL.Underlying] => implicit ToR: Type[toR.Underlying] =>
                deriveRecursiveTransformationExpr[fromL.Underlying, toL.Underlying](
                  ctx.src.upcastExpr[Left[fromL.Underlying, fromR.Underlying]].value
                ).flatMap { (derivedToL: TransformationExpr[toL.Underlying]) =>
                  // We're constructing:
                  // '{ Left( ${ derivedToL } ) // from ${ src }.value }
                  DerivationResult.expanded(
                    derivedToL.map(Expr.Either.Left[toL.Underlying, toR.Underlying](_).upcastExpr[To])
                  )
                }
          }
        case (Type.Either.Right(fromL, fromR), Type.Either(toL, toR)) if !Type[To].isLeft =>
          ExistentialType.use4(fromL, fromR, toL, toR) {
            implicit FromL: Type[fromL.Underlying] => implicit FromR: Type[fromR.Underlying] =>
              implicit ToL: Type[toL.Underlying] => implicit ToR: Type[toR.Underlying] =>
                deriveRecursiveTransformationExpr[fromR.Underlying, toR.Underlying](
                  ctx.src.upcastExpr[Right[fromL.Underlying, fromR.Underlying]].value
                ).flatMap { (derivedToR: TransformationExpr[toR.Underlying]) =>
                  // We're constructing:
                  // '{ Right( ${ derivedToR } ) // from ${ src }.value }
                  DerivationResult.expanded(
                    derivedToR.map(Expr.Either.Right[toL.Underlying, toR.Underlying](_).upcastExpr[To])
                  )
                }
          }
        case (Type.Either(fromL, fromR), Type.Either(toL, toR)) =>
          ExistentialType.use4(fromL, fromR, toL, toR) {
            implicit FromL: Type[fromL.Underlying] => implicit FromR: Type[fromR.Underlying] =>
              implicit ToL: Type[toL.Underlying] => implicit ToR: Type[toR.Underlying] =>
                val toLeftResult = ExprPromise
                  .promise[fromL.Underlying](ExprPromise.NameGenerationStrategy.FromPrefix("left"))
                  .traverse { (leftExpr: Expr[fromL.Underlying]) =>
                    deriveRecursiveTransformationExpr[fromL.Underlying, toL.Underlying](leftExpr)
                  }

                val toRightResult = ExprPromise
                  .promise[fromR.Underlying](ExprPromise.NameGenerationStrategy.FromPrefix("right"))
                  .traverse { (rightExpr: Expr[fromR.Underlying]) =>
                    deriveRecursiveTransformationExpr[fromR.Underlying, toR.Underlying](rightExpr)
                  }

                val inLeft =
                  (expr: Expr[toL.Underlying]) => Expr.Either.Left[toL.Underlying, toR.Underlying](expr).upcastExpr[To]
                val inRight =
                  (expr: Expr[toR.Underlying]) => Expr.Either.Right[toL.Underlying, toR.Underlying](expr).upcastExpr[To]

                toLeftResult
                  .map2(toRightResult) {
                    (
                        toLeft: ExprPromise[fromL.Underlying, TransformationExpr[toL.Underlying]],
                        toRight: ExprPromise[fromR.Underlying, TransformationExpr[toR.Underlying]]
                    ) =>
                      (toLeft.exprPartition, toRight.exprPartition) match {
                        case (Left(totalToLeft), Left(totalToRight)) =>
                          // We're constructing:
                          // '{ ${ src }.fold {
                          //    left: $fromL => Left(${ derivedToL })
                          // } {
                          //    right: $fromR => Right(${ derivedToR })
                          // }
                          TransformationExpr.fromTotal(
                            ctx.src
                              .upcastExpr[Either[fromL.Underlying, fromR.Underlying]]
                              .fold[To](
                                totalToLeft.map(inLeft).fulfilAsLambda
                              )(
                                totalToRight.map(inRight).fulfilAsLambda
                              )
                          )
                        case _ =>
                          // We're constructing:
                          // '{ ${ src }.fold {
                          //    left: $fromL => ${ derivedToL }.map(Left(_))
                          // } {
                          //    right: $fromR => ${ derivedToR }.map(Right(_))
                          // }
                          TransformationExpr.fromPartial(
                            ctx.src
                              .upcastExpr[Either[fromL.Underlying, fromR.Underlying]]
                              .fold[partial.Result[To]](
                                toLeft
                                  .map(_.ensurePartial.map[To](Expr.Function1.instance(inLeft)))
                                  .fulfilAsLambda
                              )(
                                toRight
                                  .map(_.ensurePartial.map[To](Expr.Function1.instance(inRight)))
                                  .fulfilAsLambda
                              )
                          )
                      }
                  }
                  .flatMap(DerivationResult.expanded)

          }
        case _ => DerivationResult.attemptNextRule
      }
  }
}
