package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformEitherToEitherRuleModule {
  this: Derivation & TransformProductToProductRuleModule =>

  import Type.Implicits.*, ChimneyType.Implicits.*, TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformEitherToEitherRule extends Rule("EitherToEither") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (Type.Either.Left(fromL, fromR), Type.Either(toL, toR)) if !Type[To].isRight =>
          import fromL.Underlying as FromL, fromR.Underlying as FromR, toL.Underlying as ToL, toR.Underlying as ToR
          mapLeft[From, To, FromL, FromR, ToL, ToR]
        case (Type.Either.Right(fromL, fromR), Type.Either(toL, toR)) if !Type[To].isLeft =>
          import fromL.Underlying as FromL, fromR.Underlying as FromR, toL.Underlying as ToL, toR.Underlying as ToR
          mapRight[From, To, FromL, FromR, ToL, ToR]
        case (Type.Either(fromL, fromR), Type.Either(toL, toR)) =>
          import fromL.Underlying as FromL, fromR.Underlying as FromR, toL.Underlying as ToL, toR.Underlying as ToR
          mapEither[From, To, FromL, FromR, ToL, ToR]
        case _ => DerivationResult.attemptNextRule
      }

    private def mapLeft[From, To, FromL: Type, FromR: Type, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      useOverrideIfPresentOr("matchingLeft", ctx.config.filterCurrentOverridesForLeft) {
        deriveRecursiveTransformationExpr[FromL, ToL](
          ctx.src.upcastToExprOf[Left[FromL, FromR]].value,
          Path(_.matching[Left[FromL, FromR]].select("value")),
          Path(_.matching[Left[ToL, ToR]].select("value"))
        )
      }
        .flatMap { (derivedToL: TransformationExpr[ToL]) =>
          // We're constructing:
          // '{ Left( ${ derivedToL } ) /* from ${ src }.value */ }
          DerivationResult.expanded(derivedToL.map(Expr.Either.Left[ToL, ToR](_).upcastToExprOf[To]))
        }

    private def mapRight[From, To, FromL: Type, FromR: Type, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      useOverrideIfPresentOr("matchingRight", ctx.config.filterCurrentOverridesForRight) {
        deriveRecursiveTransformationExpr[FromR, ToR](
          ctx.src.upcastToExprOf[Right[FromL, FromR]].value,
          Path(_.matching[Right[FromL, FromR]].select("value")),
          Path(_.matching[Right[ToL, ToR]].select("value"))
        )
      }
        .flatMap { (derivedToR: TransformationExpr[ToR]) =>
          // We're constructing:
          // '{ Right( ${ derivedToR } ) /* from ${ src }.value */ }
          DerivationResult.expanded(derivedToR.map(Expr.Either.Right[ToL, ToR](_).upcastToExprOf[To]))
        }

    private def mapEither[From, To, FromL: Type, FromR: Type, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] = {
      val toLeftResult = ExprPromise
        .promise[FromL](ExprPromise.NameGenerationStrategy.FromPrefix("left"))
        .traverse { (leftExpr: Expr[FromL]) =>
          useOverrideIfPresentOr("matchingLeft", ctx.config.filterCurrentOverridesForLeft) {
            deriveRecursiveTransformationExpr[FromL, ToL](
              leftExpr,
              Path(_.matching[Left[FromL, FromR]].select("value")),
              Path(_.matching[Left[ToL, ToR]].select("value"))
            )
          }
        }

      val toRightResult = ExprPromise
        .promise[FromR](ExprPromise.NameGenerationStrategy.FromPrefix("right"))
        .traverse { (rightExpr: Expr[FromR]) =>
          useOverrideIfPresentOr("matchingRight", ctx.config.filterCurrentOverridesForRight) {
            deriveRecursiveTransformationExpr[FromR, ToR](
              rightExpr,
              Path(_.matching[Right[FromL, FromR]].select("value")),
              Path(_.matching[Right[ToL, ToR]].select("value"))
            )
          }
        }

      val inLeft =
        (expr: Expr[ToL]) => Expr.Either.Left[ToL, ToR](expr).upcastToExprOf[To]
      val inRight =
        (expr: Expr[ToR]) => Expr.Either.Right[ToL, ToR](expr).upcastToExprOf[To]

      toLeftResult
        .map2(toRightResult) {
          (toLeft: ExprPromise[FromL, TransformationExpr[ToL]], toRight: ExprPromise[FromR, TransformationExpr[ToR]]) =>
            (toLeft.exprPartition, toRight.exprPartition) match {
              case (Left(totalToLeft), Left(totalToRight)) =>
                // We're constructing:
                // '{ ${ src }.fold {
                //    left: $FromL => Left(${ derivedToL })
                // } {
                //    right: $FromR => Right(${ derivedToR })
                // }
                TransformationExpr.fromTotal(
                  ctx.src
                    .upcastToExprOf[Either[FromL, FromR]]
                    .fold[To](
                      totalToLeft.map(inLeft).fulfilAsLambda
                    )(
                      totalToRight.map(inRight).fulfilAsLambda
                    )
                )
              case _ =>
                // We're constructing:
                // '{ ${ src }.fold {
                //    left: $FromL => ${ derivedToL }.map(Left(_))
                // } {
                //    right: $FromR => ${ derivedToR }.map(Right(_))
                // }
                TransformationExpr.fromPartial(
                  ctx.src
                    .upcastToExprOf[Either[FromL, FromR]]
                    .fold[partial.Result[To]](
                      toLeft.map(_.ensurePartial.map[To](Expr.Function1.instance(inLeft))).fulfilAsLambda
                    )(
                      toRight.map(_.ensurePartial.map[To](Expr.Function1.instance(inRight))).fulfilAsLambda
                    )
                )
            }
        }
        .flatMap(DerivationResult.expanded)
    }
  }
}
