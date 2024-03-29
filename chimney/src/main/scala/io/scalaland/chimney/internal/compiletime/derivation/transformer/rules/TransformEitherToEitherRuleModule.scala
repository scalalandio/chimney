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
      deriveRecursiveTransformationExpr[FromL, ToL](
        ctx.src.upcastExpr[Left[FromL, FromR]].value
      ).flatMap { (derivedToL: TransformationExpr[ToL]) =>
        // We're constructing:
        // '{ Left( ${ derivedToL } ) /* from ${ src }.value */ }
        DerivationResult.expanded(derivedToL.map(Expr.Either.Left[ToL, ToR](_).upcastExpr[To]))
      }

    private def mapRight[From, To, FromL: Type, FromR: Type, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      deriveRecursiveTransformationExpr[FromR, ToR](
        ctx.src.upcastExpr[Right[FromL, FromR]].value
      ).flatMap { (derivedToR: TransformationExpr[ToR]) =>
        // We're constructing:
        // '{ Right( ${ derivedToR } ) /* from ${ src }.value */ }
        DerivationResult.expanded(derivedToR.map(Expr.Either.Right[ToL, ToR](_).upcastExpr[To]))
      }

    private def mapEither[From, To, FromL: Type, FromR: Type, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] = {
      val toLeftResult = ExprPromise
        .promise[FromL](ExprPromise.NameGenerationStrategy.FromPrefix("left"))
        .traverse { (leftExpr: Expr[FromL]) =>
          deriveRecursiveTransformationExpr[FromL, ToL](leftExpr)
        }

      val toRightResult = ExprPromise
        .promise[FromR](ExprPromise.NameGenerationStrategy.FromPrefix("right"))
        .traverse { (rightExpr: Expr[FromR]) =>
          deriveRecursiveTransformationExpr[FromR, ToR](rightExpr)
        }

      val inLeft =
        (expr: Expr[ToL]) => Expr.Either.Left[ToL, ToR](expr).upcastExpr[To]
      val inRight =
        (expr: Expr[ToR]) => Expr.Either.Right[ToL, ToR](expr).upcastExpr[To]

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
                    .upcastExpr[Either[FromL, FromR]]
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
                    .upcastExpr[Either[FromL, FromR]]
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
