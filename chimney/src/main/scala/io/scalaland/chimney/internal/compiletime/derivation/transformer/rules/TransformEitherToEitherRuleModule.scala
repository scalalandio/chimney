package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformEitherToEitherRuleModule { this: Derivation =>

  import TypeImplicits.* // , ChimneyTypeImplicits.*

  protected object TransformEitherToEitherRule extends Rule("EitherToEither") {

    // TODO: find out what to append to error path

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (Type.Either.Left(fromL, fromR), Type.Either(toL, toR)) if !Type[To].isRight =>
          ComputedType.use4(fromL, fromR, toL, toR) {
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
          ComputedType.use4(fromL, fromR, toL, toR) {
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
          // We're constructing:
          // '{ ${ src }.fold {
          //    left: $fromL => Left(${ derivedToL })
          // } {
          //    right: $fromR => Right(${ derivedToR })
          // }

          // We're constructing:
          // '{ ${ src }.fold {
          //    left: $fromL => ${ derivedToL }.map(Left(_))
          // } {
          //    right: $fromR => ${ derivedToR }.map(Right(_))
          // }
          val _ = (fromL, fromR, toL, toR)
          DerivationResult.attemptNextRule // TODO
        case _ => DerivationResult.attemptNextRule
      }
  }
}
