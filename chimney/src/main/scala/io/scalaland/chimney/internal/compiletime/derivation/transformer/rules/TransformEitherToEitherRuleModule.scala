package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformEitherToEitherRuleModule { this: Derivation =>

  // import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformEitherToEitherRule extends Rule("EitherToEither") {

    // TODO: find out what to append to error path

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (Type.Either.Left(fromL, fromR), Type.Either(toL, toR)) if !Type[To].isRight =>
          // We're constructing:
          // '{ Left( ${ derivedToL } ) // from ${ src }.value }
          val _ = (fromL, fromR, toL, toR)
          DerivationResult.continue // TODO
        case (Type.Either.Right(fromL, fromR), Type.Either(toL, toR)) if !Type[To].isLeft =>
          // We're constructing:
          // '{ Right( ${ derivedToR } ) // from ${ src }.value }
          val _ = (fromL, fromR, toL, toR)
          DerivationResult.continue // TODO
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
          DerivationResult.continue // TODO
        case _ => DerivationResult.continue
      }
  }
}
