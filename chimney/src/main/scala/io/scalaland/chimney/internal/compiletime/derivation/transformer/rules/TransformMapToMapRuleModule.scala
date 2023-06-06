package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformMapToMapRuleModule { this: Derivation & TransformIterableToIterableRuleModule =>

  // import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformMapToMapRule extends Rule("MapToMap") {

    // TODO: on fail append key/value path

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (Type.Map(fromK, fromV), Type.Map(toK, toV)) =>
          ctx.fold { total =>
            val _ = (fromK, fromV, toK, toV)
            // TODO: fallback log
            TransformIterableToIterableRule.expand(ctx)
          } { partial =>
            // Nope, we need partials!

            // We're constructing:
            // '{ ( ${ src }.map { case (k, v) => ${ derivedToK } -> ${ derivedToL } } ) }
            // TODO fix the above

            val _ = (fromK, fromV, toK, toV)
            DerivationResult.continue // TODO
          }
        case _ => DerivationResult.continue
      }
  }
}
