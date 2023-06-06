package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait TransformMapToMapRuleModule { this: Derivation & TransformIterableToIterableRuleModule =>

  // import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformMapToMapRule extends Rule("MapToMap") {

    // TODO: on fail append key/value path

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (ctx, Type[From], Type[To]) match {
        case (TransformationContext.ForTotal(_), Type.Map(_, _), Type.Map(_, _)) =>
          // TODO: fallback log
          TransformIterableToIterableRule.expand(ctx)
        case (TransformationContext.ForPartial(src, failFast), Type.Map(fromK, fromV), Type.Map(toK, toV)) =>
          // Nope, we need partials!

          // We're constructing:
          // '{ ( ${ src }.map { case (k, v) => ${ derivedToK } -> ${ derivedToL } } ) }
          // TODO fix the above

          val _ = (src, failFast, fromK, fromV, toK, toV)
          DerivationResult.attemptNextRule // TODO
        case _ => DerivationResult.attemptNextRule
      }
  }
}
