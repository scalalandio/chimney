package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformSubtypesRuleModule { this: Derivation =>

  protected object TransformSubtypesRule extends Rule("Subtypes") {

    override def expand[From, To](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      // We're constructing:
      // '{ ${ src } : $To } }
      if (Type[From] <:< Type[To]) DerivationResult.expandedTotal(ctx.src.upcastExpr[To])
      else DerivationResult.attemptNextRule
  }
}
