package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait TransformSubtypesRuleModule { this: Derivation =>

  object TransformSubtypesRule extends Rule("Subtypes") {

    override def expand[From, To](implicit
        ctx: TransformerContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      if (Type[From] <:< Type[To]) DerivationResult.totalExpr(ctx.src.asInstanceOfExpr[To])
      else DerivationResult.continue
  }
}
