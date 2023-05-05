package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.Definitions
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait TransformSubtypesRuleModule { this: Definitions & Derivation =>

  object TransformSubtypesRule extends Rule {

    def isApplicableTo[From, To](implicit ctx: TransformerContext[From, To]): Boolean = Type[From] <:< Type[To]

    def apply[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] =
      DerivationResult.log(s"Matched ${Type.prettyPrint[From]} <:< ${Type.prettyPrint[To]} derivation rule") >>
        DerivationResult.pure(DerivedExpr.TotalExpr[To](ctx.src.asInstanceOfExpr[To]))
  }
}
