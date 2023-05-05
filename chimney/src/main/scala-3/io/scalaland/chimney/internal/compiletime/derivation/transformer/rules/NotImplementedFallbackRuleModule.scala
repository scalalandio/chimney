package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.{Definitions, DefinitionsPlatform, DerivationResult}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait NotImplementedFallbackRuleModule { this: DefinitionsPlatform & Derivation =>

  object NotImplementedFallbackRule extends Rule {

    def isApplicableTo[From, To](implicit ctx: TransformerContext[From, To]): Boolean = true

    def apply[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] =
      DerivationResult.log(s"Matched fallback to ??? implementation derivation rule") >>
        DerivationResult.pure(DerivedExpr.TotalExpr[To]('{ ??? }))
  }
}
