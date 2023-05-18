package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.{Definitions, DefinitionsPlatform, DerivationResult}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait NotImplementedFallbackRuleModule { this: DefinitionsPlatform & Derivation =>

  // TODO: remove this rule once all rules are migrated; it's here only to make the Scala 3 tests compile
  object NotImplementedFallbackRule extends Rule("NotImplementedFallback") {

    def expand[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult.totalExpr('{ ??? })
  }
}
