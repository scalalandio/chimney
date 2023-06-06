package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.{Definitions, DefinitionsPlatform, DerivationResult}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.{Derivation, DerivationPlatform}

private[compiletime] trait NotImplementedFallbackRuleModule { this: DerivationPlatform =>

  // TODO: remove this rule once all rules are migrated; it's here only to make the Scala 3 tests compile
  protected object NotImplementedFallbackRule extends Rule("NotImplementedFallback") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult.expandedTotal('{ ??? })
  }
}
