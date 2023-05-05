package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{DefinitionsPlatform, DerivationResult}
import io.scalaland.chimney.{partial, PartialTransformer, Transformer}

private[derivation] trait DerivationPlatform
    extends Derivation
    with rules.TransformSubtypesRuleModule
    with rules.TransformOptionToOptionRuleModule
    with rules.NotImplementedFallbackRuleModule {
  this: DefinitionsPlatform =>

  override protected val rulesAvailableForPlatform: List[Rule] =
    List(TransformSubtypesRule, TransformOptionToOptionRule, NotImplementedFallbackRule)
}
