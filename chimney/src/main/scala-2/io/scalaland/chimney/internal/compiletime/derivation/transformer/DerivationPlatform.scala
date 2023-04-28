package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[derivation] trait DerivationPlatform
    extends Derivation
    with rules.TransformSubtypesRuleModule
    with rules.LegacyMacrosFallbackRuleModule {
  this: DefinitionsPlatform =>

  override protected val rulesAvailableForPlatform: Seq[Rule] = Seq(TransformSubtypesRule, LegacyMacrosFallbackRule)
}
