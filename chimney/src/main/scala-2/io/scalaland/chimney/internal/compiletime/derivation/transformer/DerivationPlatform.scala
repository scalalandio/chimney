package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[derivation] trait DerivationPlatform
    extends Derivation
    with DefinitionsPlatform
    with ImplicitSummoningPlatform
    with rules.TransformImplicitRuleModule
    with rules.TransformSubtypesRuleModule
    with rules.TransformOptionToOptionRuleModule
    with rules.TransformPartialOptionToNonOptionRuleModule
    with rules.TransformToOptionRuleModule
    with rules.LegacyMacrosFallbackRuleModule {

  final override protected val rulesAvailableForPlatform: List[Rule] = List(
    TransformImplicitRule,
    TransformSubtypesRule,
    TransformOptionToOptionRule,
    TransformPartialOptionToNonOptionRule,
    TransformToOptionRule,
    LegacyMacrosFallbackRule
  )
}
