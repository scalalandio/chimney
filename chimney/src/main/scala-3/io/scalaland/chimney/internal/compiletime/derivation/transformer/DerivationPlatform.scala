package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform
import io.scalaland.chimney.internal.compiletime.datatypes

abstract private[compiletime] class DerivationPlatform(q: scala.quoted.Quotes)
    extends DefinitionsPlatform(using q)
    with Derivation
    with ImplicitSummoningPlatform
    with datatypes.ValueClassesPlatform
    with rules.TransformImplicitRuleModule
    with rules.TransformSubtypesRuleModule
    with rules.TransformOptionToOptionRuleModule
    with rules.TransformPartialOptionToNonOptionRuleModule
    with rules.TransformToOptionRuleModule
    with rules.TransformValueClassToValueClassRuleModule
    with rules.TransformValueClassToTypeRuleModule
    with rules.TransformTypeToValueClassRuleModule
    with rules.NotImplementedFallbackRuleModule {

  final override protected val rulesAvailableForPlatform: List[Rule] = List(
    TransformImplicitRule,
    TransformSubtypesRule,
    TransformOptionToOptionRule,
    TransformPartialOptionToNonOptionRule,
    TransformToOptionRule,
    TransformValueClassToValueClassRule,
    TransformValueClassToTypeRule,
    TransformTypeToValueClassRule,
    NotImplementedFallbackRule
  )
}
