package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{datatypes, ChimneyDefinitionsPlatform}

trait DerivationPlatform
    extends Derivation
    with ChimneyDefinitionsPlatform
    with datatypes.IterableOrArraysPlatform
    with datatypes.ProductTypesPlatform
    with datatypes.SealedHierarchiesPlatform
    with datatypes.ValueClassesPlatform
    with rules.TransformImplicitRuleModule
    with rules.TransformSubtypesRuleModule
    with rules.TransformToSingletonRuleModule
    with rules.TransformOptionToOptionRuleModule
    with rules.TransformPartialOptionToNonOptionRuleModule
    with rules.TransformToOptionRuleModule
    with rules.TransformValueClassToValueClassRuleModule
    with rules.TransformValueClassToTypeRuleModule
    with rules.TransformTypeToValueClassRuleModule
    with rules.TransformEitherToEitherRuleModule
    with rules.TransformMapToMapRuleModule
    with rules.TransformIterableToIterableRuleModule
    with rules.TransformProductToProductRuleModule
    with rules.TransformSealedHierarchyToSealedHierarchyRuleModule {

  final override protected val rulesAvailableForPlatform: List[Rule] = List(
    TransformImplicitRule,
    TransformSubtypesRule,
    TransformToSingletonRule,
    TransformOptionToOptionRule,
    TransformPartialOptionToNonOptionRule,
    TransformToOptionRule,
    TransformValueClassToValueClassRule,
    TransformValueClassToTypeRule,
    TransformTypeToValueClassRule,
    TransformEitherToEitherRule,
    TransformMapToMapRule,
    TransformIterableToIterableRule,
    TransformProductToProductRule,
    TransformSealedHierarchyToSealedHierarchyRule
  )
}
