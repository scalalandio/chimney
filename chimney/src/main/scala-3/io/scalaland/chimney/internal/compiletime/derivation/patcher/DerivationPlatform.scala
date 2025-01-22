package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.compiletime.derivation.transformer

abstract private[compiletime] class DerivationPlatform(q: scala.quoted.Quotes)
    extends transformer.DerivationPlatform(q)
    with Derivation
    with rules.PatchImplicitRuleModule
    with rules.PatchSubtypeRuleModule
    with rules.PatchOptionWithOptionOptionRuleModule
    with rules.PatchEitherWithOptionEitherRuleModule
    with rules.PatchCollectionWithOptionCollectionRuleModule
    with rules.PatchOptionWithNonOptionRuleModule
    with rules.PatchProductWithProductRuleModule
    with rules.PatchNotMatchedRuleModule {

  override protected val rulesAvailableForPlatform: List[Rule] = List(
    PatchImplicitRule,
    TransformImplicitRule,
    TransformImplicitOuterTransformerRule,
    PatchSubtypeRuleModule,
    PatchOptionWithNonOptionRule,
    PatchOptionWithOptionOptionRule,
    PatchEitherWithOptionEitherRule,
    PatchCollectionWithOptionCollectionRule,
    TransformOptionToOptionRule,
    TransformToOptionRule,
    TransformToSingletonRule,
    TransformValueClassToValueClassRule,
    TransformValueClassToTypeRule,
    TransformTypeToValueClassRule,
    TransformEitherToEitherRule,
    TransformMapToMapRule,
    TransformIterableToIterableRule,
    PatchProductWithProductRule,
    TransformSealedHierarchyToSealedHierarchyRule,
    TransformSubtypesRule,
    PatchNotMatchedRule
  )
}
