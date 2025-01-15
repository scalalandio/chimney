package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.compiletime.derivation.transformer

private[compiletime] trait DerivationPlatform
    extends Derivation
    with transformer.DerivationPlatform
    with rules.PatchImplicitRuleModule
    with rules.PatchSubtypeRuleModule
    with rules.PatchOptionWithOptionOptionModule
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
