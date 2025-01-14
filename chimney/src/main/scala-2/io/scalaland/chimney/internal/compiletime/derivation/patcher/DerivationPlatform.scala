package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.compiletime.derivation.transformer

private[compiletime] trait DerivationPlatform
    extends Derivation
    with transformer.DerivationPlatform
    with rules.PatchImplicitRuleModule
    with rules.PatchFlattenOptionPatchRuleModule
    with rules.PatchOptionWithNonOptionRuleModule {

  override protected val rulesAvailableForPlatform: List[Rule] = List(
    PatchImplicitRule,
    TransformImplicitRule,
    TransformImplicitOuterTransformerRule,
    PatchOptionWithNonOptionRule,
    PatchFlattenOptionPatchRule,
    TransformOptionToOptionRule,
    TransformToOptionRule,
    TransformSubtypesRule,
    TransformToSingletonRule,
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
