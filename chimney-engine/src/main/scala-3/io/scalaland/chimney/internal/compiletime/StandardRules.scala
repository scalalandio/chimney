package io.scalaland.chimney.internal.compiletime

/** All Rules implemented for this platform. */
trait StandardRules
    extends derivation.transformer.rules.TransformImplicitRuleModule
    with derivation.transformer.rules.TransformSubtypesRuleModule
    with derivation.transformer.rules.TransformToSingletonRuleModule
    with derivation.transformer.rules.TransformOptionToOptionRuleModule
    with derivation.transformer.rules.TransformPartialOptionToNonOptionRuleModule
    with derivation.transformer.rules.TransformToOptionRuleModule
    with derivation.transformer.rules.TransformValueClassToValueClassRuleModule
    with derivation.transformer.rules.TransformValueClassToTypeRuleModule
    with derivation.transformer.rules.TransformTypeToValueClassRuleModule
    with derivation.transformer.rules.TransformEitherToEitherRuleModule
    with derivation.transformer.rules.TransformMapToMapRuleModule
    with derivation.transformer.rules.TransformIterableToIterableRuleModule
    with derivation.transformer.rules.TransformProductToProductRuleModule
    with derivation.transformer.rules.TransformSealedHierarchyToSealedHierarchyRuleModule { this: DerivationEngine => }
