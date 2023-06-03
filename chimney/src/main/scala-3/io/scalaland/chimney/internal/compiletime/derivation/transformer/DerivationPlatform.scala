package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform
import io.scalaland.chimney.{partial, PartialTransformer, Transformer}

abstract private[derivation] class DerivationPlatform(q: scala.quoted.Quotes)
    extends DefinitionsPlatform(using q)
    with Derivation
    with ImplicitSummoningPlatform
    with rules.TransformImplicitRuleModule
    with rules.TransformSubtypesRuleModule
    with rules.TransformOptionToOptionRuleModule
    with rules.TransformPartialOptionToNonOptionRuleModule
    with rules.NotImplementedFallbackRuleModule {

  final override protected val rulesAvailableForPlatform: List[Rule] = List(
    TransformImplicitRule,
    TransformSubtypesRule,
    TransformOptionToOptionRule,
    TransformPartialOptionToNonOptionRule,
    NotImplementedFallbackRule
  )
}
