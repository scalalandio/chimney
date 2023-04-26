package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{DefinitionsPlatform, DerivationResult}
import io.scalaland.chimney.{partial, PartialTransformer, Transformer}

private[derivation] trait DerivationPlatform
    extends Derivation
    with rules.TransformSubtypesRuleModule
    with rules.NotImplementedFallbackRuleModule {
  this: DefinitionsPlatform =>

// TODO: stack overflow error on Scala 3 when TransformSubtypesRule enabled

//  override protected val rulesAvailableForPlatform: Seq[Rule] =
//    Seq(TransformSubtypesRule, NotImplementedFallbackRule)

  override protected val rulesAvailableForPlatform: Seq[Rule] =
    Seq(NotImplementedFallbackRule)
}
