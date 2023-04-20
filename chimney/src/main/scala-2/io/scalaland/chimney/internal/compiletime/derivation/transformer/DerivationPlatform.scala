package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[derivation] trait DerivationPlatform extends Derivation with DerivationWithLegacyMacros {
  this: DefinitionsPlatform =>

  override protected val rulesAvailableForPlatform: Seq[Rule] = Seq(LegacyMacrosRule)
}
