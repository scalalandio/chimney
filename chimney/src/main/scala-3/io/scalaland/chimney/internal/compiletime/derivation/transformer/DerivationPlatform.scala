package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{DefinitionsPlatform, DerivationResult}
import io.scalaland.chimney.{partial, PartialTransformer, Transformer}

private[derivation] trait DerivationPlatform extends Derivation { this: DefinitionsPlatform =>

  override protected val rulesAvailableForPlatform: Seq[Rule] = Seq()
}
