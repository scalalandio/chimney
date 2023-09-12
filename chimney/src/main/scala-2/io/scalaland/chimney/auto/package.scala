package io.scalaland.chimney

import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros

import scala.language.experimental.macros

package object auto {

  implicit def deriveAutomaticTransformer[From, To]: Transformer[From, To] =
    macro TransformerMacros.deriveTotalTransformerWithDefaults[From, To]

  implicit def deriveAutomaticPartialTransformer[From, To]: PartialTransformer[From, To] =
    macro TransformerMacros.derivePartialTransformerWithDefaults[From, To]

  implicit def deriveAutomaticPatcher[From, To]: Patcher[From, To] =
    macro PatcherMacros.derivePatcherWithDefaults[From, To]
}
