package io.scalaland.chimney

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros

object auto extends LowPriorityAutoInstances

private[chimney] trait LowPriorityAutoInstances { this: auto.type =>

  implicit inline def deriveAutomaticTransformer[From, To]: Transformer[From, To] =
    ${ TransformerMacros.deriveTotalTransformerWithDefaults[From, To] }

  implicit inline def deriveAutomaticPartialTransformer[From, To]: PartialTransformer[From, To] =
    ${ TransformerMacros.derivePartialTransformerWithDefaults[From, To] }

  implicit inline def deriveAutomaticPatcher[From, To]: Patcher[From, To] =
    ${ PatcherMacros.derivePatcherWithDefaults[From, To] }
}
