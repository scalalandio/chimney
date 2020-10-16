package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerFlags

class TransformerConfiguration[Flags <: TransformerFlags]
    extends FlagsDsl[Lambda[`F1 <: TransformerFlags` => TransformerConfiguration[F1]], Flags]

object TransformerConfiguration {

  implicit val default: TransformerConfiguration[TransformerFlags.Default] =
    new TransformerConfiguration[TransformerFlags.Default]
}
