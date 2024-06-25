package io.scalaland.chimney

import io.scalaland.chimney.dsl.CodecDefinition
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

final case class Codec[Domain, Dto](encode: Transformer[Domain, Dto], decode: PartialTransformer[Dto, Domain])
    extends Codec.AutoDerived[Domain, Dto]
object Codec extends CodecCompanionPlatform {

  def define[Domain, Dto]
      : CodecDefinition[Domain, Dto, TransformerOverrides.Empty, TransformerOverrides.Empty, TransformerFlags.Default] =
    new CodecDefinition(Transformer.define, PartialTransformer.define)

  trait AutoDerived[Domain, Dto] {
    val encode: Transformer[Domain, Dto]
    val decode: PartialTransformer[Dto, Domain]
  }
  object AutoDerived extends CodecAutoDerivedCompanionPlatform
}
