package io.scalaland.chimney.dsl

import io.scalaland.chimney.Codec
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

final class CodecDefinition[
    Domain,
    Dto,
    EncodeOverrides <: TransformerOverrides,
    DecodeOverrides <: TransformerOverrides,
    Flags <: TransformerFlags
](
    val encode: TransformerDefinition[Domain, Dto, EncodeOverrides, Flags],
    val decode: PartialTransformerDefinition[Dto, Domain, DecodeOverrides, Flags]
) extends TransformerFlagsDsl[Lambda[
      `Flags1 <: TransformerFlags` => CodecDefinition[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags1]
    ], Flags] {

  // TODO: def withFieldRenamed

  def buildCodec[ImplicitScopeFlags <: TransformerFlags](implicit
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): Codec[Domain, Dto] = ???
  // macro TransformerMacros.deriveTotalTransformerWithConfig[From, To, Overrides, Flags, ImplicitScopeFlags]
}
