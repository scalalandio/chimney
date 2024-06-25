package io.scalaland.chimney.dsl

import io.scalaland.chimney.Codec
import io.scalaland.chimney.internal.compiletime.derivation.codec.CodecMacros
import io.scalaland.chimney.internal.compiletime.dsl.CodecDefinitionMacros
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
) extends TransformerFlagsDsl[
      [Flags1 <: TransformerFlags] =>> CodecDefinition[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags1],
      Flags
    ] {

  transparent inline def withFieldRenamed[T, U](
      inline selectorDomain: Domain => T,
      inline selectorDto: Dto => U
  ): CodecDefinition[Domain, Dto, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags] =
    ${ CodecDefinitionMacros.withFieldRenamedImpl('this, 'selectorDomain, 'selectorDto) }

  /** Build Codec using current configuration.
    *
    * It runs macro that tries to derive instance of `Codec[Domain, Dto]`. When transformation can't be derived, it
    * results with compilation error.
    *
    * @return
    *   [[io.scalaland.chimney.Codec]] type class instance
    *
    * @since 1.2.0
    */
  inline def buildCodec[ImplicitScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): Codec[Domain, Dto] =
    ${
      CodecMacros.deriveCodecWithConfig[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags, ImplicitScopeFlags]('this)
    }
}
