package io.scalaland.chimney.dsl

import io.scalaland.chimney.Codec
import io.scalaland.chimney.internal.compiletime.derivation.codec.CodecMacros
import io.scalaland.chimney.internal.compiletime.dsl.CodecDefinitionMacros
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

import scala.language.experimental.macros

/** Allows customization of [[io.scalaland.chimney.Codec]] derivation.
  *
  * @tparam Domain
  *   type of the domain value
  * @tparam Dto
  *   typeof the DTO value
  * @tparam EncodeOverrides
  *   type-level encoded config
  * @tparam DecodeOverrides
  *   type-level encoded config
  * @tparam Flags
  *   type-level encoded flags
  *
  * @since 1.2.0
  */
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

  /** Use `selectorDomain` field in `Domain` to obtain the value of `selectorDto` field in `Dto`.
    *
    * By default if `Domain` is missing field picked by `selectorDto` (or reverse) the compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-its-source-field]]
    *   for more details
    *
    * @tparam T
    *   type of domain field
    * @tparam U
    *   type of DTO field
    * @param selectorDomain
    *   source field in `Domain`, defined like `_.originalName`
    * @param selectorDto
    *   target field in `Dto`, defined like `_.newName`
    * @return
    *   [[io.scalaland.chimney.dsl.CodecDefinition]]
    *
    * @since 1.2.0
    */
  def withFieldRenamed[T, U](
      selectorDomain: Domain => T,
      selectorDto: Dto => U
  ): CodecDefinition[Domain, Dto, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags] =
    macro CodecDefinitionMacros.withFieldRenamedImpl[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags]

  /** Use `DomainSubtype` in `Domain` as a source for `DtoSubtype` in `Dto`.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#TODO]] for more details
    *
    * @tparam DomainSubtype
    *   type of sealed/enum instance
    * @tparam DtoSubtype
    *   type of sealed/enum instance
    * @return
    *   [[io.scalaland.chimney.dsl.CodecDefinition]]
    *
    * @since 1.2.0
    */
  def withSealedSubtypeRenamed[DomainSubtype, DtoSubtype]
      : CodecDefinition[Domain, Dto, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags] =
    macro CodecDefinitionMacros
      .withSealedSubtypeRenamedImpl[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags, DomainSubtype, DtoSubtype]

  /** Alias to [[withSealedSubtypeRenamed]].
    *
    * @since 1.2.0
    */
  def withEnumCaseRenamed[DomainSubtype, DtoSubtype]
      : CodecDefinition[Domain, Dto, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags] =
    macro CodecDefinitionMacros
      .withSealedSubtypeRenamedImpl[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags, DomainSubtype, DtoSubtype]

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
  def buildCodec[ImplicitScopeFlags <: TransformerFlags](implicit
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): Codec[Domain, Dto] =
    macro CodecMacros.deriveCodecWithConfig[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags, ImplicitScopeFlags]
}
