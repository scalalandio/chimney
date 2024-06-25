package io.scalaland.chimney.dsl

import io.scalaland.chimney.Iso
import io.scalaland.chimney.internal.compiletime.derivation.iso.IsoMacros
import io.scalaland.chimney.internal.compiletime.dsl.IsoDefinitionMacros
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

import scala.language.experimental.macros

final class IsoDefinition[
    From,
    To,
    FromOverrides <: TransformerOverrides,
    ToOverrides <: TransformerOverrides,
    Flags <: TransformerFlags
](
    val from: TransformerDefinition[From, To, FromOverrides, Flags],
    val to: TransformerDefinition[To, From, ToOverrides, Flags]
) extends TransformerFlagsDsl[Lambda[
      `Flags1 <: TransformerFlags` => CodecDefinition[From, To, FromOverrides, ToOverrides, Flags1]
    ], Flags] {

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` (or reverse) the compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-its-source-field]]
    *   for more details
    *
    * @tparam T
    *   type of source field
    * @tparam U
    *   type of target field
    * @param selectorFrom
    *   source field in `From`, defined like `_.originalName`
    * @param selectorTo
    *   target field in `To`, defined like `_.newName`
    * @return
    *   [[io.scalaland.chimney.dsl.IsoDefinition]]
    *
    * @since 1.2.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): IsoDefinition[From, To, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags] =
    macro IsoDefinitionMacros.withFieldRenamedImpl[From, To, FromOverrides, ToOverrides, Flags]

  /** Build Iso using current configuration.
    *
    * It runs macro that tries to derive instance of `Iso[From, To]`. When transformation can't be derived, it results
    * with compilation error.
    *
    * @return
    *   [[io.scalaland.chimney.Iso]] type class instance
    *
    * @since 1.2.0
    */
  def buildIso[ImplicitScopeFlags <: TransformerFlags](implicit
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): Iso[From, To] =
    macro IsoMacros.deriveIsoWithConfig[From, To, FromOverrides, ToOverrides, Flags, ImplicitScopeFlags]
}
