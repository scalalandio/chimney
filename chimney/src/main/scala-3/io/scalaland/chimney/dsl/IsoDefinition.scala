package io.scalaland.chimney.dsl

import io.scalaland.chimney.Iso
import io.scalaland.chimney.internal.compiletime.derivation.iso.IsoMacros
import io.scalaland.chimney.internal.compiletime.dsl.IsoDefinitionMacros
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

/** Allows customization of [[io.scalaland.chimney.Iso]] derivation.
  *
  * @tparam First
  *   input type of the first conversion, output type of the second conversion
  * @tparam Second
  *   output type of the first conversion, input type of the second conversion
  * @tparam FirstOverrides
  *   type-level encoded config
  * @tparam SecondOverrides
  *   type-level encoded config
  * @tparam Flags
  *   type-level encoded flags
  *
  * @since 1.2.0
  */
final class IsoDefinition[
    First,
    Second,
    FirstOverrides <: TransformerOverrides,
    SecondOverrides <: TransformerOverrides,
    Flags <: TransformerFlags
](
    val first: TransformerDefinition[First, Second, FirstOverrides, Flags],
    val second: TransformerDefinition[Second, First, SecondOverrides, Flags]
) extends TransformerFlagsDsl[
      [Flags1 <: TransformerFlags] =>> IsoDefinition[First, Second, FirstOverrides, SecondOverrides, Flags1],
      Flags
    ] {

  /** Use `selectorFirst` field in `First` to obtain the value of `selectorSecond` field in `Second`.
    *
    * By default if `First` is missing field picked by `selectorSecond` (or reverse) the compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-its-source-field]]
    *   for more details
    *
    * @tparam T
    *   type of source field
    * @tparam U
    *   type of target field
    * @param selectorFirst
    *   source field in `First`, defined like `_.originalName`
    * @param selectorSecond
    *   target field in `Second`, defined like `_.newName`
    * @return
    *   [[io.scalaland.chimney.dsl.IsoDefinition]]
    *
    * @since 1.2.0
    */
  transparent inline def withFieldRenamed[T, U](
      inline selectorFirst: First => T,
      inline selectorSecond: Second => U
  ): IsoDefinition[First, Second, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags] =
    ${ IsoDefinitionMacros.withFieldRenamedImpl('this, 'selectorFirst, 'selectorSecond) }

  /** Use `FirstSubtype` in `First` as a source for `SecondSubtype` in `Second`.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#handling-a-specific-sealed-subtype-by-a-specific-target-subtype]]
    *   for more details
    *
    * @tparam DomainSubtype
    *   type of sealed/enum instance
    * @tparam DtoSubtype
    *   type of sealed/enum instance
    * @return
    *   [[io.scalaland.chimney.dsl.IsoDefinition]]
    *
    * @since 1.2.0
    */
  transparent inline def withSealedSubtypeRenamed[DomainSubtype, DtoSubtype]
      : IsoDefinition[First, Second, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags] =
    ${
      IsoDefinitionMacros
        .withSealedSubtypeRenamedImpl[First, Second, FirstOverrides, SecondOverrides, Flags, DomainSubtype, DtoSubtype](
          'this
        )
    }

  /** Alias to [[withSealedSubtypeRenamed]].
    *
    * @since 1.2.0
    */
  transparent inline def withEnumCaseRenamed[DomainSubtype, DtoSubtype]
      : IsoDefinition[First, Second, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags] =
    ${
      IsoDefinitionMacros
        .withSealedSubtypeRenamedImpl[First, Second, FirstOverrides, SecondOverrides, Flags, DomainSubtype, DtoSubtype](
          'this
        )
    }

  /** Build Iso using current configuration.
    *
    * It runs macro that tries to derive instance of `Iso[First, Second]`. When transformation can't be derived, it
    * results with compilation error.
    *
    * @return
    *   [[io.scalaland.chimney.Iso]] type class instance
    *
    * @since 1.2.0
    */
  inline def buildIso[ImplicitScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): Iso[First, Second] =
    ${ IsoMacros.deriveIsoWithConfig[First, Second, FirstOverrides, SecondOverrides, Flags, ImplicitScopeFlags]('this) }
}
