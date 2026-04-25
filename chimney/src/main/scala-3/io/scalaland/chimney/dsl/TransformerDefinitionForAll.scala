package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.compiletime.dsl.TransformerDefinitionForAllMacros
import io.scalaland.chimney.internal.runtime.{IsFunction, Path, TransformerFlags, TransformerOverrides, WithRuntimeDataStore}
import io.scalaland.chimney.partial

/** Scoped builder for defining overrides that apply to all matching `[FromMatch, ToMatch]` derivations.
  *
  * @tparam From
  *   type of top-level input value
  * @tparam To
  *   type of top-level output value
  * @tparam Overrides
  *   type-level encoded config
  * @tparam Flags
  *   type-level encoded flags
  * @tparam FromMatch
  *   source type to match in recursive derivations
  * @tparam ToMatch
  *   target type to match in recursive derivations
  *
  * @since 1.7.0
  */
final class TransformerDefinitionForAll[From, To, Overrides <: TransformerOverrides, Flags <: TransformerFlags, FromMatch, ToMatch](
    val runtimeData: TransformerDefinitionCommons.RuntimeDataStore
) extends WithRuntimeDataStore {

  /** Use the `selectorFrom` field in `FromMatch` to obtain the value of the `selectorTo` field in `ToMatch`.
    *
    * @since 1.7.0
    */
  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: FromMatch => T,
      inline selectorTo: ToMatch => U
  ): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ TransformerDefinitionForAllMacros.withFieldRenamedImpl[From, To, Overrides, Flags, FromMatch, ToMatch, T, U]('this, 'selectorFrom, 'selectorTo) }

  /** Use the `value` provided here for the field picked using `selector`.
    *
    * @since 1.7.0
    */
  transparent inline def withFieldConst[T, U](
      inline selector: ToMatch => T,
      inline value: U
  )(using U <:< T): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ TransformerDefinitionForAllMacros.withFieldConstImpl[From, To, Overrides, Flags, FromMatch, ToMatch, T, U]('this, 'selector, 'value) }

  /** Use the function `f` to compute the value of the field picked using `selector`.
    *
    * @since 1.7.0
    */
  transparent inline def withFieldComputed[T, U](
      inline selector: ToMatch => T,
      inline f: FromMatch => U
  )(using U <:< T): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ TransformerDefinitionForAllMacros.withFieldComputedImpl[From, To, Overrides, Flags, FromMatch, ToMatch, T, U]('this, 'selector, 'f) }

  /** Use the function `f` to compute the partial result for the field picked using `selector`.
    *
    * @since 1.7.0
    */
  transparent inline def withFieldComputedPartial[T, U](
      inline selector: ToMatch => T,
      inline f: FromMatch => partial.Result[U]
  )(using U <:< T): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ TransformerDefinitionForAllMacros.withFieldComputedPartialImpl[From, To, Overrides, Flags, FromMatch, ToMatch, T, U]('this, 'selector, 'f) }

  private[chimney] def addOverride(overrideData: Any): this.type =
    new TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch](overrideData +: runtimeData)
      .asInstanceOf[this.type]
}
