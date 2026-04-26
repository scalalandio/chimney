package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.compiletime.dsl.PartialTransformerDefinitionForAllMacros
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides, WithRuntimeDataStore}
import io.scalaland.chimney.partial

import scala.language.experimental.macros

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
  * @since 1.10.0
  */
final class PartialTransformerDefinitionForAll[
    From,
    To,
    Overrides <: TransformerOverrides,
    Flags <: TransformerFlags,
    FromMatch,
    ToMatch
](
    val runtimeData: TransformerDefinitionCommons.RuntimeDataStore
) extends WithRuntimeDataStore {

  /** Use the `selectorFrom` field in `FromMatch` to obtain the value of the `selectorTo` field in `ToMatch`.
    *
    * @since 1.10.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: FromMatch => T,
      selectorTo: ToMatch => U
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerDefinitionForAllMacros.withFieldRenamedImpl[From, To, Overrides, Flags, FromMatch, ToMatch]

  /** Use the `value` provided here for the field picked using `selector`.
    *
    * @since 1.10.0
    */
  def withFieldConst[T, U](
      selector: ToMatch => T,
      value: U
  )(implicit ev: U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerDefinitionForAllMacros.withFieldConstImpl[From, To, Overrides, Flags, FromMatch, ToMatch]

  /** Use the function `f` to compute the value of the field picked using `selector`.
    *
    * @since 1.10.0
    */
  def withFieldComputed[T, U](
      selector: ToMatch => T,
      f: FromMatch => U
  )(implicit ev: U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerDefinitionForAllMacros.withFieldComputedImpl[From, To, Overrides, Flags, FromMatch, ToMatch]

  /** Use the function `f` to compute the partial result for the field picked using `selector`.
    *
    * @since 1.10.0
    */
  def withFieldComputedPartial[T, U](
      selector: ToMatch => T,
      f: FromMatch => partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerDefinitionForAllMacros
      .withFieldComputedPartialImpl[From, To, Overrides, Flags, FromMatch, ToMatch]

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PartialTransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch](overrideData +: runtimeData)
      .asInstanceOf[this.type]
}
