package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.compiletime.dsl.PartialTransformerIntoForAllMacros
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides, WithRuntimeDataStore}
import io.scalaland.chimney.partial

import scala.language.experimental.macros

/** Scoped builder for defining overrides that apply to all matching `[FromMatch, ToMatch]` derivations, used with
  * `.intoPartial[To]` syntax.
  *
  * @since 1.10.0
  */
final class PartialTransformerIntoForAll[
    From,
    To,
    Overrides <: TransformerOverrides,
    Flags <: TransformerFlags,
    FromMatch,
    ToMatch
](
    val source: From,
    val td: PartialTransformerDefinition[From, To, Overrides, Flags]
) extends WithRuntimeDataStore {

  /** Use the `selectorFrom` field in `FromMatch` to obtain the value of the `selectorTo` field in `ToMatch`.
    *
    * @since 1.10.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: FromMatch => T,
      selectorTo: ToMatch => U
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoForAllMacros.withFieldRenamedImpl[From, To, Overrides, Flags, FromMatch, ToMatch]

  /** Use the `value` provided here for the field picked using `selector`.
    *
    * @since 1.10.0
    */
  def withFieldConst[T, U](
      selector: ToMatch => T,
      value: U
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoForAllMacros.withFieldConstImpl[From, To, Overrides, Flags, FromMatch, ToMatch]

  /** Use the function `f` to compute the value of the field picked using `selector`.
    *
    * @since 1.10.0
    */
  def withFieldComputed[T, U](
      selector: ToMatch => T,
      f: FromMatch => U
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoForAllMacros.withFieldComputedImpl[From, To, Overrides, Flags, FromMatch, ToMatch]

  /** Use the function `f` to compute the partial result for the field picked using `selector`.
    *
    * @since 1.10.0
    */
  def withFieldComputedPartial[T, U](
      selector: ToMatch => T,
      f: FromMatch => partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoForAllMacros
      .withFieldComputedPartialImpl[From, To, Overrides, Flags, FromMatch, ToMatch]

  val runtimeData: TransformerDefinitionCommons.RuntimeDataStore = td.runtimeData

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PartialTransformerIntoForAll(source, td.addOverride(overrideData)).asInstanceOf[this.type]
}
