package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.compiletime.dsl.TransformerIntoForAllMacros
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides, WithRuntimeDataStore}

import scala.language.experimental.macros

/** Scoped builder for defining overrides that apply to all matching `[FromMatch, ToMatch]` derivations, used with
  * `.into[To]` syntax.
  *
  * @since 1.10.0
  */
final class TransformerIntoForAll[
    From,
    To,
    Overrides <: TransformerOverrides,
    Flags <: TransformerFlags,
    FromMatch,
    ToMatch
](
    val source: From,
    val td: TransformerDefinition[From, To, Overrides, Flags]
) extends WithRuntimeDataStore {

  /** Use the `selectorFrom` field in `FromMatch` to obtain the value of the `selectorTo` field in `ToMatch`.
    *
    * @since 1.10.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: FromMatch => T,
      selectorTo: ToMatch => U
  ): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoForAllMacros.withFieldRenamedImpl[From, To, Overrides, Flags, FromMatch, ToMatch]

  /** Use the `value` provided here for the field picked using `selector`.
    *
    * @since 1.10.0
    */
  def withFieldConst[T, U](
      selector: ToMatch => T,
      value: U
  )(implicit ev: U <:< T): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoForAllMacros.withFieldConstImpl[From, To, Overrides, Flags, FromMatch, ToMatch]

  /** Use the function `f` to compute the value of the field picked using `selector`.
    *
    * @since 1.10.0
    */
  def withFieldComputed[T, U](
      selector: ToMatch => T,
      f: FromMatch => U
  )(implicit ev: U <:< T): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoForAllMacros.withFieldComputedImpl[From, To, Overrides, Flags, FromMatch, ToMatch]

  val runtimeData: TransformerDefinitionCommons.RuntimeDataStore = td.runtimeData

  private[chimney] def addOverride(overrideData: Any): this.type =
    new TransformerIntoForAll(source, td.addOverride(overrideData)).asInstanceOf[this.type]
}
