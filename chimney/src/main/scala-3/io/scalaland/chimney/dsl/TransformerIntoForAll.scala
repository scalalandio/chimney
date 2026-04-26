package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.compiletime.dsl.TransformerIntoForAllMacros
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides, WithRuntimeDataStore}
import io.scalaland.chimney.partial

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
  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: FromMatch => T,
      inline selectorTo: ToMatch => U
  ): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    ${
      TransformerIntoForAllMacros.withFieldRenamedImpl[From, To, Overrides, Flags, FromMatch, ToMatch, T, U](
        'this,
        'selectorFrom,
        'selectorTo
      )
    }

  /** Use the `value` provided here for the field picked using `selector`.
    *
    * @since 1.10.0
    */
  transparent inline def withFieldConst[T, U](
      inline selector: ToMatch => T,
      inline value: U
  )(using U <:< T): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    ${
      TransformerIntoForAllMacros.withFieldConstImpl[From, To, Overrides, Flags, FromMatch, ToMatch, T, U](
        'this,
        'selector,
        'value
      )
    }

  /** Use the function `f` to compute the value of the field picked using `selector`.
    *
    * @since 1.10.0
    */
  transparent inline def withFieldComputed[T, U](
      inline selector: ToMatch => T,
      inline f: FromMatch => U
  )(using U <:< T): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    ${
      TransformerIntoForAllMacros.withFieldComputedImpl[From, To, Overrides, Flags, FromMatch, ToMatch, T, U](
        'this,
        'selector,
        'f
      )
    }

  val runtimeData: TransformerDefinitionCommons.RuntimeDataStore = td.runtimeData

  private[chimney] def addOverride(overrideData: Any): this.type =
    new TransformerIntoForAll(source, td.addOverride(overrideData)).asInstanceOf[this.type]
}
