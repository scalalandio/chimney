package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.compiletime.dsl.PartialTransformerIntoForAllMacros
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides, WithRuntimeDataStore}
import io.scalaland.chimney.partial

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
  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: FromMatch => T,
      inline selectorTo: ToMatch => U
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    ${
      PartialTransformerIntoForAllMacros.withFieldRenamedImpl[From, To, Overrides, Flags, FromMatch, ToMatch, T, U](
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
  )(using U <:< T): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    ${
      PartialTransformerIntoForAllMacros.withFieldConstImpl[From, To, Overrides, Flags, FromMatch, ToMatch, T, U](
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
  )(using U <:< T): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    ${
      PartialTransformerIntoForAllMacros.withFieldComputedImpl[From, To, Overrides, Flags, FromMatch, ToMatch, T, U](
        'this,
        'selector,
        'f
      )
    }

  /** Use the function `f` to compute the partial result for the field picked using `selector`.
    *
    * @since 1.10.0
    */
  transparent inline def withFieldComputedPartial[T, U](
      inline selector: ToMatch => T,
      inline f: FromMatch => partial.Result[U]
  )(using U <:< T): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    ${
      PartialTransformerIntoForAllMacros
        .withFieldComputedPartialImpl[From, To, Overrides, Flags, FromMatch, ToMatch, T, U]('this, 'selector, 'f)
    }

  val runtimeData: TransformerDefinitionCommons.RuntimeDataStore = td.runtimeData

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PartialTransformerIntoForAll(source, td.addOverride(overrideData)).asInstanceOf[this.type]
}
