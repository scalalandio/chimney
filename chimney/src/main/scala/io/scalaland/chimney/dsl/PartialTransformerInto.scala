package io.scalaland.chimney.dsl

import io.scalaland.chimney.partial
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.dsl.{PartialTransformerIntoWhiteboxMacros, TransformerBlackboxMacros}

import scala.language.experimental.macros

/** Provides DSL for configuring [[io.scalaland.chimney.PartialTransformer]]'s
  * generation and using the result to transform value at the same time
  *
  * @param  source object to transform
  * @param  td     transformer definition
  * @tparam From   type of input value
  * @tparam To     type of output value
  * @tparam C      type-level encoded config
  */
final class PartialTransformerInto[From, To, C <: TransformerCfg, Flags <: TransformerFlags](
    val source: From,
    val td: PartialTransformerDefinition[From, To, C, Flags]
) extends FlagsDsl[Lambda[`F1 <: TransformerFlags` => PartialTransformerInto[From, To, C, F1]], Flags] {

  def withFieldConst[T, U](selector: To => T, value: U)(
      implicit ev: U <:< T
  ): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withFieldConstImpl

  def withFieldConstPartial[T, U](
      selector: To => T,
      value: partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withFieldConstPartialImpl

  def withFieldComputed[T, U](
      selector: To => T,
      f: From => U
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withFieldComputedImpl

  def withFieldComputedPartial[T, U](
      selector: To => T,
      f: From => partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withFieldComputedPartialImpl

  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withFieldRenamedImpl

  def withCoproductInstance[Inst](f: Inst => To): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withCoproductInstanceImpl

  def withCoproductInstancePartial[Inst](
      f: Inst => partial.Result[To]
  ): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withCoproductInstancePartialImpl

  def transform[ScopeFlags <: TransformerFlags](
      implicit tc: io.scalaland.chimney.dsl.TransformerConfiguration[ScopeFlags]
  ): partial.Result[To] =
    macro TransformerBlackboxMacros.partialTransformNoFailFastImpl[From, To, C, Flags, ScopeFlags]

  def transformFailFast[ScopeFlags <: TransformerFlags](
      implicit tc: io.scalaland.chimney.dsl.TransformerConfiguration[ScopeFlags]
  ): partial.Result[To] =
    macro TransformerBlackboxMacros.partialTransformFailFastImpl[From, To, C, Flags, ScopeFlags]

  /** Used internally by macro. Please don't use in your code.
    */
  def __refineTransformerDefinition[C1 <: TransformerCfg](
      f: PartialTransformerDefinition[From, To, C, Flags] => PartialTransformerDefinition[From, To, C1, Flags]
  ): PartialTransformerInto[From, To, C1, Flags] =
    new PartialTransformerInto[From, To, C1, Flags](source, f(td))

}
