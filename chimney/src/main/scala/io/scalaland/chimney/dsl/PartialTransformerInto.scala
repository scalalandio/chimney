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

  /** Use provided `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldConst[T, U](selector: To => T, value: U)(
      implicit ev: U <:< T
  ): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withFieldConstImpl

  /** Use provided partial result `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldConstPartial[T, U](
      selector: To => T,
      value: partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withFieldConstPartialImpl

  /** Use function `f` to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldComputed[T, U](
      selector: To => T,
      f: From => U
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withFieldComputedImpl

  /** Use function `f` to compute partial result for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    * @since 0.7.0
    */
  def withFieldComputedPartial[T, U](
      selector: To => T,
      f: From => partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withFieldComputedPartialImpl

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withFieldRenamedImpl

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withCoproductInstance[Inst](f: Inst => To): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withCoproductInstanceImpl

  /** Use `f` to calculate the (missing) coproduct instance partial result when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withCoproductInstancePartial[Inst](
      f: Inst => partial.Result[To]
  ): PartialTransformerInto[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerIntoWhiteboxMacros.withCoproductInstancePartialImpl

  /** Apply configured partial transformation in-place.
    *
    * It runs macro that tries to derive instance of `PartialTransformer[From, To]`
    * and immediately apply it to captured `source` value.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return partial transformation result of type `partial.Result[To]`
    * @since 0.7.0
    */
  def transform[ScopeFlags <: TransformerFlags](
      implicit tc: io.scalaland.chimney.dsl.TransformerConfiguration[ScopeFlags]
  ): partial.Result[To] =
    macro TransformerBlackboxMacros.partialTransformNoFailFastImpl[From, To, C, Flags, ScopeFlags]

  /** Apply configured partial transformation in-place in a short-circuit (fail fast) mode.
    *
    * It runs macro that tries to derive instance of `PartialTransformer[From, To]`
    * and immediately apply it to captured `source` value.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return partial transformation result of type `partial.Result[To]`
    * @since 0.7.0
    */
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
