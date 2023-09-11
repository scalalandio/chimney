package io.scalaland.chimney.dsl

import io.scalaland.chimney.partial
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.PartialTransformerIntoMacros
import io.scalaland.chimney.internal.runtime.{TransformerCfg, TransformerFlags, WithRuntimeDataStore}

import scala.language.experimental.macros

/** Provides DSL for configuring [[io.scalaland.chimney.PartialTransformer]]'s
  * generation and using the result to transform value at the same time
  *
  * @tparam From   type of input value
  * @tparam To     type of output value
  * @tparam Cfg    type-level encoded config
  * @tparam Flags  type-level encoded flags
  * @param  source object to transform
  * @param  td     transformer definition
  *
  * @since 0.7.0
  */
final class PartialTransformerInto[From, To, Cfg <: TransformerCfg, Flags <: TransformerFlags](
    val source: From,
    val td: PartialTransformerDefinition[From, To, Cfg, Flags]
) extends TransformerFlagsDsl[Lambda[
      `Flags1 <: TransformerFlags` => PartialTransformerInto[From, To, Cfg, Flags1]
    ], Flags]
    with WithRuntimeDataStore {

  /** Use provided `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of provided value
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldConst[T, U](selector: To => T, value: U)(implicit
      ev: U <:< T
  ): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerIntoMacros.withFieldConstImpl[From, To, Cfg, Flags]

  /** Use provided partial result `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of provided value
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldConstPartial[T, U](
      selector: To => T,
      value: partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerIntoMacros.withFieldConstPartialImpl[From, To, Cfg, Flags]

  /** Use function `f` to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldComputed[T, U](
      selector: To => T,
      f: From => U
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerIntoMacros.withFieldComputedImpl[From, To, Cfg, Flags]

  /** Use function `f` to compute partial result for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldComputedPartial[T, U](
      selector: To => T,
      f: From => partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerIntoMacros.withFieldComputedPartialImpl[From, To, Cfg, Flags]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#fields-renaming]] for more details
    *
    * @tparam T type of source field
    * @tparam U type of target field
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerIntoMacros.withFieldRenamedImpl[From, To, Cfg, Flags]

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    *
    * @tparam Inst type of coproduct instance
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withCoproductInstance[Inst](f: Inst => To): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerIntoMacros.withCoproductInstanceImpl[From, To, Cfg, Flags, Inst]

  /** Use `f` to calculate the (missing) coproduct instance partial result when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    *
    * @tparam Inst type of coproduct instance
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withCoproductInstancePartial[Inst](
      f: Inst => partial.Result[To]
  ): PartialTransformerInto[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerIntoMacros.withCoproductInstancePartialImpl[From, To, Cfg, Flags, Inst]

  /** Apply configured partial transformation in-place.
    *
    * It runs macro that tries to derive instance of `PartialTransformer[From, To]`
    * and immediately apply it to captured `source` value.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return partial transformation result of type `partial.Result[To]`
    *
    * @since 0.7.0
    */
  def transform[ImplicitScopeFlags <: TransformerFlags](implicit
      tc: io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]
  ): partial.Result[To] =
    macro TransformerMacros.derivePartialTransformationWithConfigNoFailFast[From, To, Cfg, Flags, ImplicitScopeFlags]

  /** Apply configured partial transformation in-place in a short-circuit (fail fast) mode.
    *
    * It runs macro that tries to derive instance of `PartialTransformer[From, To]`
    * and immediately apply it to captured `source` value.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return partial transformation result of type `partial.Result[To]`
    *
    * @since 0.7.0
    */
  def transformFailFast[ImplicitScopeFlags <: TransformerFlags](implicit
      tc: io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]
  ): partial.Result[To] =
    macro TransformerMacros.derivePartialTransformationWithConfigFailFast[From, To, Cfg, Flags, ImplicitScopeFlags]

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PartialTransformerInto(source, td.addOverride(overrideData)).asInstanceOf[this.type]
}
