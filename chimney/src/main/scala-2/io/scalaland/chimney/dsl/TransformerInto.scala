package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.macros.dsl.{TransformerBlackboxMacros, TransformerIntoWhiteboxMacros}

import scala.language.experimental.macros

/** Provides DSL for configuring [[io.scalaland.chimney.Transformer]]'s
  * generation and using the result to transform value at the same time
  *
  * @tparam From   type of input value
  * @tparam To     type of output value
  * @tparam Cfg    type-level encoded config
  * @tparam Flags  type-level encoded flags
  * @param  source object to transform
  * @param  td     transformer definition
  *
  * @since 0.1.0
  */
final class TransformerInto[From, To, Cfg <: TransformerCfg, Flags <: TransformerFlags](
    val source: From,
    val td: TransformerDefinition[From, To, Cfg, Flags]
) extends FlagsDsl[Lambda[`Flags1 <: TransformerFlags` => TransformerInto[From, To, Cfg, Flags1]], Flags] {

  /** Lifts current transformation as partial transformation.
    *
    * It keeps all the configuration, provided missing values, renames,
    * coproduct instances etc.
    *
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    */
  def partial: PartialTransformerInto[From, To, Cfg, Flags] =
    new PartialTransformerInto[From, To, Cfg, Flags](source, td.partial)

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  def withFieldConst[T, U](selector: To => T, value: U)(implicit
      ev: U <:< T
  ): TransformerInto[From, To, ? <: TransformerCfg, Flags] =
    macro TransformerIntoWhiteboxMacros.withFieldConstImpl

  /** Use function `f` to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  def withFieldComputed[T, U](
      selector: To => T,
      f: From => U
  )(implicit ev: U <:< T): TransformerInto[From, To, ? <: TransformerCfg, Flags] =
    macro TransformerIntoWhiteboxMacros.withFieldComputedImpl

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
    *
    * @tparam T type of source field
    * @tparam U type of target field
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): TransformerInto[From, To, ? <: TransformerCfg, Flags] =
    macro TransformerIntoWhiteboxMacros.withFieldRenamedImpl

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts will have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it will fail.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    *
    * @tparam Inst type of coproduct instance@param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.2
    */
  def withCoproductInstance[Inst](f: Inst => To): TransformerInto[From, To, ? <: TransformerCfg, Flags] =
    macro TransformerIntoWhiteboxMacros.withCoproductInstanceImpl

  /** Apply configured transformation in-place.
    *
    * It runs macro that tries to derive instance of `Transformer[From, To]`
    * and immediately apply it to captured `source` value.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return transformed value of type `To`
    *
    * @since 0.1.0
    */
  def transform[ImplicitScopeFlags <: TransformerFlags](implicit
      tc: io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]
  ): To =
    macro TransformerBlackboxMacros.transformImpl[From, To, Cfg, Flags, ImplicitScopeFlags]

  /** Used internally by macro. Please don't use in your code.
    */
  def __refineTransformerDefinition[Cfg1 <: TransformerCfg](
      f: TransformerDefinition[From, To, Cfg, Flags] => TransformerDefinition[From, To, Cfg1, Flags]
  ): TransformerInto[From, To, Cfg1, Flags] =
    new TransformerInto[From, To, Cfg1, Flags](source, f(td))
}
