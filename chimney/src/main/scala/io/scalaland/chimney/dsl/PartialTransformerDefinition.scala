package io.scalaland.chimney.dsl

import io.scalaland.chimney.{PartialTransformer, partial}
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.dsl.{PartialTransformerDefinitionWhiteboxMacros, TransformerBlackboxMacros}

import scala.language.experimental.macros

/** Allows customization of [[io.scalaland.chimney.PartialTransformer]] derivation.
  *
  * @tparam From  type of input value
  * @tparam To    type of output value
  * @tparam C     type-level encoded config
  * @tparam Flags type-level encoded flags
  *
  * @since 0.7.0
  */
final class PartialTransformerDefinition[From, To, C <: TransformerCfg, Flags <: TransformerFlags](
    val runtimeData: TransformerDefinitionCommons.RuntimeDataStore
) extends FlagsDsl[Lambda[`F1 <: TransformerFlags` => PartialTransformerDefinition[From, To, C, F1]], Flags]
    with TransformerDefinitionCommons[
      Lambda[`C1 <: TransformerCfg` => PartialTransformerDefinition[From, To, C1, Flags]]
    ] {

  /** Use provided `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of provided value
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    * @since 0.7.0
    */
  def withFieldConst[T, U](selector: To => T, value: U)(
      implicit ev: U <:< T
  ): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withFieldConstImpl[C]

  /** Use provided partial result `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    * @since 0.7.0
    */
  def withFieldConstPartial[T, U](selector: To => T, value: partial.Result[U])(
      implicit ev: U <:< T
  ): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withFieldConstPartialImpl[C]

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
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    * @since 0.7.0
    */
  def withFieldComputed[T, U](selector: To => T, f: From => U)(
      implicit ev: U <:< T
  ): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withFieldComputedImpl[C]

  /** Use function `f` to compute partial result for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    * @since 0.7.0
    */
  def withFieldComputedPartial[T, U](
      selector: To => T,
      f: From => partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withFieldComputedPartialImpl[C]

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
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    * @since 0.7.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withFieldRenamedImpl[C]

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    *
    * @tparam Inst type of coproduct instance
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  def withCoproductInstance[Inst](f: Inst => To): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withCoproductInstanceImpl[To, Inst, C]

  /** Use `f` to calculate the (missing) coproduct instance partial result when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    *
    * @tparam Inst type of coproduct instance
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  def withCoproductInstancePartial[Inst](
      f: Inst => partial.Result[To]
  ): PartialTransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionWhiteboxMacros.withCoproductInstancePartialImpl[To, Inst, C]

  /** Build Partial Transformer using current configuration.
    *
    * It runs macro that tries to derive instance of `PartialTransformer[From, To]`.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.7.0
    */
  def buildTransformer[ScopeFlags <: TransformerFlags](
      implicit tc: io.scalaland.chimney.dsl.TransformerConfiguration[ScopeFlags]
  ): PartialTransformer[From, To] =
    macro TransformerBlackboxMacros.buildPartialTransformerImpl[From, To, C, Flags, ScopeFlags]

  override protected def __updateRuntimeData(newRuntimeData: TransformerDefinitionCommons.RuntimeDataStore): this.type =
    new PartialTransformerDefinition(newRuntimeData).asInstanceOf[this.type]
}
