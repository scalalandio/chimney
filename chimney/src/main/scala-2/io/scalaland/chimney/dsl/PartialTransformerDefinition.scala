package io.scalaland.chimney.dsl

import io.scalaland.chimney.{partial, PartialTransformer}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.PartialTransformerDefinitionMacros
import io.scalaland.chimney.internal.runtime.{TransformerCfg, TransformerFlags, WithRuntimeDataStore}

import scala.language.experimental.macros

/** Allows customization of [[io.scalaland.chimney.PartialTransformer]] derivation.
  *
  * @tparam From  type of input value
  * @tparam To    type of output value
  * @tparam Cfg   type-level encoded config
  * @tparam Flags type-level encoded flags
  *
  * @since 0.7.0
  */
final class PartialTransformerDefinition[From, To, Cfg <: TransformerCfg, Flags <: TransformerFlags](
    val runtimeData: TransformerDefinitionCommons.RuntimeDataStore
) extends TransformerFlagsDsl[Lambda[
      `Flags1 <: TransformerFlags` => PartialTransformerDefinition[From, To, Cfg, Flags1]
    ], Flags]
    with TransformerDefinitionCommons[
      Lambda[`Cfg1 <: TransformerCfg` => PartialTransformerDefinition[From, To, Cfg1, Flags]]
    ]
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
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  def withFieldConst[T, U](selector: To => T, value: U)(implicit
      ev: U <:< T
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionMacros.withFieldConstImpl[From, To, Cfg, Flags]

  /** Use provided partial result `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    *
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  def withFieldConstPartial[T, U](selector: To => T, value: partial.Result[U])(implicit
      ev: U <:< T
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionMacros.withFieldConstPartialImpl[From, To, Cfg, Flags]

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
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  def withFieldComputed[T, U](selector: To => T, f: From => U)(implicit
      ev: U <:< T
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionMacros.withFieldComputedImpl[From, To, Cfg, Flags]

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
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  def withFieldComputedPartial[T, U](
      selector: To => T,
      f: From => partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionMacros.withFieldComputedPartialImpl[From, To, Cfg, Flags]

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
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionMacros.withFieldRenamedImpl[From, To, Cfg, Flags]

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
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  def withCoproductInstance[Inst](f: Inst => To): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionMacros.withCoproductInstanceImpl[From, To, Cfg, Flags, Inst]

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
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  def withCoproductInstancePartial[Inst](
      f: Inst => partial.Result[To]
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    macro PartialTransformerDefinitionMacros.withCoproductInstancePartialImpl[From, To, Cfg, Flags, Inst]

  /** Build Partial Transformer using current configuration.
    *
    * It runs macro that tries to derive instance of `PartialTransformer[From, To]`.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.7.0
    */
  def buildTransformer[ImplicitScopeFlags <: TransformerFlags](implicit
      tc: io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]
  ): PartialTransformer[From, To] =
    macro TransformerMacros.derivePartialTransformerWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags]

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PartialTransformerDefinition(overrideData +: runtimeData).asInstanceOf[this.type]
}
