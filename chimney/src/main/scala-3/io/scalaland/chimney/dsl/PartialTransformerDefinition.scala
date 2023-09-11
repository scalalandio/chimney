package io.scalaland.chimney.dsl

import io.scalaland.chimney.{partial, PartialTransformer}
import io.scalaland.chimney.internal.compiletime.dsl.*
import io.scalaland.chimney.internal.runtime.{TransformerCfg, TransformerFlags, WithRuntimeDataStore}

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
) extends TransformerFlagsDsl[
      [Flags1 <: TransformerFlags] =>> PartialTransformerDefinition[From, To, Cfg, Flags1],
      Flags
    ]
    with TransformerDefinitionCommons[[Cfg1 <: TransformerCfg] =>> PartialTransformerDefinition[From, To, Cfg1, Flags]]
    with WithRuntimeDataStore {

  /** Use provided `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @tparam T type of target field
    * @tparam U type of provided value
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      inline value: U
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldConstImpl('this, 'selector, 'value) }

  /** Use provided partial result `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  transparent inline def withFieldConstPartial[T, U](
      inline selector: To => T,
      inline value: partial.Result[U]
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldConstPartialImpl('this, 'selector, 'value) }

  /** Use function `f` to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  transparent inline def withFieldComputed[T, U](
      inline selector: To => T,
      inline f: From => U
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldComputedImpl('this, 'selector, 'f) }

  /** Use function `f` to compute partial result for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  transparent inline def withFieldComputedPartial[T, U](
      inline selector: To => T,
      inline f: From => partial.Result[U]
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldComputedPartialImpl('this, 'selector, 'f) }

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @tparam T type of source field
    * @tparam U type of target field
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldRenamed('this, 'selectorFrom, 'selectorTo) }

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    * @tparam Inst type of coproduct instance
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  transparent inline def withCoproductInstance[Inst](
      inline f: Inst => To
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withCoproductInstance('this, 'f) }

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
  transparent inline def withCoproductInstancePartial[Inst](
      inline f: Inst => partial.Result[To]
  ): PartialTransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ PartialTransformerDefinitionMacros.withCoproductInstancePartial('this, 'f) }

  /** Build Partial Transformer using current configuration.
    *
    * It runs macro that tries to derive instance of `PartialTransformer[From, To]`.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.7.0
    */
  inline def buildTransformer[ImplicitScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): PartialTransformer[From, To] =
    ${ PartialTransformerDefinitionMacros.buildTransformer[From, To, Cfg, Flags, ImplicitScopeFlags]('this) }

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PartialTransformerDefinition(overrideData +: runtimeData).asInstanceOf[this.type]
}
