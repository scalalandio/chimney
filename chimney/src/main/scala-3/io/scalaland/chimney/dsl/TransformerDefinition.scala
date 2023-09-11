package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.compiletime.dsl.*
import io.scalaland.chimney.internal.runtime.{TransformerCfg, TransformerFlags, WithRuntimeDataStore}

import scala.quoted.*

/** Allows customization of [[io.scalaland.chimney.Transformer]] derivation.
  *
  * @tparam From  type of input value
  * @tparam To    type of output value
  * @tparam Cfg   type-level encoded config
  * @tparam Flags type-level encoded flags
  *
  * @since 0.4.0
  */
final class TransformerDefinition[From, To, Cfg <: TransformerCfg, Flags <: TransformerFlags](
    val runtimeData: TransformerDefinitionCommons.RuntimeDataStore
) extends TransformerFlagsDsl[[Flags1 <: TransformerFlags] =>> TransformerDefinition[From, To, Cfg, Flags1], Flags]
    with TransformerDefinitionCommons[[Cfg1 <: TransformerCfg] =>> TransformerDefinition[From, To, Cfg1, Flags]]
    with WithRuntimeDataStore {

  /** Lifts current transformer definition as `PartialTransformer` definition
    *
    * It keeps all the configuration, provided missing values, renames,
    * coproduct instances etc.
    *
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    */
  def partial: PartialTransformerDefinition[From, To, Cfg, Flags] =
    new PartialTransformerDefinition[From, To, Cfg, Flags](runtimeData)

  /** Use provided value `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @tparam T type of target field
    * @tparam U type of provided value
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 0.4.0
    */
  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      inline value: U
  )(using U <:< T): TransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ TransformerDefinitionMacros.withFieldConstImpl('this, 'selector, 'value) }

  /** Use function `f` to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 0.4.0
    */
  transparent inline def withFieldComputed[T, U](
      inline selector: To => T,
      inline f: From => U
  )(using U <:< T): TransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ TransformerDefinitionMacros.withFieldComputedImpl('this, 'selector, 'f) }

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @tparam T type of source field
    * @tparam U type of target field
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 0.4.0
    */
  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
  ): TransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ TransformerDefinitionMacros.withFieldRenamed('this, 'selectorFrom, 'selectorTo) }

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
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 0.4.0
    */
  transparent inline def withCoproductInstance[Inst](
      inline f: Inst => To
  ): TransformerDefinition[From, To, ? <: TransformerCfg, Flags] =
    ${ TransformerDefinitionMacros.withCoproductInstance('this, 'f) }

  /** Build Transformer using current configuration.
    *
    * It runs macro that tries to derive instance of `Transformer[From, To]`.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return [[io.scalaland.chimney.Transformer]] type class instance
    *
    * @since 0.4.0
    */
  inline def buildTransformer[ImplicitScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): Transformer[From, To] =
    ${ TransformerDefinitionMacros.buildTransformer[From, To, Cfg, Flags, ImplicitScopeFlags]('this) }

  private[chimney] def addOverride(overrideData: Any): this.type =
    new TransformerDefinition(overrideData +: runtimeData).asInstanceOf[this.type]
}
