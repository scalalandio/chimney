package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.TransformerIntoMacros
import io.scalaland.chimney.internal.runtime.{TransformerCfg, TransformerFlags, WithRuntimeDataStore}

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
) extends TransformerFlagsDsl[[Flags1 <: TransformerFlags] =>> TransformerInto[From, To, Cfg, Flags1], Flags]
    with WithRuntimeDataStore {

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
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      inline value: U
  )(using U <:< T): TransformerInto[From, To, ? <: TransformerCfg, Flags] =
    ${ TransformerIntoMacros.withFieldConstImpl('this, 'selector, 'value) }

  /** Use function `f` to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @tparam T type of target field
    * @tparam U type of computed value
    * @param selector target field in `To`, defined like `_.name`
    * @param f        function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  transparent inline def withFieldComputed[T, U](
      inline selector: To => T,
      inline f: From => U
  )(using U <:< T): TransformerInto[From, To, ? <: TransformerCfg, Flags] =
    ${ TransformerIntoMacros.withFieldComputedImpl('this, 'selector, 'f) }

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @tparam T type of source field
    * @tparam U type of target field
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
  ): TransformerInto[From, To, ? <: TransformerCfg, Flags] =
    ${ TransformerIntoMacros.withFieldRenamedImpl('this, 'selectorFrom, 'selectorTo) }

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts will have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it will fail.
    *
    * @see [[https://chimney.readthedocs.io/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    * @tparam Inst type of coproduct instance@param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.2
    */
  transparent inline def withCoproductInstance[Inst](
      inline f: Inst => To
  ): TransformerInto[From, To, ? <: TransformerCfg, Flags] =
    ${ TransformerIntoMacros.withCoproductInstanceImpl('this, 'f) }

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
  inline def transform[ImplicitScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): To =
    ${ TransformerIntoMacros.transform[From, To, Cfg, Flags, ImplicitScopeFlags]('source, 'td) }

  private[chimney] def addOverride(overrideData: Any): this.type =
    new TransformerInto(source, td.addOverride(overrideData)).asInstanceOf[this.type]
}
