package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.TransformerIntoMacros
import io.scalaland.chimney.internal.runtime.{IsFunction, TransformerFlags, TransformerOverrides, WithRuntimeDataStore}

import scala.language.experimental.macros

/** Provides DSL for configuring [[io.scalaland.chimney.Transformer]]'s generation and using the result to transform
  * value at the same time
  *
  * @tparam From      type of input value
  * @tparam To        type of output value
  * @tparam Overrides type-level encoded config
  * @tparam Flags     type-level encoded flags
  * @param  source object to transform
  * @param  td     transformer definition
  *
  * @since 0.1.0
  */
final class TransformerInto[From, To, Overrides <: TransformerOverrides, Flags <: TransformerFlags](
    val source: From,
    val td: TransformerDefinition[From, To, Overrides, Flags]
) extends TransformerFlagsDsl[Lambda[
      `Flags1 <: TransformerFlags` => TransformerInto[From, To, Overrides, Flags1]
    ], Flags]
    with WithRuntimeDataStore {

  /** Lifts current transformation as partial transformation.
    *
    * It keeps all the configuration, provided missing values, renames, coproduct instances etc.
    *
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    */
  def partial: PartialTransformerInto[From, To, Overrides, Flags] =
    new PartialTransformerInto[From, To, Overrides, Flags](source, td.partial)

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-a-provided-value]] for more details
    *
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  def withFieldConst[T, U](selector: To => T, value: U)(implicit
      ev: U <:< T
  ): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withFieldConstImpl[From, To, Overrides, Flags]

  /** Use function `f` to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-computed-value]] for more details
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
  )(implicit ev: U <:< T): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withFieldComputedImpl[From, To, Overrides, Flags]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-its-source-field]] for more details
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
  ): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withFieldRenamedImpl[From, To, Overrides, Flags]

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts will have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it will fail.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#handling-a-specific-sealed-subtype-with-a-computed-value]] for more details
    *
    * @tparam Subtypetype of coproduct instance@param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.2
    */
  def withCoproductInstance[Subtype](f: Subtype => To): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withCoproductInstanceImpl[From, To, Overrides, Flags, Subtype]

  /** Use `f` instead of the primary constructor to construct the `To` value.
    *
    * Macro will read the names of Eta-expanded method's/lambda's parameters and try to match them with `From` getters.
    *
    * Values for each parameter can be provided the same way as if they were normal constructor's arguments.
    *
    * @see [[https://chimney.readthedocs.io/supported-transformations/#types-with-manually-provided-constructors]] for more details
    *
    * @tparam Ctor type of the Eta-expanded method/lambda which should return `To`
    * @param f method name or lambda which constructs `To`
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.8.4
    */
  def withConstructor[Ctor](
      f: Ctor
  )(implicit ev: IsFunction.Of[Ctor, To]): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withConstructorImpl[From, To, Overrides, Flags]

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
    macro TransformerMacros.deriveTotalTransformationWithConfig[From, To, Overrides, Flags, ImplicitScopeFlags]

  private[chimney] def addOverride(overrideData: Any): this.type =
    new TransformerInto(source, td.addOverride(overrideData)).asInstanceOf[this.type]
}
