package io.scalaland.chimney.dsl

import io.scalaland.chimney.partial
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.PartialTransformerIntoMacros
import io.scalaland.chimney.internal.runtime.{IsFunction, TransformerFlags, TransformerOverrides, WithRuntimeDataStore}

import scala.language.experimental.macros

/** Provides DSL for configuring [[io.scalaland.chimney.PartialTransformer]]'s generation and using the result to
  * transform value at the same time
  *
  * @tparam From
  *   type of input value
  * @tparam To
  *   type of output value
  * @tparam Overrides
  *   type-level encoded config
  * @tparam Flags
  *   type-level encoded flags
  * @param source
  *   object to transform
  * @param td
  *   transformer definition
  *
  * @since 0.7.0
  */
final class PartialTransformerInto[From, To, Overrides <: TransformerOverrides, Flags <: TransformerFlags](
    val source: From,
    val td: PartialTransformerDefinition[From, To, Overrides, Flags]
) extends TransformerFlagsDsl[Lambda[
      `Flags1 <: TransformerFlags` => PartialTransformerInto[From, To, Overrides, Flags1]
    ], Flags]
    with WithRuntimeDataStore {

  /** Use provided `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-a-provided-value]]
    *   for more details
    *
    * @tparam T
    *   type of target field
    * @tparam U
    *   type of provided value
    * @param selector
    *   target field in `To`, defined like `_.name`
    * @param value
    *   constant value to use for the target field
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldConst[T, U](selector: To => T, value: U)(implicit
      ev: U <:< T
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withFieldConstImpl[From, To, Overrides, Flags]

  /** Use provided partial result `value` for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-a-provided-value]]
    *   for more details
    *
    * @tparam T
    *   type of target field
    * @tparam U
    *   type of provided value
    * @param selector
    *   target field in `To`, defined like `_.name`
    * @param value
    *   constant value to use for the target field
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldConstPartial[T, U](
      selector: To => T,
      value: partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withFieldConstPartialImpl[From, To, Overrides, Flags]

  /** Use function `f` to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-computed-value]]
    *   for more details
    *
    * @tparam T
    *   type of target field
    * @tparam U
    *   type of computed value
    * @param selector
    *   target field in `To`, defined like `_.name`
    * @param f
    *   function used to compute value of the target field
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldComputed[T, U](
      selector: To => T,
      f: From => U
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withFieldComputedImpl[From, To, Overrides, Flags]

  /** Use function `f` to compute partial result for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-computed-value]]
    *   for more details
    *
    * @tparam T
    *   type of target field
    * @tparam U
    *   type of computed value
    * @param selector
    *   target field in `To`, defined like `_.name`
    * @param f
    *   function used to compute value of the target field
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldComputedPartial[T, U](
      selector: To => T,
      f: From => partial.Result[U]
  )(implicit ev: U <:< T): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withFieldComputedPartialImpl[From, To, Overrides, Flags]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-its-source-field]]
    *   for more details
    *
    * @tparam T
    *   type of source field
    * @tparam U
    *   type of target field
    * @param selectorFrom
    *   source field in `From`, defined like `_.originalName`
    * @param selectorTo
    *   target field in `To`, defined like `_.newName`
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withFieldRenamedImpl[From, To, Overrides, Flags]

  /** Use `f` to calculate the unmatched subtype when mapping one sealed/enum into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation expects that coproducts to
    * have matching names of its components, and for every component in `To` field's type there is matching component in
    * `From` type. If some component is missing it fails compilation unless provided replacement with this operation.
    *
    * For convenience/readability [[withEnumCaseHandled]] alias can be used (e.g. for Scala 3 enums or Java enums).
    *
    * It differs from `withFieldComputed(_.matching[Subtype], src => ...)`, since `withSealedSubtypeHandled` matches on
    * `From` subtype, while `.matching[Subtype]` matches on `To` value's piece.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#handling-a-specific-sealed-subtype-with-a-computed-value]]
    *   for more details
    *
    * @tparam Subtype
    *   type of sealed/enum instance
    * @param f
    *   function to calculate values of components that cannot be mapped automatically
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 1.0.0
    */
  def withSealedSubtypeHandled[Subtype](
      f: Subtype => To
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withSealedSubtypeHandledImpl[From, To, Overrides, Flags, Subtype]

  /** Alias to [[withSealedSubtypeHandled]].
    *
    * @since 1.0.0
    */
  def withEnumCaseHandled[Subtype](
      f: Subtype => To
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withSealedSubtypeHandledImpl[From, To, Overrides, Flags, Subtype]

  /** Renamed to [[withSealedSubtypeHandled]].
    *
    * @since 0.7.0
    */
  @deprecated("Use .withSealedSubtypeHandled or .withEnumCaseHandled for more clarity", "1.0.0")
  def withCoproductInstance[Subtype](
      f: Subtype => To
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withSealedSubtypeHandledImpl[From, To, Overrides, Flags, Subtype]

  /** Use `f` to calculate the unmatched subtype's partial.Result when mapping one sealed/enum into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation expects that coproducts to
    * have matching names of its components, and for every component in `To` field's type there is matching component in
    * `From` type. If some component is missing it fails compilation unless provided replacement with this operation.
    *
    * For convenience/readability [[withEnumCaseHandledPartial]] alias can be used (e.g. for Scala 3 enums or Java
    * enums).
    *
    * It differs from `withFieldComputedPartial(_.matching[Subtype], src => ...)`, since `withSealedSubtypeHandled`
    * matches on `From` subtype, while `.matching[Subtype]` matches on `To` value's piece.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#handling-a-specific-sealed-subtype-with-a-computed-value]]
    *   for more details
    *
    * @tparam Subtype
    *   type of sealed/enum instance
    * @param f
    *   function to calculate values of components that cannot be mapped automatically
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 1.0.0
    */
  def withSealedSubtypeHandledPartial[Subtype](
      f: Subtype => partial.Result[To]
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withSealedSubtypeHandledPartialImpl[From, To, Overrides, Flags, Subtype]

  /** Alias to [[withSealedSubtypeHandledPartial]].
    *
    * @since 1.0.0
    */
  def withEnumCaseHandledPartial[Subtype](
      f: Subtype => partial.Result[To]
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withSealedSubtypeHandledPartialImpl[From, To, Overrides, Flags, Subtype]

  /** Renamed to [[withSealedSubtypeHandledPartial]].
    *
    * @since 0.7.0
    */
  @deprecated("Use .withSealedSubtypeHandledPartial or .withEnumCaseHandledPartial for more clarity", "1.0.0")
  def withCoproductInstancePartial[Subtype](
      f: Subtype => partial.Result[To]
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withSealedSubtypeHandledPartialImpl[From, To, Overrides, Flags, Subtype]

  /** Use `FromSubtype` in `From` as a source for `ToSubtype` in `To`.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#handling-a-specific-sealed-subtype-by-a-specific-target-subtype]]
    *   for more details
    *
    * @tparam FromSubtype
    *   type of sealed/enum instance
    * @tparam ToSubtype
    *   type of sealed/enum instance
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 1.2.0
    */
  def withSealedSubtypeRenamed[FromSubtype, ToSubtype]
      : PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withSealedSubtypeRenamedImpl[From, To, Overrides, Flags, FromSubtype, ToSubtype]

  /** Alias to [[withSealedSubtypeRenamed]].
    *
    * @since 1.2.0
    */
  def withEnumCaseRenamed[FromSubtype, ToSubtype]: PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withSealedSubtypeRenamedImpl[From, To, Overrides, Flags, FromSubtype, ToSubtype]

  /** Use `f` instead of the primary constructor to construct the `To` value.
    *
    * Macro will read the names of Eta-expanded method's/lambda's parameters and try to match them with `From` getters.
    *
    * Values for each parameter can be provided the same way as if they were normal constructor's arguments.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#types-with-manually-provided-constructors]] for more
    *   details
    *
    * @tparam Ctor
    *   type of the Eta-expanded method/lambda which should return `To`
    * @param f
    *   method name or lambda which constructs `To`
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.8.4
    */
  def withConstructor[Ctor](
      f: Ctor
  )(implicit ev: IsFunction.Of[Ctor, To]): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withConstructorImpl[From, To, Overrides, Flags]

  /** Use `f` instead of the primary constructor to parse into `partial.Result[To]` value.
    *
    * Macro will read the names of Eta-expanded method's/lambda's parameters and try to match them with `From` getters.
    *
    * Values for each parameter can be provided the same way as if they were normal constructor's arguments.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#types-with-manually-provided-constructors]] for more
    *   details
    *
    * @tparam Ctor
    *   type of the Eta-expanded method/lambda which should return `partial.Result[To]`
    * @param f
    *   method name or lambda which constructs `partial.Result[To]`
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.8.4
    */
  def withConstructorPartial[Ctor](
      f: Ctor
  )(implicit
      ev: IsFunction.Of[Ctor, partial.Result[To]]
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withConstructorPartialImpl[From, To, Overrides, Flags]

  /** Use `f` instead of the primary constructor to parse into `Either[String, To]` value.
    *
    * Macro will read the names of Eta-expanded method's/lambda's parameters and try to match them with `From` getters.
    *
    * Values for each parameter can be provided the same way as if they were normal constructor's arguments.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#types-with-manually-provided-constructors]] for more
    *   details
    *
    * @tparam Ctor
    *   type of the Eta-expanded method/lambda which should return `Either[String, To]`
    * @param f
    *   method name or lambda which constructs `Either[String, To]`
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 1.0.0
    */
  def withConstructorEither[Ctor](
      f: Ctor
  )(implicit
      ev: IsFunction.Of[Ctor, Either[String, To]]
  ): PartialTransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withConstructorEitherImpl[From, To, Overrides, Flags]

  /** To use `fallback` when the source of type `From` is missing fields.
    *
    * Fallbacks can be stacked - then they will be tried in the order in which they were added.
    *
    * @see
    *   TODO
    *
    * @tparam FromFallback
    *   type of the fallback value which would be checked for fields when the `From` value would be missing
    * @param fallback
    *   fallback value which would be checked for fields when the `From` value would be missing
    * @return
    *   [[io.scalaland.chimney.dsl.TransformerOverrides]]
    *
    * @since TODO
    */
  def withFallback[FromFallback](
      fallback: FromFallback
  ): TransformerOverrides[From, To, ? <: TransformerOverrides, Flags] =
    macro PartialTransformerIntoMacros.withFallbackImpl[From, To, Overrides, Flags, FromFallback]

  /** Apply configured partial transformation in-place.
    *
    * It runs macro that tries to derive instance of `PartialTransformer[From, To]` and immediately apply it to captured
    * `source` value. When transformation can't be derived, it results with compilation error.
    *
    * @return
    *   partial transformation result of type `partial.Result[To]`
    *
    * @since 0.7.0
    */
  def transform[ImplicitScopeFlags <: TransformerFlags](implicit
      tc: io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]
  ): partial.Result[To] =
    macro TransformerMacros
      .derivePartialTransformationWithConfigNoFailFast[From, To, Overrides, Flags, ImplicitScopeFlags]

  /** Apply configured partial transformation in-place in a short-circuit (fail fast) mode.
    *
    * It runs macro that tries to derive instance of `PartialTransformer[From, To]` and immediately apply it to captured
    * `source` value. When transformation can't be derived, it results with compilation error.
    *
    * @return
    *   partial transformation result of type `partial.Result[To]`
    *
    * @since 0.7.0
    */
  def transformFailFast[ImplicitScopeFlags <: TransformerFlags](implicit
      tc: io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]
  ): partial.Result[To] =
    macro TransformerMacros
      .derivePartialTransformationWithConfigFailFast[From, To, Overrides, Flags, ImplicitScopeFlags]

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PartialTransformerInto(source, td.addOverride(overrideData)).asInstanceOf[this.type]
}
