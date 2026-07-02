package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.TransformerIntoMacros
import io.scalaland.chimney.internal.runtime.{
  IsFunction,
  Path,
  TransformerFlags,
  TransformerOverrides,
  WithRuntimeDataStore
}

import scala.language.experimental.macros

/** Provides DSL for configuring [[io.scalaland.chimney.Transformer]]'s generation and using the result to transform
  * value at the same time
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
  * @since 0.1.0
  */
final class TransformerInto[From, To, Overrides <: TransformerOverrides, Flags <: TransformerFlags](
    val source: From,
    val td: TransformerDefinition[From, To, Overrides, Flags]
) extends TransformerFlagsDsl[Lambda[
      `Flags1 <: TransformerFlags` => TransformerInto[From, To, Overrides, Flags1]
    ], Flags]
    with WithRuntimeDataStore {

  /** Lifts the current transformation to the partial transformation.
    *
    * It keeps all the configuration, provided missing values, renames, coproduct instances etc.
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    */
  def partial: PartialTransformerInto[From, To, Overrides, Flags] =
    new PartialTransformerInto[From, To, Overrides, Flags](source, td.partial)

  /** Use the `value` provided here for the field picked using the `selector`.
    *
    * By default, if `From` is missing a field and it's not provided with some `selector`, the compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-a-provided-value]]
    *   for more details
    *
    * @return
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  def withFieldConst[T, U](selector: To => T, value: U)(implicit
      ev: U <:< T
  ): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withFieldConstImpl[From, To, Overrides, Flags]

  /** Use the function `f` to compute a value of the field picked using the `selector`.
    *
    * By default, if `From` is missing a field and it's not provided with some `selector`, the compilation fails.
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
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  def withFieldComputed[T, U](
      selector: To => T,
      f: From => U
  )(implicit ev: U <:< T): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withFieldComputedImpl[From, To, Overrides, Flags]

  /** Use the function `f` to compute a value of the field picked using the `selectorTo` from a value extracted with
    * `selectorFrom` as an input.
    *
    * By default, if `From` is missing a field and it's not provided with some `selectorTo`, the compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-computed-value]]
    *   for more details
    *
    * @tparam S
    *   * type of source field
    * @tparam T
    *   type of target field
    * @tparam U
    *   type of computed value
    * @param selectorFrom
    *   source field in `From`, defined like `_.name`
    * @param selectorTo
    *   target field in `To`, defined like `_.name`
    * @param f
    *   function used to compute value of the target field
    * @return
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 1.6.0
    */
  def withFieldComputedFrom[S, T, U](selectorFrom: From => S)(
      selectorTo: To => T,
      f: S => U
  )(implicit ev: U <:< T): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withFieldComputedFromImpl[From, To, Overrides, Flags]

  /** Use the `selectorFrom` field in `From` to obtain the value of the `selectorTo` field in `To`.
    *
    * By default, if `From` is missing a field and it's not provided with some `selectorTo`, the compilation fails.
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
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.5
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withFieldRenamedImpl[From, To, Overrides, Flags]

  /** Mark field as expected to be unused when it would fail [[UnusedFieldPolicy]] by default.
    *
    * @see
    *   [[https://chimney.readthedocs.io/cookbook/#checking-for-unused-source-fieldsunmatched-target-subtypes]] for more
    *   details
    *
    * @tparam T
    *   type of source field
    * @param selectorFrom
    *   source field in `From`, defined like `_.originalName`
    * @return
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 1.7.0
    */
  def withFieldUnused[T](selectorFrom: From => T): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withFieldUnusedImpl[From, To, Overrides, Flags]

  /** Use `f` to calculate the unmatched subtype when mapping one sealed/enum into another.
    *
    * By default, if mapping one coproduct in `From` into another coproduct in `To` derivation expects that coproducts
    * will have matching names of its components, and for every component in `To` field's type there is matching
    * component in `From` type. If some component is missing it will fail.
    *
    * For convenience/readability [[withEnumCaseHandled]] alias can be used (e.g. for Scala 3 enums or Java enums).
    *
    * It differs from `withFieldComputed(_.matching[Subtype], src => ...)`, since `withSealedSubtypeHandled` matches on
    * a `From` subtype, while `.matching[Subtype]` matches on a `To` value's piece.
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
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 1.0.0
    */
  def withSealedSubtypeHandled[Subtype](f: Subtype => To): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withSealedSubtypeHandledImpl[From, To, Overrides, Flags, Subtype]

  /** Alias to [[withSealedSubtypeHandled]].
    *
    * @since 1.0.0
    */
  def withEnumCaseHandled[Subtype](f: Subtype => To): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withSealedSubtypeHandledImpl[From, To, Overrides, Flags, Subtype]

  /** Use the `FromSubtype` in `From` as a source for the `ToSubtype` in `To`.
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
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 1.2.0
    */
  def withSealedSubtypeRenamed[FromSubtype, ToSubtype]: TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withSealedSubtypeRenamedImpl[From, To, Overrides, Flags, FromSubtype, ToSubtype]

  /** Alias to [[withSealedSubtypeRenamed]].
    *
    * @since 1.2.0
    */
  def withEnumCaseRenamed[FromSubtype, ToSubtype]: TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withSealedSubtypeRenamedImpl[From, To, Overrides, Flags, FromSubtype, ToSubtype]

  /** Mark subtype as expected to be unmatched when it would fail [[UnmatchedSubtypePolicy]] by default.
    *
    * @see
    *   [[https://chimney.readthedocs.io/cookbook/#checking-for-unused-source-fieldsunmatched-target-subtypes]] for more
    *   details
    *
    * @tparam T
    *   type of subtype
    * @param selectorTo
    *   target subtype in `To`, defined like `_.matching[Subtype]`
    * @return
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 1.7.0
    */
  def withSealedSubtypeUnmatched[T](selectorTo: To => T): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withSealedSubtypeUnmatchedImpl[From, To, Overrides, Flags]

  /** Alias to [[withSealedSubtypeUnmatched]].
    *
    * @since 1.7.0
    */
  def withEnumCaseUnmatched[T](selectorTo: To => T): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withSealedSubtypeUnmatchedImpl[From, To, Overrides, Flags]

  /** To use `fallback` when the source of type `From` is missing fields.
    *
    * Fallbacks can be stacked - then they will be tried in the order in which they were added.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#merging-multiple-input-sources-into-a-single-target-value]]
    *   for more details
    *
    * @tparam FromFallback
    *   type of the fallback value which would be checked for fields when the `From` value would be missing
    * @param fallback
    *   fallback value which would be checked for fields when the `From` value would be missing
    * @return
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 1.7.0
    */
  def withFallback[FromFallback](fallback: FromFallback): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withFallbackImpl[From, To, Overrides, Flags, FromFallback]

  /** To use `fallback` when the source of type `T`, extracted with `selectorFrom`, is missing fields.
    *
    * Fallbacks can be stacked - then they will be tried in the order in which they were added.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#merging-multiple-input-sources-into-a-single-target-value]]
    *   for more details
    *
    * @tparam T
    *   type of the source value that fallback is provided for
    * @tparam FromFallback
    *   type of the fallback value which would be checked for fields when the `From` value would be missing
    * @param selectorFrom
    *   path to the source value the fallback will be provided for
    * @param fallback
    *   fallback value which would be checked for fields when the `From` value would be missing
    * @return
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 1.7.0
    */
  def withFallbackFrom[T, FromFallback](selectorFrom: From => T)(
      fallback: FromFallback
  ): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withFallbackFromImpl[From, To, Overrides, Flags, FromFallback]

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
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.8.4
    */
  def withConstructor[Ctor](f: Ctor)(implicit
      ev: IsFunction.Of[Ctor, To]
  ): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withConstructorImpl[From, To, Overrides, Flags]

  /** Use `f` instead of the primary constructor to construct the value extracted from `To` using the `selector`.
    *
    * Macro will read the names of Eta-expanded method's/lambda's parameters and try to match them with `From` getters.
    *
    * Values for each parameter can be provided the same way as if they were normal constructor's arguments.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#types-with-manually-provided-constructors]] for more
    *   details
    *
    * @tparam T
    *   type of the value which would be constructed with a custom constructor
    * @tparam Ctor
    *   type of the Eta-expanded method/lambda which should return `T`
    * @param selector
    *   target field in `To`, defined like `_.name`
    * @param f
    *   method name or lambda which constructs `To`
    * @return
    *   [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 1.6.0
    */
  def withConstructorTo[T, Ctor](selector: To => T)(f: Ctor)(implicit
      ev: IsFunction.Of[Ctor, T]
  ): TransformerInto[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerIntoMacros.withConstructorToImpl[From, To, Overrides, Flags]

  /** Define a flag only on some source value using the `selectorFrom`, rather than globally.
    *
    * @see
    *   [[https://chimney.readthedocs.io/cookbook/#constraining-flags-to-a-specific-fieldsubtype]] for more details
    *
    * @tparam T
    *   type of the source field
    * @param selectorFrom
    *   source field in `From`, defined like `_.name`
    * @return
    *   [[io.scalaland.chimney.dsl.TransformerSourceFlagsDsl.OfTransformerInto]]
    *
    * @since 1.6.0
    */
  def withSourceFlag[T](
      selectorFrom: From => T
  ): TransformerSourceFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, ? <: Path] =
    macro TransformerIntoMacros.withSourceFlagImpl[From, To, Overrides, Flags]

  /** Define a flag only on some source value using the `selectorTo`, rather than globally.
    *
    * @see
    *   [[https://chimney.readthedocs.io/cookbook/#constraining-flags-to-a-specific-fieldsubtype]] for more details
    *
    * @tparam T
    *   type of the target field
    * @param selectorTo
    *   target field in `To`, defined like `_.name`
    * @return
    *   [[io.scalaland.chimney.dsl.TransformerTargetFlagsDsl.OfTransformerInto]]
    *
    * @since 1.6.0
    */
  def withTargetFlag[T](
      selectorTo: To => T
  ): TransformerTargetFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, ? <: Path] =
    macro TransformerIntoMacros.withTargetFlagImpl[From, To, Overrides, Flags]

  /** Apply configured transformation in-place.
    *
    * It runs macro that tries to derive instance of `Transformer[From, To]` and immediately apply it to captured
    * `source` value. When transformation can't be derived, it results with compilation error.
    *
    * @return
    *   transformed value of type `To`
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
