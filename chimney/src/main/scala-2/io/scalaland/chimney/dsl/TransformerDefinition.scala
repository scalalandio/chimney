package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.dsl.TransformerDefinitionMacros
import io.scalaland.chimney.internal.runtime.{IsFunction, TransformerFlags, TransformerOverrides, WithRuntimeDataStore}

import scala.language.experimental.macros

/** Allows customization of [[io.scalaland.chimney.Transformer]] derivation.
  *
  * @tparam From
  *   type of input value
  * @tparam To
  *   type of output value
  * @tparam Overrides
  *   type-level encoded config
  * @tparam Flags
  *   type-level encoded flags
  *
  * @since 0.4.0
  */
final class TransformerDefinition[From, To, Overrides <: TransformerOverrides, Flags <: TransformerFlags](
    val runtimeData: TransformerDefinitionCommons.RuntimeDataStore
) extends TransformerFlagsDsl[Lambda[
      `Flags1 <: TransformerFlags` => TransformerDefinition[From, To, Overrides, Flags1]
    ], Flags]
    with TransformerDefinitionCommons[
      Lambda[`Overrides1 <: TransformerOverrides` => TransformerDefinition[From, To, Overrides1, Flags]]
    ]
    with WithRuntimeDataStore {

  /** Lifts current transformer definition as `PartialTransformer` definition.
    *
    * It keeps all the configuration, provided missing values, renames, coproduct instances etc.
    *
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    */
  def partial: PartialTransformerDefinition[From, To, Overrides, Flags] =
    new PartialTransformerDefinition[From, To, Overrides, Flags](runtimeData)

  /** Use provided value `value` for field picked using `selector`.
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
    *   [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 0.4.0
    */
  def withFieldConst[T, U](selector: To => T, value: U)(implicit
      ev: U <:< T
  ): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerDefinitionMacros.withFieldConstImpl[From, To, Overrides, Flags]

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
    *   [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 0.4.0
    */
  def withFieldComputed[T, U](
      selector: To => T,
      f: From => U
  )(implicit ev: U <:< T): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerDefinitionMacros.withFieldComputedImpl[From, To, Overrides, Flags]

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`.
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
    *   [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 0.4.0
    */
  def withFieldRenamed[T, U](
      selectorFrom: From => T,
      selectorTo: To => U
  ): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerDefinitionMacros.withFieldRenamedImpl[From, To, Overrides, Flags]

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
    *   [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 1.0.0
    */
  def withSealedSubtypeHandled[Subtype](
      f: Subtype => To
  ): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerDefinitionMacros.withSealedSubtypeHandledImpl[From, To, Overrides, Flags, Subtype]

  /** Alias to [[withSealedSubtypeHandled]].
    *
    * @since 1.0.0
    */
  def withEnumCaseHandled[Subtype](
      f: Subtype => To
  ): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerDefinitionMacros.withSealedSubtypeHandledImpl[From, To, Overrides, Flags, Subtype]

  /** Renamed to [[withSealedSubtypeHandled]].
    *
    * @since 0.4.0
    */
  @deprecated("Use .withSealedSubtypeHandled or .withEnumCaseHandled for more clarity", "1.0.0")
  def withCoproductInstance[Subtype](
      f: Subtype => To
  ): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerDefinitionMacros.withSealedSubtypeHandledImpl[From, To, Overrides, Flags, Subtype]

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
    *   [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 1.2.0
    */
  def withSealedSubtypeRenamed[FromSubtype, ToSubtype]
      : TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerDefinitionMacros.withSealedSubtypeRenamedImpl[From, To, Overrides, Flags, FromSubtype, ToSubtype]

  /** Alias to [[withSealedSubtypeRenamed]].
    *
    * @since 1.2.0
    */
  def withEnumCaseRenamed[FromSubtype, ToSubtype]: TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerDefinitionMacros.withSealedSubtypeRenamedImpl[From, To, Overrides, Flags, FromSubtype, ToSubtype]

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
    *   [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since 0.8.4
    */
  def withConstructor[Ctor](
      f: Ctor
  )(implicit ev: IsFunction.Of[Ctor, To]): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerDefinitionMacros.withConstructorImpl[From, To, Overrides, Flags]

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
    *   [[io.scalaland.chimney.dsl.TransformerDefinition]]
    *
    * @since TODO
    */
  def withFallback[FromFallback](
      fallback: FromFallback
  ): TransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    macro TransformerDefinitionMacros.withFallbackImpl[From, To, Overrides, Flags, FromFallback]

  /** Build Transformer using current configuration.
    *
    * It runs macro that tries to derive instance of `Transformer[From, To]`. When transformation can't be derived, it
    * results with compilation error.
    *
    * @return
    *   [[io.scalaland.chimney.Transformer]] type class instance
    *
    * @since 0.4.0
    */
  def buildTransformer[ImplicitScopeFlags <: TransformerFlags](implicit
      tc: io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]
  ): Transformer[From, To] =
    macro TransformerMacros.deriveTotalTransformerWithConfig[From, To, Overrides, Flags, ImplicitScopeFlags]

  private[chimney] def addOverride(overrideData: Any): this.type =
    new TransformerDefinition(overrideData +: runtimeData).asInstanceOf[this.type]
}
