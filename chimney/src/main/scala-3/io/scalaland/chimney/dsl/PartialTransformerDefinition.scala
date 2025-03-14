package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.{partial, PartialTransformer}
import io.scalaland.chimney.internal.compiletime.dsl.*
import io.scalaland.chimney.internal.runtime.{
  IsFunction,
  Path,
  TransformerFlags,
  TransformerOverrides,
  WithRuntimeDataStore
}

/** Allows customization of [[io.scalaland.chimney.PartialTransformer]] derivation.
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
  * @since 0.7.0
  */
final class PartialTransformerDefinition[From, To, Overrides <: TransformerOverrides, Flags <: TransformerFlags](
    val runtimeData: TransformerDefinitionCommons.RuntimeDataStore
) extends TransformerFlagsDsl[
      [Flags1 <: TransformerFlags] =>> PartialTransformerDefinition[From, To, Overrides, Flags1],
      Flags
    ]
    with TransformerDefinitionCommons[
      [Overrides1 <: TransformerOverrides] =>> PartialTransformerDefinition[From, To, Overrides1, Flags]
    ]
    with WithRuntimeDataStore {

  /** Use provided `value` for field picked using `selector`.
    *
    * By default, if `From` is missing field picked by `selector`, compilation fails.
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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  transparent inline def withFieldConst[T, U](
      inline selector: To => T,
      inline value: U
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldConstImpl('this, 'selector, 'value) }

  /** Use provided partial result `value` for field picked using `selector`.
    *
    * By default, if `From` is missing field picked by `selector`, compilation fails.
    *
    * @see
    *   [[https://chimney.readthedocs.io/supported-transformations/#wiring-the-constructors-parameter-to-a-provided-value]]
    *   for more details
    *
    * @tparam T
    *   type of target field
    * @tparam U
    *   type of computed value
    * @param selector
    *   target field in `To`, defined like `_.name`
    * @param value
    *   constant value to use for the target field
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  transparent inline def withFieldConstPartial[T, U](
      inline selector: To => T,
      inline value: partial.Result[U]
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldConstPartialImpl('this, 'selector, 'value) }

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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  transparent inline def withFieldComputed[T, U](
      inline selector: To => T,
      inline f: From => U
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldComputedImpl('this, 'selector, 'f) }

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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 1.6.0
    */
  transparent inline def withFieldComputedFrom[S, T, U](inline selectorFrom: From => S)(
      inline selectorTo: To => T,
      inline f: S => U
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldComputedFromImpl('this, 'selectorFrom, 'selectorTo, 'f) }

  /** Use the function `f` to compute a partial result for the field picked using the `selector`.
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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  transparent inline def withFieldComputedPartial[T, U](
      inline selector: To => T,
      inline f: From => partial.Result[U]
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldComputedPartialImpl('this, 'selector, 'f) }

  /** Use the function `f` to compute a partial result of the field picked using the `selectorTo` from a value extracted
    * with `selectorFrom` as an input.
    *
    * By default, if `From` is missing a field and it's not provided with some `selector`, the compilation fails.
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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 1.6.0
    */
  transparent inline def withFieldComputedPartialFrom[S, T, U](inline selectorFrom: From => S)(
      inline selectorTo: To => T,
      inline f: S => partial.Result[U]
  )(using U <:< T): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldComputedPartialFromImpl('this, 'selectorFrom, 'selectorTo, 'f) }

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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.7.0
    */
  transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldRenamedImpl('this, 'selectorFrom, 'selectorTo) }

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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 1.7.0
    */
  transparent inline def withFieldUnused[T](
      inline selectorFrom: From => T
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withFieldUnusedImpl('this, 'selectorFrom) }

  /** Use `f` to calculate the unmatched subtype when mapping one sealed/enum into another.
    *
    * By default, if mapping one coproduct in `From` into another coproduct in `To` derivation expects that coproducts
    * to have matching names of its components, and for every component in `To` field's type there is matching component
    * in `From` type. If some component is missing it fails compilation unless provided replacement with this operation.
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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 1.0.0
    */
  transparent inline def withSealedSubtypeHandled[Subtype](
      inline f: Subtype => To
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withSealedSubtypeHandledImpl('this, 'f) }

  /** Alias to [[withSealedSubtypeHandled]].
    *
    * @since 1.0.0
    */
  transparent inline def withEnumCaseHandled[Subtype](
      inline f: Subtype => To
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withSealedSubtypeHandledImpl('this, 'f) }

  /** Use `f` to calculate the unmatched subtype's partial.Result when mapping one sealed/enum into another.
    *
    * By default, if mapping one coproduct in `From` into another coproduct in `To` derivation expects that coproducts
    * to have matching names of its components, and for every component in `To` field's type there is matching component
    * in `From` type. If some component is missing it fails compilation unless provided replacement with this operation.
    *
    * For convenience/readability [[withEnumCaseHandledPartial]] alias can be used (e.g. for Scala 3 enums or Java
    * enums).
    *
    * It differs from `withFieldComputedPartial(_.matching[Subtype], src => ...)`, since `withSealedSubtypeHandled`
    * matches on a `From` subtype, while `.matching[Subtype]` matches on a `To` value's piece.
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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 1.0.0
    */
  transparent inline def withSealedSubtypeHandledPartial[Subtype](
      inline f: Subtype => partial.Result[To]
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withSealedSubtypeHandledPartialImpl('this, 'f) }

  /** Alias to [[withSealedSubtypeHandledPartial]].
    *
    * @since 1.0.0
    */
  transparent inline def withEnumCaseHandledPartial[Subtype](
      inline f: Subtype => partial.Result[To]
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withSealedSubtypeHandledPartialImpl('this, 'f) }

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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 1.2.0
    */
  transparent inline def withSealedSubtypeRenamed[FromSubtype, ToSubtype]
      : PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${
      PartialTransformerDefinitionMacros
        .withSealedSubtypeRenamedImpl[From, To, Overrides, Flags, FromSubtype, ToSubtype]('this)
    }

  /** Alias to [[withSealedSubtypeRenamed]].
    *
    * @since 1.2.0
    */
  transparent inline def withEnumCaseRenamed[FromSubtype, ToSubtype]
      : PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${
      PartialTransformerDefinitionMacros
        .withSealedSubtypeRenamedImpl[From, To, Overrides, Flags, FromSubtype, ToSubtype]('this)
    }

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
  transparent inline def withSealedSubtypeUnmatched[T](
      inline selectorTo: To => T
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${
      PartialTransformerDefinitionMacros.withSealedSubtypeUnmatchedImpl[From, To, Overrides, Flags, T](
        'this,
        'selectorTo
      )
    }

  /** Alias to [[withSealedSubtypeUnmatched]].
    *
    * @since 1.7.0
    */
  transparent inline def withEnumCaseUnmatched[T](
      inline selectorTo: To => T
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${
      PartialTransformerDefinitionMacros.withSealedSubtypeUnmatchedImpl[From, To, Overrides, Flags, T](
        'this,
        'selectorTo
      )
    }

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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 1.7.0
    */
  transparent inline def withFallback[FromFallback](
      inline fallback: FromFallback
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withFallbackImpl('this, 'fallback) }

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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 1.7.0
    */
  transparent inline def withFallbackFrom[T, FromFallback](inline selectorFrom: From => T)(
      inline fallback: FromFallback
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withFallbackFromImpl('this, 'selectorFrom, 'fallback) }

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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.8.4
    */
  transparent inline def withConstructor[Ctor](
      inline f: Ctor
  )(using IsFunction.Of[Ctor, To]): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withConstructorImpl('this, 'f) }

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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 1.6.0
    */
  transparent inline def withConstructorTo[T, Ctor](inline selector: To => T)(
      inline f: Ctor
  )(using IsFunction.Of[Ctor, T]): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withConstructorToImpl('this, 'selector, 'f) }

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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 0.8.4
    */
  transparent inline def withConstructorPartial[Ctor](
      inline f: Ctor
  )(using
      IsFunction.Of[Ctor, partial.Result[To]]
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withConstructorPartialImpl('this, 'f) }

  /** Use `f` instead of the primary constructor to parse into the value extracted from `To` using the `selector`.
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
    *   type of the Eta-expanded method/lambda which should return `partial.Result[T]`
    * @param selector
    *   target field in `To`, defined like `_.name`
    * @param f
    *   method name or lambda which constructs `partial.Result[T]`
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 1.6.0
    */
  transparent inline def withConstructorPartialTo[T, Ctor](inline selector: To => T)(
      inline f: Ctor
  )(using
      IsFunction.Of[Ctor, partial.Result[T]]
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withConstructorPartialToImpl('this, 'selector, 'f) }

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
    *   [[io.scalaland.chimney.dsl.PartialTransformerDefinition]]
    *
    * @since 1.0.0
    */
  transparent inline def withConstructorEither[Ctor](
      inline f: Ctor
  )(using
      IsFunction.Of[Ctor, Either[String, To]]
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withConstructorEitherImpl('this, 'f) }

  /** Use `f` instead of the primary constructor to parse into the `Either` value extracted from `To` using the
    * `selector`.
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
    *   type of the Eta-expanded method/lambda which should return `Either[String, T]`
    * @param selector
    *   target field in `To`, defined like `_.name`
    * @param f
    *   method name or lambda which constructs `Either[String, T]`
    * @return
    *   [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 1.6.0
    */
  transparent inline def withConstructorEitherTo[T, Ctor](inline selector: To => T)(
      inline f: Ctor
  )(using
      IsFunction.Of[Ctor, Either[String, T]]
  ): PartialTransformerDefinition[From, To, ? <: TransformerOverrides, Flags] =
    ${ PartialTransformerDefinitionMacros.withConstructorEitherToImpl('this, 'selector, 'f) }

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
    *   [[io.scalaland.chimney.dsl.TransformerSourceFlagsDsl.OfPartialTransformerDefinition]]
    *
    * @since 1.6.0
    */
  transparent inline def withSourceFlag[T](
      inline selectorFrom: From => T
  ): TransformerSourceFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, ? <: Path] =
    ${ PartialTransformerDefinitionMacros.withSourceFlagImpl[From, To, Overrides, Flags, T]('this, 'selectorFrom) }

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
    *   [[io.scalaland.chimney.dsl.TransformerTargetFlagsDsl.OfPartialTransformerDefinition]]
    *
    * @since 1.6.0
    */
  transparent inline def withTargetFlag[T](
      inline selectorTo: To => T
  ): TransformerTargetFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, ? <: Path] =
    ${ PartialTransformerDefinitionMacros.withTargetFlagImpl[From, To, Overrides, Flags, T]('this, 'selectorTo) }

  /** Build Partial Transformer using current configuration.
    *
    * It runs macro that tries to derive instance of `PartialTransformer[From, To]`. When transformation can't be
    * derived, it results with compilation error.
    *
    * @return
    *   [[io.scalaland.chimney.PartialTransformer]] type class instance
    *
    * @since 0.7.0
    */
  inline def buildTransformer[ImplicitScopeFlags <: TransformerFlags](using
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): PartialTransformer[From, To] =
    ${ TransformerMacros.derivePartialTransformerWithConfig[From, To, Overrides, Flags, ImplicitScopeFlags]('this) }

  private[chimney] def addOverride(overrideData: Any): this.type =
    new PartialTransformerDefinition(overrideData +: runtimeData).asInstanceOf[this.type]
}
