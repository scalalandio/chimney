package io.scalaland.chimney.syntax

import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.internal.runtime.{IsCollection, IsEither, IsMap, IsOption}

import scala.annotation.{compileTimeOnly, unused}
import scala.util.Try

// Extension methods in dsl.* summon TypeClass.AutoDerived while extension methods in syntax.* summon TypeClass.
// This help us preserve legacy behavior in dsl code while keeping stricter separation in auto/syntax imports.

/** Provides transformer operations on values of any type.
  *
  * @tparam From
  *   type of source value
  * @param source
  *   wrapped source value
  *
  * @since 0.4.0
  */
extension [From](source: From) {

  /** Performs in-place transformation of captured source value to target type.
    *
    * If you want to customize transformer behavior, consider using [[io.scalaland.chimney.dsl.TransformerOps#into]]
    * method.
    *
    * @see
    *   [[io.scalaland.chimney.auto#deriveAutomaticTransformer]] for default implicit instance
    *
    * @tparam To
    *   target type
    * @param transformer
    *   implicit instance of [[io.scalaland.chimney.Transformer]] type class
    * @return
    *   transformed value of target type `To`
    * @since 0.1.0
    */
  transparent inline def transformInto[To](implicit transformer: Transformer[From, To]): To =
    transformer.transform(source)
}

/** Provides partial transformer operations on values of any type.
  *
  * @tparam From
  *   type of source value
  * @param source
  *   wrapped source value
  *
  * @since 0.7.0
  */
extension [From](source: From) {

  /** Performs in-place partial transformation of captured source value to target type.
    *
    * If you want to customize transformer behavior, consider using
    * [[io.scalaland.chimney.dsl.PartialTransformerOps#intoPartial]] method.
    *
    * @see
    *   [[io.scalaland.chimney.auto#deriveAutomaticPartialTransformer]] for default implicit instance
    *
    * @tparam To
    *   result target type of partial transformation
    * @param transformer
    *   implicit instance of [[io.scalaland.chimney.Transformer]] type class
    * @return
    *   partial transformation result value of target type `To`
    *
    * @since 0.7.0
    */
  transparent inline def transformIntoPartial[To](implicit
      transformer: PartialTransformer[From, To]
  ): partial.Result[To] =
    transformIntoPartial(failFast = false)

  /** Performs in-place partial transformation of captured source value to target type.
    *
    * If you want to customize transformer behavior, consider using
    * [[io.scalaland.chimney.dsl.PartialTransformerOps#intoPartial]] method.
    *
    * @see
    *   [[io.scalaland.chimney.auto#deriveAutomaticPartialTransformer]] for default implicit instance
    *
    * @tparam To
    *   result target type of partial transformation
    * @param failFast
    *   should fail as early as the first set of errors appear
    * @param transformer
    *   implicit instance of [[io.scalaland.chimney.Transformer]] type class
    * @return
    *   partial transformation result value of target type `To`
    *
    * @since 0.7.0
    */
  transparent inline def transformIntoPartial[To](failFast: Boolean)(implicit
      transformer: PartialTransformer[From, To]
  ): partial.Result[To] =
    transformer.transform(source, failFast)
}

/** Provides patcher operations on values of any type
  *
  * @param obj
  *   wrapped object to patch
  * @tparam A
  *   type of object to patch
  *
  * @since 0.1.3
  */
extension [A](obj: A) {

  /** Performs in-place patching of wrapped object with provided value.
    *
    * If you want to customize patching behavior, consider using [[io.scalaland.chimney.dsl.PatcherOps#using using]]
    * method.
    *
    * @see
    *   [[io.scalaland.chimney.auto#deriveAutomaticPatcher]] for default implicit instance
    *
    * @tparam Patch
    *   type of patch object
    * @param patch
    *   patch object value
    * @param patcher
    *   implicit instance of [[io.scalaland.chimney.Patcher]] type class
    * @return
    *   patched value
    *
    * @since 0.4.0
    */
  transparent inline def patchUsing[Patch](patch: Patch)(implicit patcher: Patcher[A, Patch]): A =
    patcher.patch(obj, patch)
}

// $COVERAGE-OFF$methods used only within macro-erased expressions

/** Allows subtype matching when selecting path to override in Chimney DSL.
  *
  * @tparam A
  *   type to match
  *
  * @since 1.0.0
  */
extension [A](@unused a: A) {

  /** Allows paths like `_.adt.matching[Subtype].field` when selecting the target fields to override in Chimney DSL.
    *
    * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
    *
    * @tparam B
    *   subtype for which override should be provided
    * @return
    *   stubs a value of selected subtype
    *
    * @since 1.0.0
    */
  inline def matching[B <: A]: B = compiletime.error(".matching should be only called within Chimney DSL")

  /** Allows paths like `_.optional.matchingSome.field` when selecting the target fields to override in Chimney DSL.
    *
    * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
    *
    * @return
    *   stubs a value extracted from `Some`
    *
    * @since 1.0.0
    */
  inline def matchingSome[SV, S](implicit @unused ev: IsOption.Of[A, SV, S]): SV =
    compiletime.error(".matchingSome should be only called within Chimney DSL")

  /** Allows paths like `_.either.matchingLeft.field` when selecting the target fields to override in Chimney DSL.
    *
    * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
    *
    * @return
    *   stubs a value extracted from `Left`
    *
    * @since 1.0.0
    */
  inline def matchingLeft[LV, RV, L, R](implicit @unused ev: IsEither.Of[A, LV, RV, L, R]): LV =
    compiletime.error(".matchingLeft should be only called within Chimney DSL")

  /** Allows paths like `_.either.matchingRight.field` when selecting the target fields to override in Chimney DSL.
    *
    * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
    *
    * @return
    *   stubs a value extracted from `Right`
    *
    * @since 1.0.0
    */
  inline def matchingRight[LV, RV, L, R](implicit @unused ev: IsEither.Of[A, LV, RV, L, R]): RV =
    compiletime.error(".matchingRight should be only called within Chimney DSL")
}

/** Allow item extraction from collections when selecting path to override in Chimney DSL.
  *
  * @tparam C
  *   type of the collection
  * @tparam I
  *   type of items in the collection
  *
  * @since 1.0.0
  */
extension [C[_], I](@unused cc: C[I]) {

  /** Allows paths like `_.collection.everyItem.field` when selecting the target fields to override in Chimney DSL.
    *
    * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
    *
    * @return
    *   stubs an item extracted from the collection
    *
    * @since 1.0.0
    */
  inline def everyItem(implicit @unused ev: IsCollection.Of[C[I], I]): I =
    compiletime.error(".everyItem should be only called within Chimney DSL")
}

/** Allow key/value extraction from maps when selecting path to override in Chimney DSL.
  *
  * @tparam M
  *   type of the map
  * @tparam K
  *   type of keys in the map
  * @tparam V
  *   type of values in the map
  *
  * @since 1.0.0
  */
extension [M[_, _], K, V](@unused cc: M[K, V]) {

  /** Allows paths like `_.map.everyMapKey.field` when selecting the target fields to override in Chimney DSL.
    *
    * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
    *
    * @return
    *   stubs a key extracted from the map
    *
    * @since 1.0.0
    */
  inline def everyMapKey(implicit @unused ev: IsMap.Of[M[K, V], K, V]): K =
    compiletime.error(".everyMapKey should be only called within Chimney DSL")

  /** Allows paths like `_.map.everyMapValue.field` when selecting the target fields to override in Chimney DSL.
    *
    * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
    *
    * @return
    *   stubs a value extracted from the map
    *
    * @since 1.0.0
    */
  inline def everyMapValue(implicit @unused ev: IsMap.Of[M[K, V], K, V]): V =
    compiletime.error(".everyMapValue should be only called within Chimney DSL")
}

// $COVERAGE-ON$
