package io.scalaland.chimney

import io.scalaland.chimney.internal.runtime.{IsCollection, IsEither, IsMap, IsOption}

import scala.annotation.{compileTimeOnly, unused}

/** Imports only extension methods for summoning and using Transformer, PartialTransformer or Patcher
  *
  * @since 0.8.0
  */
package object syntax {

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
  implicit class TransformerOps[From](private val source: From) extends AnyVal {

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
      *
      * @since 0.1.0
      */
    final def transformInto[To](implicit transformer: Transformer[From, To]): To =
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
  implicit class PartialTransformerOps[From](private val source: From) extends AnyVal {

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
    final def transformIntoPartial[To](implicit
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
    final def transformIntoPartial[To](failFast: Boolean)(implicit
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
  implicit class PatcherOps[A](private val obj: A) extends AnyVal {

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
    final def patchUsing[Patch](patch: Patch)(implicit patcher: Patcher[A, Patch]): A =
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
  implicit final class TransformationMatchingPathOps[A](@unused private val a: A) extends AnyVal {

    /** Allows paths like `_.adt.matching[Subtype].field` when selecting the target fields to override in Chimney DSL.
      *
      * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
      *
      * @tparam B
      *   subtype for which override should be provided
      * @return
      *   stubs value of selected subtype
      *
      * @since 1.0.0
      */
    @compileTimeOnly(".matching should be only called within Chimney DSL")
    def matching[B <: A]: B = sys.error("")

    /** Allows paths like `_.optional.matchingSome.field` when selecting the target fields to override in Chimney DSL.
      *
      * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
      *
      * @return
      *   stubs a value extracted from `Some`
      *
      * @since 1.0.0
      */
    @compileTimeOnly(".matchingSome should be only called within Chimney DSL")
    def matchingSome[SV, S](implicit @unused ev: IsOption.Of[A, SV, S]): SV = sys.error("")

    /** Allows paths like `_.either.matchingLeft` when selecting the target fields to override in Chimney DSL.
      *
      * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
      *
      * @return
      *   stubs a value extracted from `Left`
      *
      * @since 1.0.0
      */
    @compileTimeOnly(".matchingLeft should be only called within Chimney DSL")
    def matchingLeft[LV, RV, L, R](implicit @unused ev: IsEither.Of[A, LV, RV, L, R]): LV = sys.error("")

    /** Allows paths like `_.either.matchingRight.field` when selecting the target fields to override in Chimney DSL.
      *
      * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
      *
      * @return
      *   stubs a value extracted from `Right`
      *
      * @since 1.0.0
      */
    @compileTimeOnly(".matchingRight should be only called within Chimney DSL")
    def matchingRight[LV, RV, L, R](implicit @unused ev: IsEither.Of[A, LV, RV, L, R]): RV = sys.error("")
  }

  /** Allow item extraction when selecting path to override in Chimney DSL.
    *
    * @tparam C
    *   type of the collection
    * @tparam I
    *   type of items in the collection
    *
    * @since 1.0.0
    */
  implicit final class TransformationCollectionPathOps[C[_], I](@unused private val cc: C[I]) extends AnyVal {

    /** Allows paths like `_.collection.everyItem.field` when selecting the target fields to override in Chimney DSL.
      *
      * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
      *
      * @return
      *   stubs an item extracted from the collection
      *
      * @since 1.0.0
      */
    @compileTimeOnly(".everyItem should be only called within Chimney DSL")
    def everyItem(implicit @unused ev: IsCollection.Of[C[I], I]): I = sys.error("")
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
  implicit final class TransformationMapPathOps[M[_, _], K, V](@unused private val cc: M[K, V]) extends AnyVal {

    /** Allows paths like `_.map.everyMapKey.field` when selecting the target fields to override in Chimney DSL.
      *
      * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
      *
      * @return
      *   stubs a key extracted from the map
      *
      * @since 1.0.0
      */
    @compileTimeOnly(".everyMapKey should be only called within Chimney DSL")
    def everyMapKey(implicit @unused ev: IsMap.Of[M[K, V], K, V]): K = sys.error("")

    /** Allows paths like `_.map.everyMapValue.field` when selecting the target fields to override in Chimney DSL.
      *
      * It can only be used within `.withField*` methods where the macros reads it and erases it from the final code!
      *
      * @return
      *   stubs a value extracted from the map
      *
      * @since 1.0.0
      */
    @compileTimeOnly(".everyMapValue should be only called within Chimney DSL")
    def everyMapValue(implicit @unused ev: IsMap.Of[M[K, V], K, V]): V = sys.error("")
  }

  // $COVERAGE-ON$
}
