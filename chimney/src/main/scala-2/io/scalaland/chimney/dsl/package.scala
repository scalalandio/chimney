package io.scalaland.chimney

import scala.language.implicitConversions

/** Main object to import in order to use Chimney's features
  *
  * @since 0.1.0
  */
package object dsl {

  // export inlined.*

  implicit def TransformationOps[From](from: From): inlined.TransformationOps[From] =
    inlined.TransformationOps(from)

  implicit def PartialTransformationOps[From](from: From): inlined.PartialTransformationOps[From] =
    inlined.PartialTransformationOps(from)

  implicit def PatchingOps[A](obj: A): inlined.PatchingOps[A] =
    inlined.PatchingOps(obj)

  // export syntax.*

  implicit def TransformationMatchingPathOps[A](a: A): syntax.TransformationMatchingPathOps[A] =
    syntax.TransformationMatchingPathOps(a)

  implicit def TransformationCollectionPathOps[C[_], A](a: C[A]): syntax.TransformationCollectionPathOps[C, A] =
    syntax.TransformationCollectionPathOps(a)

  implicit def TransformationMapPathOps[M[_, _], K, V](a: M[K, V]): syntax.TransformationMapPathOps[M, K, V] =
    syntax.TransformationMapPathOps(a)

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
      *   [[io.scalaland.chimney.Transformer.AutoDerived#deriveAutomatic]] for default implicit instance
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
    final def transformInto[To](implicit transformer: Transformer.AutoDerived[From, To]): To =
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
      *   [[io.scalaland.chimney.PartialTransformer.AutoDerived#deriveAutomatic]] for default implicit instance
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
        transformer: PartialTransformer.AutoDerived[From, To]
    ): partial.Result[To] =
      transformIntoPartial(failFast = false)

    /** Performs in-place partial transformation of captured source value to target type.
      *
      * If you want to customize transformer behavior, consider using
      * [[io.scalaland.chimney.dsl.PartialTransformerOps#intoPartial]] method.
      *
      * @see
      *   [[io.scalaland.chimney.PartialTransformer.AutoDerived#deriveAutomatic]] for default implicit instance
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
        transformer: PartialTransformer.AutoDerived[From, To]
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
      *   [[io.scalaland.chimney.Patcher.AutoDerived#deriveAutomatic]] for default implicit instance
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
    final def patchUsing[Patch](patch: Patch)(implicit patcher: Patcher.AutoDerived[A, Patch]): A =
      patcher.patch(obj, patch)
  }
}
