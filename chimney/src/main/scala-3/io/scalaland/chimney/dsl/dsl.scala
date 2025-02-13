package io.scalaland.chimney.dsl

import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.TransformerMacros
import io.scalaland.chimney.internal.compiletime.derivation.patcher.PatcherMacros

export io.scalaland.chimney.inlined.{into, intoPartial, using}
export io.scalaland.chimney.syntax.{
  everyItem,
  everyMapKey,
  everyMapValue,
  matching,
  matchingLeft,
  matchingRight,
  matchingSome
}

// Extension methods in dsl.* summon TypeClass.AutoDerived while extension methods in syntax.* summon TypeClass.
// This helps us preserve legacy behavior in dsl code while keeping stricter separation in auto/syntax imports.

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
  transparent inline def transformInto[To](implicit transformer: Transformer.AutoDerived[From, To]): To =
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
    *   [[io.scalaland.chimney.PartialTransformer#deriveAutomatic]] for default implicit instance
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
      transformer: PartialTransformer.AutoDerived[From, To]
  ): partial.Result[To] =
    transformIntoPartial(failFast = false)

  /** Performs in-place partial transformation of captured source value to target type.
    *
    * If you want to customize transformer behavior, consider using
    * [[io.scalaland.chimney.dsl.PartialTransformerOps#intoPartial]] method.
    *
    * @see
    *   [[io.scalaland.chimney.PartialTransformer#deriveAutomatic]] for default implicit instance
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
      transformer: PartialTransformer.AutoDerived[From, To]
  ): partial.Result[To] =
    transformer.transform(source, failFast)
}

/** Provides patcher operations on values of any type
  *
  * @param obj
  *   wrapped object to patch
  * @tparam T
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
    *   [[io.scalaland.chimney.Patcher#deriveAutomatic]] for default implicit instance
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
  transparent inline def patchUsing[Patch](patch: Patch)(implicit patcher: Patcher.AutoDerived[A, Patch]): A =
    patcher.patch(obj, patch)
}
