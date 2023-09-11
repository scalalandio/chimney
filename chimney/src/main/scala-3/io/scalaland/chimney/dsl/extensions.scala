package io.scalaland.chimney.dsl

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.runtime.{PatcherCfg, PatcherFlags, TransformerCfg, TransformerFlags}
import io.scalaland.chimney.partial

import scala.util.Try

/** Provides transformer operations on values of any type.
  *
  * @tparam From type of source value
  * @param source wrapped source value
  *
  * @since 0.4.0
  */
extension [From](source: From) {

  /** Allows to customize transformer generation to your target type.
    *
    * @tparam To target type
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.0
    */
  transparent inline def into[To]: TransformerInto[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new TransformerInto(source, new TransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore))

  /** Performs in-place transformation of captured source value to target type.
    *
    * If you want to customize transformer behavior, consider using
    * [[io.scalaland.chimney.dsl.TransformerOps#into]] method.
    *
    * @see [[io.scalaland.chimney.Transformer#derive]] for default implicit instance
    * @tparam To target type
    * @param transformer implicit instance of [[io.scalaland.chimney.Transformer]] type class
    * @return transformed value of target type `To`
    *
    * @since 0.1.0
    */
  transparent inline def transformInto[To](implicit transformer: Transformer.AutoDerived[From, To]): To =
    transformer.transform(source)
}

/** Provides partial transformer operations on values of any type.
  *
  * @tparam From type of source value
  * @param source wrapped source value
  *
  * @since 0.7.0
  */
extension [From](source: From) {

  /** Allows to customize partial transformer generation to your target type.
    *
    * @tparam To target success type
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  transparent inline def intoPartial[To]
      : PartialTransformerInto[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new PartialTransformerInto(
      source,
      new PartialTransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore)
    )

  /** Performs in-place partial transformation of captured source value to target type.
    *
    * If you want to customize transformer behavior, consider using
    * [[io.scalaland.chimney.dsl.PartialTransformerOps#intoPartial]] method.
    *
    * @see [[io.scalaland.chimney.PartialTransformer#derive]] for default implicit instance
    * @tparam To result target type of partial transformation
    * @param transformer implicit instance of [[io.scalaland.chimney.Transformer]] type class
    * @return partial transformation result value of target type `To`
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
    * @see [[io.scalaland.chimney.PartialTransformer#derive]] for default implicit instance
    * @tparam To result target type of partial transformation
    * @param failFast    should fail as early as the first set of errors appear
    * @param transformer implicit instance of [[io.scalaland.chimney.Transformer]] type class
    * @return partial transformation result value of target type `To`
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
  * @param obj wrapped object to patch
  * @tparam T type of object to patch
  *
  * @since 0.1.3
  */
extension [T](obj: T) {

  /** Allows to customize patcher generation
    *
    * @tparam P type of patch object
    * @param patch patch object value
    * @return [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.4.0
    */
  transparent inline def using[P](patch: P): PatcherUsing[T, P, PatcherCfg.Empty, PatcherFlags.Default] =
    new PatcherUsing[T, P, PatcherCfg.Empty, PatcherFlags.Default](obj, patch)

  /** Performs in-place patching of wrapped object with provided value.
    *
    * If you want to customize patching behavior, consider using
    * [[io.scalaland.chimney.dsl.PatcherOps#using using]] method.
    *
    * @see [[io.scalaland.chimney.Patcher#derive]] for default implicit instance
    * @tparam P type of patch object
    * @param patch   patch object value
    * @param patcher implicit instance of [[io.scalaland.chimney.Patcher]] type class
    * @return patched value
    *
    * @since 0.4.0
    */
  transparent inline def patchUsing[P](patch: P)(implicit patcher: Patcher[T, P]): T =
    patcher.patch(obj, patch)
}

/** Lifts [[scala.Option]] into [[io.scalaland.chimney.partial.Result]].
  *
  * @tparam T type of value inside Option
  * @param option value to convert
  *
  * @since 0.7.0
  */
extension [T](option: Option[T]) {

  /** Converts Option to Result, using EmptyValue error if None.
    *
    * @return successful result if [[scala.Some]], failed result with EmptyValue error if [[None]]
    *
    * @since 0.7.0
    */
  transparent inline def toPartialResult: partial.Result[T] =
    partial.Result.fromOption(option)

  /** Converts Option to Result, using provided error message if None.
    *
    * @param ifEmpty lazy error message for [[scala.None]]
    * @return successful result if [[scala.Some]], failed result with provided error message if [[scala.None]]
    *
    * @since 0.7.0
    */
  transparent inline def toPartialResultOrString(ifEmpty: => String): partial.Result[T] =
    partial.Result.fromOptionOrString(option, ifEmpty)
}

/** Lifts [[scala.Either]] into [[io.scalaland.chimney.partial.Result]].
  *
  * @tparam T type of value inside Option
  * @param either value to convert
  *
  * @since 0.7.0
  */
extension [T](either: Either[String, T]) {

  /** Converts Either to Result, using an error message from Left as failed result.
    *
    * @return successful result if [[scala.Right]], failed result with an error message if [[scala.Left]]
    *
    * @since 0.7.0
    */
  transparent inline def toPartialResult: partial.Result[T] =
    partial.Result.fromEitherString(either)
}

/** Lifts [[scala.util.Try]] into [[io.scalaland.chimney.partial.Result]].
  *
  * @tparam T type of value inside Option
  * @param `try` value to convert
  *
  * @since 0.7.0
  */
extension [T](`try`: Try[T]) {

  /** Converts Try to Result, using Throwable from Failure as failed result.
    *
    * @return successful result if [[scala.util.Success]], failed result with Throwable if [[scala.util.Failure]]
    *
    * @since 0.7.0
    */
  transparent inline def toPartialResult: partial.Result[T] =
    partial.Result.fromTry(`try`)
}
