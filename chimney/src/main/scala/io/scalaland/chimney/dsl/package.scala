package io.scalaland.chimney

import io.scalaland.chimney.internal.{PatcherCfg, TransformerCfg, TransformerFlags}

import scala.util.Try

/** Main object to import in order to use Chimney's features
  *
  * @since 0.1.0
  */
package object dsl {

  /** Provides transformer operations on values of any type.
    *
    * @param source wrapped source value
    * @tparam From type of source value
    *
    * @since 0.4.0
    */
  implicit class TransformerOps[From](private val source: From) extends AnyVal {

    /** Allows to customize transformer generation to your target type.
      *
      * @tparam To target type
      * @return [[io.scalaland.chimney.dsl.TransformerInto]]
      *
      * @since 0.1.0
      */
    final def into[To]: TransformerInto[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
      new TransformerInto(source, new TransformerDefinition(Map.empty, Map.empty))

    /** Performs in-place transformation of captured source value to target type.
      *
      * If you want to customize transformer behavior, consider using
      * [[io.scalaland.chimney.dsl.TransformerOps#into]] method.
      *
      * @see [[io.scalaland.chimney.Transformer#derive]] for default implicit instance
      * @param transformer implicit instance of [[io.scalaland.chimney.Transformer]] type class
      * @tparam To target type
      * @return transformed value of target type `To`
      *
      * @since 0.1.0
      */
    final def transformInto[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }

  /** Provides partial transformer operations on values of any type.
    *
    * @param source wrapped source value
    * @tparam From type of source value
    *
    * @since 0.7.0
    */
  implicit class PartialTransformerOps[From](private val source: From) extends AnyVal {

    /** Allows to customize partial transformer generation to your target type.
      *
      * @tparam To target success type
      * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
      *
      * @since 0.7.0
      */
    final def intoPartial[To]: PartialTransformerInto[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
      new PartialTransformerInto(source, new PartialTransformerDefinition(Map.empty, Map.empty))

    /** Performs in-place partial transformation of captured source value to target type.
      *
      * If you want to customize transformer behavior, consider using
      * [[io.scalaland.chimney.dsl.PartialTransformerOps#intoPartial]] method.
      *
      * @see [[io.scalaland.chimney.PartialTransformer#derive]] for default implicit instance
      * @param transformer implicit instance of [[io.scalaland.chimney.Transformer]] type class
      * @tparam To result target type of partial transformation
      * @return partial transformation result value of target type `To`
      *
      * @since 0.7.0
      */
    final def transformIntoPartial[To](
        implicit transformer: PartialTransformer[From, To]
    ): partial.Result[To] =
      transformIntoPartial(failFast = false)

    final def transformIntoPartial[To](failFast: Boolean)(
        implicit transformer: PartialTransformer[From, To]
    ): partial.Result[To] =
      transformer.transform(source, failFast)
  }

  /** Provides lifted transformer operations on values of any type.
    *
    * @param source wrapped source value
    * @tparam From type of source value
    *
    * @since 0.5.0
    */
  implicit class TransformerFOps[From](private val source: From) extends AnyVal {

    /** Allows to customize wrapped transformer generation to your target type.
      *
      * @tparam F  wrapper type constructor
      * @tparam To target type
      * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
      *
      * @since 0.5.0
      */
    @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "0.7.0")
    final def intoF[F[+_], To]
        : TransformerFInto[F, From, To, TransformerCfg.WrapperType[F, TransformerCfg.Empty], TransformerFlags.Default] =
      new TransformerFInto(source, new TransformerFDefinition(Map.empty, Map.empty))

    /** Performs in-place lifted transformation of captured source value to target type.
      *
      * If you want to customize transformer behavior, consider using
      * [[io.scalaland.chimney.dsl.TransformerFOps#intoF]] method.
      *
      * @see [[io.scalaland.chimney.TransformerF#derive]] for default implicit instance
      * @param transformer implicit instance of [[io.scalaland.chimney.TransformerF]] type class
      * @tparam To target type
      * @return transformed wrapped target value of type `F[To]`
      *
      * @since 0.5.0
      */
    @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "0.7.0")
    final def transformIntoF[F[+_], To](implicit transformer: TransformerF[F, From, To]): F[To] =
      transformer.transform(source)
  }

  /** Provides patcher operations on values of any type
    *
    * @param obj wrapped object to patch
    * @tparam T type of object to patch
    *
    * @since 0.1.3
    */
  implicit class PatcherOps[T](private val obj: T) extends AnyVal {

    /** Allows to customize patcher generation
      *
      * @param patch patch object value
      * @tparam P type of patch object
      * @return [[io.scalaland.chimney.dsl.PatcherUsing]]
      *
      * @since 0.4.0
      */
    final def using[P](patch: P): PatcherUsing[T, P, PatcherCfg.Empty] =
      new PatcherUsing[T, P, PatcherCfg.Empty](obj, patch)

    /** Performs in-place patching of wrapped object with provided value.
      *
      * If you want to customize patching behavior, consider using
      * [[io.scalaland.chimney.dsl.PatcherOps#using using]] method.
      *
      * @see [[io.scalaland.chimney.Patcher#derive]] for default implicit instance
      * @param patch patch object value
      * @param patcher implicit instance of [[io.scalaland.chimney.Patcher]] type class
      * @tparam P type of patch object
      * @return patched value
      *
      * @since 0.4.0
      */
    final def patchUsing[P](patch: P)(implicit patcher: Patcher[T, P]): T =
      patcher.patch(obj, patch)

    /** Performs in-place patching of wrapped object with provided value.
      *
      * If you want to customize patching behavior, consider using
      * [[io.scalaland.chimney.dsl.PatcherOps#using using]] method.
      *
      * @deprecated use [[io.scalaland.chimney.dsl.PatcherOps#patchUsing patchUsing]] instead
      * @see [[io.scalaland.chimney.Patcher#derive]] for default implicit instance
      * @param patch patch object value
      * @param patcher implicit instance of [[io.scalaland.chimney.Patcher]] type class
      * @tparam P type of patch object
      * @return patched value
      *
      * @since 0.1.3
      */
    @deprecated("please use .patchUsing", "0.4.0")
    final def patchWith[P](patch: P)(implicit patcher: Patcher[T, P]): T = {
      // $COVERAGE-OFF$
      obj.patchUsing(patch)
      // $COVERAGE-ON$
    }
  }

  /** Lifts [[scala.Option]] into [[io.scalaland.chimney.partial.Result]].
    *
    * @tparam T type of value inside Option
    * @param option value to convert
    *
    * @since 0.7.0
    */
  implicit class OptionPartialTransformerOps[T](private val option: Option[T]) extends AnyVal {

    /** Converts Option to Result, using EmptyValue error if None.
      *
      * @return successful result if [[scala.Some]], failed result with EmptyValue error if [[None]]
      *
      * @since 0.7.0
      */
    def toPartialResult: partial.Result[T] =
      partial.Result.fromOption(option)

    /** Converts Option to Result, using provided Errors if None.
      *
      * @param ifEmpty lazy failed result for [[scala.None]]
      * @return successful result if [[scala.Some]], provided failed result if [[scala.None]]
      *
      * @since 0.7.0
      */
    def toPartialResultOrErrors(ifEmpty: => partial.Result.Errors): partial.Result[T] =
      partial.Result.fromOptionOrErrors(option, ifEmpty)

    /** Converts Option to Result, using provided Error if None.
      *
      * @param ifEmpty lazy error for [[scala.None]]
      * @return successful result if [[scala.Some]], failed result with provided error if [[scala.None]]
      *
      * @since 0.7.0
      */
    def toPartialResultOrError(ifEmpty: => partial.Error): partial.Result[T] =
      partial.Result.fromOptionOrError(option, ifEmpty)

    /** Converts Option to Result, using provided error message if None.
      *
      * @param ifEmpty lazy error message for [[scala.None]]
      * @return successful result if [[scala.Some]], failed result with provided error message if [[scala.None]]
      *
      * @since 0.7.0
      */
    def toPartialResultOrString(ifEmpty: => String): partial.Result[T] =
      partial.Result.fromOptionOrString(option, ifEmpty)

    /** Converts Option to Result, using provided Throwable if None.
      *
      * @param ifEmpty lazy error for [[scala.None]]
      * @return successful result if [[scala.Some]], failed result with provided Throwable if [[scala.None]]
      *
      * @since 0.7.0
      */
    def toPartialResultOrThrowable(ifEmpty: => Throwable): partial.Result[T] =
      partial.Result.fromOptionOrThrowable(option, ifEmpty)
  }

  /** Lifts [[scala.Either]] into [[io.scalaland.chimney.partial.Result]].
    *
    * @tparam T type of value inside Option
    * @param either value to convert
    *
    * @since 0.7.0
    */
  implicit final class EitherStringPartialTransformerOps[T](private val either: Either[String, T]) extends AnyVal {

    /** Converts Either to Result, using an error message from Left as failed result.
      *
      * @return successful result if [[scala.Right]], failed result with an error message if [[scala.Left]]
      *
      * @since 0.7.0
      */
    def toPartialResult: partial.Result[T] =
      partial.Result.fromEitherString(either)
  }

  /** Lifts [[scala.util.Try]] into [[io.scalaland.chimney.partial.Result]].
    *
    * @tparam T type of value inside Option
    * @param `try` value to convert
    *
    * @since 0.7.0
    */
  implicit final class TryPartialTransformerOps[T](private val `try`: Try[T]) extends AnyVal {

    /** Converts Try to Result, using Throwable from Failure as failed result.
      *
      * @return successful result if [[scala.util.Success]], failed result with Throwable if [[scala.util.Failure]]
      *
      * @since 0.7.0
      */
    def toPartialResult: partial.Result[T] =
      partial.Result.fromTry(`try`)
  }
}
