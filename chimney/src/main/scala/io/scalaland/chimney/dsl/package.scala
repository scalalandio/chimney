package io.scalaland.chimney

import io.scalaland.chimney.internal.{PatcherCfg, TransformerCfg, TransformerFlags}

import scala.util.Try

/** Main object to import in order to use Chimney's features
  */
package object dsl {

  /** Provides transformer operations on values of any type.
    *
    * @param source wrapped source value
    * @tparam From type of source value
    */
  implicit class TransformerOps[From](private val source: From) extends AnyVal {

    /** Allows to customize transformer generation to your target type.
      *
      * @tparam To target type
      * @return [[io.scalaland.chimney.dsl.TransformerInto]]
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
      */
    final def transformInto[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }

  /** Provides partial transformer operations on values of any type.
    *
    * @param source wrapped source value
    * @tparam From type of source value
    */
  implicit class PartialTransformerOps[From](private val source: From) extends AnyVal {

    /** Allows to customize partial transformer generation to your target type.
      *
      * @tparam To target success type
      * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
      */
    final def intoPartial[To]: PartialTransformerInto[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
      new PartialTransformerInto(source, new PartialTransformerDefinition(Map.empty, Map.empty))

    /** Performs in-place partial transformation of captured source value to target type.
      *
      * If you want to customize transformer behavior, consider using
      * [[io.scalaland.chimney.dsl.PartialTransformerOps#intoPartially]] method.
      *
      * @see [[io.scalaland.chimney.PartialTransformer#derive]] for default implicit instance
      * @param transformer implicit instance of [[io.scalaland.chimney.Transformer]] type class
      * @tparam To result target type of partial transformation
      * @return partial transformation result value of target type `To`
      */
    final def transformIntoPartial[To](
        implicit transformer: PartialTransformer[From, To]
    ): PartialTransformer.Result[To] =
      transformIntoPartial(failFast = false)

    final def transformIntoPartial[To](failFast: Boolean)(
        implicit transformer: PartialTransformer[From, To]
    ): PartialTransformer.Result[To] =
      transformer.transform(source, failFast)
  }

  /** Provides lifted transformer operations on values of any type.
    *
    * @param source wrapped source value
    * @tparam From type of source value
    */
  implicit class TransformerFOps[From](private val source: From) extends AnyVal {

    /** Allows to customize wrapped transformer generation to your target type.
      *
      * @tparam F  wrapper type constructor
      * @tparam To target type
      * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
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
      */
    @deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "0.7.0")
    final def transformIntoF[F[+_], To](implicit transformer: TransformerF[F, From, To]): F[To] =
      transformer.transform(source)
  }

  /** Provides patcher operations on values of any type
    *
    * @param obj wrapped object to patch
    * @tparam T type of object to patch
    */
  implicit class PatcherOps[T](private val obj: T) extends AnyVal {

    /** Allows to customize patcher generation
      *
      * @param patch patch object value
      * @tparam P type of patch object
      * @return [[io.scalaland.chimney.dsl.PatcherUsing]]
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
      */
    @deprecated("please use .patchUsing", "0.4.0")
    final def patchWith[P](patch: P)(implicit patcher: Patcher[T, P]): T = {
      // $COVERAGE-OFF$
      obj.patchUsing(patch)
      // $COVERAGE-ON$
    }
  }

  implicit class OptionPartialTransformerOps[T](private val option: Option[T]) extends AnyVal {
    def toPartialTransformerResult: PartialTransformer.Result[T] =
      PartialTransformer.Result.fromOption(option)
    def toPartialTransformerResultOrErrors(ifEmpty: => PartialTransformer.Result.Errors): PartialTransformer.Result[T] =
      PartialTransformer.Result.fromOptionOrErrors(option, ifEmpty)
    def toPartialTransformerResultOrError(ifEmpty: => PartialTransformer.Error): PartialTransformer.Result[T] =
      PartialTransformer.Result.fromOptionOrError(option, ifEmpty)
    def toPartialTransformerResultOrString(ifEmpty: => String): PartialTransformer.Result[T] =
      PartialTransformer.Result.fromOptionOrString(option, ifEmpty)
//    def toPartialTransformerResultOrStrings(ifEmpty: => Seq[String]): PartialTransformer.Result[T] =
//      PartialTransformer.Result.fromOptionOrStrings(option, ifEmpty)
    def toPartialTransformerResultOrThrowable(ifEmpty: => Throwable): PartialTransformer.Result[T] =
      PartialTransformer.Result.fromOptionOrThrowable(option, ifEmpty)
  }

  implicit final class EitherStringPartialTransformerOps[T](private val either: Either[String, T]) extends AnyVal {
    def toPartialTransformerResult: PartialTransformer.Result[T] =
      PartialTransformer.Result.fromEitherString(either)
  }

  implicit final class EitherStringsPartialTransformerOps[T](private val either: Either[Iterable[String], T])
      extends AnyVal {
    def toPartialTransformerResult: PartialTransformer.Result[T] =
      PartialTransformer.Result.fromEitherStrings(either)
  }

  implicit final class TryPartialTransformerOps[T](private val `try`: Try[T]) extends AnyVal {
    def toPartialTransformerResult: PartialTransformer.Result[T] =
      PartialTransformer.Result.fromTry(`try`)
  }
}
