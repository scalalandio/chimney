package io.scalaland.chimney

import io.scalaland.chimney.dsl.PartialTransformerDefinition
import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros
import io.scalaland.chimney.internal.{TransformerCfg, TransformerFlags}

import scala.collection.compat.Factory
import scala.language.experimental.macros
import scala.util.{Failure, Success, Try}

/** Type class expressing partial transformation between
  * source type `From` and target type `To`, with the ability
  * of reporting transformation error
  *
  * @tparam From type of input value
  * @tparam To   type of output value
  */
trait PartialTransformer[From, To] { self =>

  def transform(src: From, failFast: Boolean): PartialTransformer.Result[To]

  def transform(src: From): PartialTransformer.Result[To] =
    transform(src, failFast = false)

  def transformFailFast(src: From): PartialTransformer.Result[To] =
    transform(src, failFast = true)
}

object PartialTransformer {

  def apply[A, B](f: A => PartialTransformer.Result[B]): PartialTransformer[A, B] =
    (src: A, _: Boolean) => f(src)

  /** Provides [[io.scalaland.chimney.PartialTransformer]] derived with the default settings.
    *
    * When transformation can't be derived, it results with compilation error.
    *
    * @tparam From type of input value
    * @tparam To type of output value
    * @return [[io.scalaland.chimney.PartialTransformer]] type class definition
    */
  implicit def derive[From, To]: PartialTransformer[From, To] =
    macro TransformerBlackboxMacros.derivePartialTransformerImpl[From, To]

  /** Creates an empty [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] that
    * you can customize to derive [[io.scalaland.chimney.PartialTransformer]].
    *
    * @see [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] for available settings
    *
    * @tparam From type of input value
    * @tparam To type of output value
    * @return [[io.scalaland.chimney.dsl.PartialTransformerDefinition]] with defaults
    */
  def define[From, To]: PartialTransformerDefinition[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new PartialTransformerDefinition(Map.empty, Map.empty)

  sealed trait Result[+T] {
    def errors: Iterable[Error]

    def asOption: Option[T] = this match {
      case Result.Value(value) => Some(value)
      case Result.Errors(_)    => None
    }
    def asEither: Either[Result.Errors, T] = this match {
      case Result.Value(value)       => Right(value)
      case errors @ Result.Errors(_) => Left(errors)
    }
    def asErrorPathMessagesStrings: Iterable[(String, String)] = this match {
      case Result.Value(_)       => Iterable.empty
      case errors: Result.Errors => errors.asErrorPathMessageStrings
    }
    def map[U](f: T => U): Result[U] = {
      this match {
        case Result.Value(value) => Result.Value(f(value))
        case errs: Result.Errors => errs
      }
    }
    def flatMap[U](f: T => Result[U]): Result[U] = {
      this match {
        case Result.Value(value) => f(value)
        case errs: Result.Errors => errs
      }
    }
    def wrapErrorPaths(pathWrapper: ErrorPath => ErrorPath): this.type = this match {
//    def wrapErrorPaths(pathWrapper: ErrorPath => ErrorPath): Result[T] = this match {
      case Result.Value(_)       => this
      case Result.Errors(errors) => Result.Errors(errors.map(_.wrapErrorPath(pathWrapper))).asInstanceOf[this.type]
    }
  }
  object Result {
    case class Value[T](value: T) extends Result[T] {
      def errors: Iterable[Error] = Iterable.empty
    }
    case class Errors(errors: Iterable[Error]) extends Result[Nothing] {
      def asErrorPathMessageStrings: Iterable[(String, String)] = {
        errors.map(_.asErrorPathMessageString)
      }
    }
    object Errors {
      def single(error: Error): Errors = Errors(Iterable(error))
      def fromString(message: String): Errors = single(Error.ofString(message))
      def fromStrings(messages: Iterable[String]): Errors = Errors(messages.map(Error.ofString))
    }

    def fromFunction[U, T](f: U => T): U => Result[T] = { u =>
      Result.fromCatching(f(u))
    }

    def fromPartialFunction[U, T](pf: PartialFunction[U, T]): U => Result[T] = { u =>
      if (pf.isDefinedAt(u)) {
        Result.fromCatching(pf(u))
      } else {
        Errors.single(Error.ofNotDefinedAt(u)) // TODO?
      }
    }

    def fromValue[T](value: T): Result[T] = Value(value)
    def fromEmpty[T]: Result[T] = Errors.single(Error.ofEmptyValue)
    def fromError[T](error: Error): Result[T] = Errors.single(error)
    def fromErrors[T](errors: Iterable[Error]): Result[T] = Errors(errors)
    def fromErrorString[T](message: String): Result[T] = Errors.fromString(message)
    def fromErrorStrings[T](messages: Iterable[String]): Result[T] = Errors.fromStrings(messages)
    def fromErrorNotDefinedAt[T](value: Any): Result[T] = Errors.single(Error.ofNotDefinedAt(value))
    def fromErrorThrowable[T](throwable: Throwable): Result[T] = Errors.single(Error.ofThrowable(throwable))

    def fromOption[T](value: Option[T]): Result[T] = value match {
      case Some(value) => fromValue(value)
      case None        => fromEmpty[T]
    }
    def fromOptionOrErrors[T](value: Option[T], ifEmpty: => Errors): Result[T] = value match {
      case Some(value) => fromValue(value)
      case None        => fromErrors(ifEmpty.errors)
    }
    def fromOptionOrError[T](value: Option[T], ifEmpty: => Error): Result[T] = value match {
      case Some(value) => fromValue(value)
      case None        => fromError(ifEmpty)
    }
    def fromOptionOrString[T](value: Option[T], ifEmpty: => String): Result[T] = value match {
      case Some(value) => fromValue(value)
      case None        => fromErrorString(ifEmpty)
    }
    def fromOptionOrStrings[T](value: Option[T], ifEmpty: => Iterable[String]): Result[T] = value match {
      case Some(value) => fromValue(value)
      case None        => fromErrorStrings(ifEmpty)
    }
    def fromOptionOrThrowable[T](value: Option[T], ifEmpty: => Throwable): Result[T] = value match {
      case Some(value) => fromValue(value)
      case None        => fromErrorThrowable(ifEmpty)
    }
    def fromEither[T](value: Either[Errors, T]): Result[T] = value match {
      case Left(Errors(errors)) => fromErrors(errors)
      case Right(value)         => fromValue(value)
    }
    def fromEitherString[T](value: Either[String, T]): Result[T] = {
      fromEither(value.left.map(Errors.fromString))
    }
    def fromEitherStrings[T](value: Either[Iterable[String], T]): Result[T] = {
      fromEither(value.left.map(errs => Errors.fromStrings(errs)))
    }
    def fromTry[T](value: Try[T]): Result[T] = value match {
      case Failure(throwable) => fromErrorThrowable(throwable)
      case Success(value)     => fromValue(value)
    }
    def fromCatching[T](value: => T): Result[T] = {
      try {
        fromValue(value)
      } catch {
        case t: Throwable => fromErrorThrowable(t)
      }
    }
    // extensions can provide more integrations, i.e. for Validated, Ior, etc.

    def traverse[M, A, B](it: Iterator[A], f: A => Result[B], failFast: Boolean)(
        implicit fac: Factory[B, M]
    ): Result[M] = {
      val b = fac.newBuilder
      if (failFast) {
        var errors: Vector[Error] = null
        while (errors == null && it.hasNext) {
          f(it.next()) match {
            case Value(value) => b += value
            case Errors(ee)   => errors = ee.toVector
          }
        }
        if (errors == null) Result.Value(b.result()) else Result.Errors(errors)
      } else {
        var allErrors: Vector[Error] = Vector.empty
        while (it.hasNext) {
          f(it.next()) match {
            case Value(value) => b += value
            case Errors(ee)   => allErrors ++= ee
          }
        }
        if (allErrors.isEmpty) Result.Value(b.result()) else Result.Errors(allErrors)
      }
    }

    def sequence[M, A](it: Iterator[Result[A]], failFast: Boolean)(implicit fac: Factory[A, M]): Result[M] = {
      traverse(it, identity[Result[A]], failFast)
    }

    def map2[A, B, C](resultA: Result[A], resultB: => Result[B], failFast: Boolean, f: (A, B) => C): Result[C] = {
      if (failFast) {
        resultA match {
          case Value(a) =>
            resultB match {
              case Value(b)       => Value(f(a, b))
              case Errors(errors) => Errors(errors)
            }
          case Errors(errors) => Errors(errors)
        }
      } else {
        (resultA, resultB) match {
          case (Value(a), Value(b)) => Value(f(a, b))
          case (otherA, otherB)     => Errors(otherA.errors ++ otherB.errors)
        }
      }
    }

    def product[A, B](res1: Result[A], res2: => Result[B], failFast: Boolean): Result[(A, B)] =
      map2(res1, res2, failFast, (x: A, y: B) => (x, y))
  }

  case class Error(message: ErrorMessage, path: ErrorPath = ErrorPath.Empty) {
    def asErrorPathMessageString: (String, String) = (path.asString, message.asString)
    def wrapErrorPath(pathWrapper: ErrorPath => ErrorPath): Error = {
      copy(path = pathWrapper(path))
    }
  }
  object Error {
    def ofEmptyValue: Error =
      Error(ErrorMessage.EmptyValue)
    def ofNotDefinedAt(value: Any): Error =
      Error(ErrorMessage.NotDefinedAt(value))
    def ofString(message: String): Error =
      Error(ErrorMessage.StringMessage(message))
    def ofThrowable(throwable: Throwable): Error =
      Error(ErrorMessage.ThrowableMessage(throwable))
  }

  sealed trait ErrorMessage {
    def asString: String = this match {
      case ErrorMessage.EmptyValue                  => "empty value"
      case ErrorMessage.NotDefinedAt(value)         => s"not defined at $value"
      case ErrorMessage.StringMessage(message)      => message
      case ErrorMessage.ThrowableMessage(throwable) => throwable.getMessage
    }
  }
  object ErrorMessage {
    case object EmptyValue extends ErrorMessage
    case class NotDefinedAt(value: Any) extends ErrorMessage
    case class StringMessage(message: String) extends ErrorMessage
    case class ThrowableMessage(throwable: Throwable) extends ErrorMessage
  }

  sealed trait ErrorPath {
    def asString: String = this match {
      case ErrorPath.Empty                                  => ""
      case ErrorPath.Accessor(name, ErrorPath.Empty)        => s"$name"
      case ErrorPath.Accessor(name, idx: ErrorPath.Index)   => s"$name${idx.asString}"
      case ErrorPath.Accessor(name, mv: ErrorPath.MapValue) => s"$name${mv.asString}"
      case ErrorPath.Accessor(name, nested)                 => s"$name.${nested.asString}"
      case ErrorPath.Index(index, nested)                   => s"($index).${nested.asString}"
      case ErrorPath.MapValue(key, ErrorPath.Empty)         => s"($key)"
      case ErrorPath.MapValue(key, nested)                  => s"($key).${nested.asString}"
      case ErrorPath.MapKey(key, ErrorPath.Empty)           => s"keys($key)"
      case ErrorPath.MapKey(key, nested)                    => s"keys($key).${nested.asString}"
    }
  }
  object ErrorPath {
    case object Empty extends ErrorPath
    case class Accessor(name: String, nested: ErrorPath) extends ErrorPath
    case class Index(index: Int, nested: ErrorPath) extends ErrorPath
    case class MapValue(key: Any, nested: ErrorPath) extends ErrorPath
    case class MapKey(key: Any, nested: ErrorPath) extends ErrorPath
  }

}
