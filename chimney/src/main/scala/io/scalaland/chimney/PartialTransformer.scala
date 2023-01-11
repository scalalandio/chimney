package io.scalaland.chimney

import io.scalaland.chimney.dsl.PartialTransformerDefinition
import io.scalaland.chimney.internal.macros.dsl.TransformerBlackboxMacros
import io.scalaland.chimney.internal.{ErrorsCollection, TransformerCfg, TransformerFlags}

import scala.collection.compat._
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

  final def transform(src: From): PartialTransformer.Result[To] =
    transform(src, failFast = false)

  final def transformFailFast(src: From): PartialTransformer.Result[To] =
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
    final def errors: ErrorsCollection = this match {
      case _: Result.Value[_]              => ErrorsCollection.empty
      case Result.Errors(errorsCollection) => errorsCollection
    }
    final def asOption: Option[T] = this match {
      case Result.Value(value) => Some(value)
      case Result.Errors(_)    => None
    }
    final def asEither: Either[Result.Errors, T] = this match {
      case Result.Value(value)   => Right(value)
      case errors: Result.Errors => Left(errors)
    }
    final def asErrorPathMessagesStrings: Iterable[(String, String)] = this match {
      case _: Result.Value[_]    => Iterable.empty
      case errors: Result.Errors => errors.asErrorPathMessageStrings
    }
    final def map[U](f: T => U): Result[U] = this match {
      case Result.Value(value) => Result.Value(f(value))
      case _: Result.Errors    => this.asInstanceOf[Result[U]]
    }
    final def flatMap[U](f: T => Result[U]): Result[U] = this match {
      case Result.Value(value) => f(value)
      case _: Result.Errors    => this.asInstanceOf[Result[U]]
    }
    final def prependErrorPath(pathElement: => PathElement): this.type = this match {
      case _: Result.Value[_] => this
      case e: Result.Errors   => e.prependPath(pathElement).asInstanceOf[this.type]
    }
  }
  object Result {
    final case class Value[T](value: T) extends Result[T]
    final case class Errors(private val ec: ErrorsCollection) extends Result[Nothing] {
      def this(errors: Iterable[Error]) = this(ErrorsCollection.fromIterable(errors))
      def this(error: Error) = this(ErrorsCollection.fromSingle(error))
      def prependPath(pathElement: PathElement): Errors = Errors(ec.prependPath(pathElement))
      def asErrorPathMessageStrings: Iterable[(String, String)] = ec.map(_.asErrorPathMessageString)
    }
    object Errors {
      final def apply(errors: Iterable[Error]): Errors = new Errors(errors)
      final def single(error: Error): Errors = new Errors(error)
      final def fromString(message: String): Errors = new Errors(Error.ofString(message))
      final def fromStrings(messages: Iterable[String]): Errors = Errors(messages.map(Error.ofString))
      final def merge(errors1: Errors, errors2: Errors): Errors = Errors(errors1.ec ++ errors2.ec)
    }

    final def fromFunction[U, T](f: U => T): U => Result[T] = { u =>
      Result.fromCatching(f(u))
    }

    final def fromPartialFunction[U, T](pf: PartialFunction[U, T]): U => Result[T] = { u =>
      if (pf.isDefinedAt(u)) {
        Result.fromCatching(pf(u))
      } else {
        Errors.single(Error.ofNotDefinedAt(u))
      }
    }

    final def fromValue[T](value: T): Result[T] = Value(value)
    final def fromEmpty[T]: Result[T] = Errors.single(Error.ofEmptyValue)
    final def fromError[T](error: Error): Result[T] = Errors.single(error)
    final def fromErrors[T](errors: Iterable[Error]): Result[T] = Errors(errors)
    final def fromErrorString[T](message: String): Result[T] = Errors.fromString(message)
    final def fromErrorStrings[T](messages: Iterable[String]): Result[T] = Errors.fromStrings(messages)
    final def fromErrorNotDefinedAt[T](value: Any): Result[T] = Errors.single(Error.ofNotDefinedAt(value))
    final def fromErrorThrowable[T](throwable: Throwable): Result[T] = Errors.single(Error.ofThrowable(throwable))

    final def fromOption[T](value: Option[T]): Result[T] = value match {
      case Some(value) => fromValue(value)
      case _           => fromEmpty[T]
    }
    final def fromOptionOrErrors[T](value: Option[T], ifEmpty: => Errors): Result[T] = value match {
      case Some(value) => fromValue(value)
      case _           => ifEmpty
    }
    final def fromOptionOrError[T](value: Option[T], ifEmpty: => Error): Result[T] = value match {
      case Some(value) => fromValue(value)
      case _           => fromError(ifEmpty)
    }
    final def fromOptionOrString[T](value: Option[T], ifEmpty: => String): Result[T] = value match {
      case Some(value) => fromValue(value)
      case _           => fromErrorString(ifEmpty)
    }
    final def fromOptionOrStrings[T](value: Option[T], ifEmpty: => Iterable[String]): Result[T] = value match {
      case Some(value) => fromValue(value)
      case _           => fromErrorStrings(ifEmpty)
    }
    final def fromOptionOrThrowable[T](value: Option[T], ifEmpty: => Throwable): Result[T] = value match {
      case Some(value) => fromValue(value)
      case _           => fromErrorThrowable(ifEmpty)
    }
    final def fromEither[T](value: Either[Errors, T]): Result[T] = value match {
      case Right(value)         => fromValue(value)
      case Left(errors: Errors) => errors
    }
    final def fromEitherString[T](value: Either[String, T]): Result[T] = {
      fromEither(value.left.map(Errors.fromString))
    }
    final def fromEitherStrings[T](value: Either[Iterable[String], T]): Result[T] = {
      fromEither(value.left.map(errs => Errors.fromStrings(errs)))
    }
    final def fromTry[T](value: Try[T]): Result[T] = value match {
      case Success(value)     => fromValue(value)
      case Failure(throwable) => fromErrorThrowable(throwable)
    }
    final def fromCatching[T](value: => T): Result[T] = {
      try {
        fromValue(value)
      } catch {
        case t: Throwable => fromErrorThrowable(t)
      }
    }
    // extensions can provide more integrations, i.e. for Validated, Ior, etc.

    final def traverse[M, A, B](it: Iterator[A], f: A => Result[B], failFast: Boolean)(
        implicit fac: Factory[B, M]
    ): Result[M] = {
      val bs = fac.newBuilder
      // possible to call only on 2.13+
      // bs.sizeHint(it)

      if (failFast) {
        var errors: Errors = null
        while (errors == null && it.hasNext) {
          f(it.next()) match {
            case Value(value) => bs += value
            case e: Errors    => errors = e
          }
        }
        if (errors == null) Result.Value(bs.result()) else errors
      } else {
        var allErrors: ErrorsCollection = ErrorsCollection.empty
        while (it.hasNext) {
          f(it.next()) match {
            case Value(value) => bs += value
            case Errors(ee)   => allErrors ++= ee
          }
        }
        if (allErrors.isEmpty) Result.Value(bs.result()) else Result.Errors(allErrors)
      }
    }

    final def sequence[M, A](it: Iterator[Result[A]], failFast: Boolean)(implicit fac: Factory[A, M]): Result[M] = {
      traverse(it, identity[Result[A]], failFast)
    }

    final def map2[A, B, C](resultA: Result[A], resultB: => Result[B], failFast: Boolean, f: (A, B) => C): Result[C] = {
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

    final def product[A, B](res1: Result[A], res2: => Result[B], failFast: Boolean): Result[(A, B)] =
      map2(res1, res2, failFast, (x: A, y: B) => (x, y))
  }

  final case class Error(message: ErrorMessage, path: ErrorPath = ErrorPath.Empty) {
    def asErrorPathMessageString: (String, String) = (path.asString, message.asString)
    def prependErrorPath(pathElement: PathElement): Error = Error(message, path.prepend(pathElement))
  }
  object Error {
    final def ofEmptyValue: Error =
      Error(ErrorMessage.EmptyValue)
    final def ofNotDefinedAt(value: Any): Error =
      Error(ErrorMessage.NotDefinedAt(value))
    final def ofString(message: String): Error =
      Error(ErrorMessage.StringMessage(message))
    final def ofThrowable(throwable: Throwable): Error =
      Error(ErrorMessage.ThrowableMessage(throwable))
  }

  sealed trait ErrorMessage {
    final def asString: String = this match {
      case ErrorMessage.EmptyValue                  => "empty value"
      case ErrorMessage.NotDefinedAt(value)         => s"not defined at $value"
      case ErrorMessage.StringMessage(message)      => message
      case ErrorMessage.ThrowableMessage(throwable) => throwable.getMessage
    }
  }
  object ErrorMessage {
    final case object EmptyValue extends ErrorMessage
    final case class NotDefinedAt(value: Any) extends ErrorMessage
    final case class StringMessage(message: String) extends ErrorMessage
    final case class ThrowableMessage(throwable: Throwable) extends ErrorMessage
  }

  final case class ErrorPath(private val elems: List[PathElement]) extends AnyVal {
    def prepend(pathElement: PathElement): ErrorPath = ErrorPath(pathElement :: elems)
    def asString: String = {
      if (elems.isEmpty) ""
      else {
        val sb = new StringBuilder
        val it = elems.iterator
        while (it.hasNext) {
          val curr = it.next()
          if (sb.nonEmpty && PathElement.shouldPrependWithDot(curr)) {
            sb += '.'
          }
          sb ++= curr.asString
        }
        sb.result()
      }
    }
  }
  object ErrorPath {
    final val Empty: ErrorPath = ErrorPath(Nil)
  }

  sealed trait PathElement { def asString: String }
  object PathElement {
    final case class Accessor(name: String) extends PathElement { override def asString: String = name }
    final case class Index(index: Int) extends PathElement { override def asString: String = s"($index)" }
    final case class MapValue(key: Any) extends PathElement { override def asString: String = s"($key)" }
    final case class MapKey(key: Any) extends PathElement { override def asString: String = s"keys($key)" }

    final def shouldPrependWithDot(pe: PathElement): Boolean = pe match {
      case _: Accessor => true
      case _: Index    => false
      case _: MapValue => false
      case _: MapKey   => true
    }
  }

}
