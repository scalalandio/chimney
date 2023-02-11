package io.scalaland.chimney.partial

import io.scalaland.chimney.internal.ErrorsCollection

import scala.collection.compat._
import scala.util.{Failure, Success, Try}

sealed trait Result[+T] {
  final def asOption: Option[T] = this match {
    case Result.Value(value) => Some(value)
    case Result.Errors(_)    => None
  }

  final def asEither: Either[Result.Errors, T] = this match {
    case Result.Value(value)   => Right(value)
    case errors: Result.Errors => Left(errors)
  }

  def asErrorPathMessages: Iterable[(String, ErrorMessage)]

  final def asErrorPathMessageStrings: Iterable[(String, String)] =
    this.asErrorPathMessages.map { case (path, message) => (path, message.asString) }

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
  final case class Value[T](value: T) extends Result[T] {
    def asErrorPathMessages: Iterable[(String, ErrorMessage)] = Iterable.empty
  }

  final case class Errors(private val ec: ErrorsCollection) extends Result[Nothing] {
    def errors: ErrorsCollection = ec

    def prependPath(pathElement: PathElement): Errors = Errors(ec.prependPath(pathElement))

    def asErrorPathMessages: Iterable[(String, ErrorMessage)] = ec.map(_.asErrorPathMessage)
  }

  object Errors {
    final def apply(error: Error, errors: Error*): Errors =
      apply(ErrorsCollection.from(error, errors: _*))

    final def single(error: Error): Errors =
      apply(ErrorsCollection.fromSingle(error))

    final def fromString(message: String): Errors =
      single(Error.ofString(message))

    final def fromStrings(message: String, messages: String*): Errors =
      apply(Error.ofString(message), messages.map(Error.ofString): _*)

    final def merge(errors1: Errors, errors2: Errors): Errors =
      apply(errors1.ec ++ errors2.ec)
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

  final def fromErrors[T](error: Error, errors: Error*): Result[T] = Errors(error, errors: _*)

  final def fromErrorString[T](message: String): Result[T] = Errors.fromString(message)

  final def fromErrorStrings[T](message: String, messages: String*): Result[T] =
    Errors.fromStrings(message, messages: _*)

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
      var allErrors: ErrorsCollection = null
      while (it.hasNext) {
        f(it.next()) match {
          case Value(value) => bs += value
          case Errors(ee) =>
            if (allErrors == null) allErrors = ee
            else allErrors ++= ee
        }
      }
      if (allErrors == null) Result.Value(bs.result()) else Result.Errors(allErrors)
    }
  }

  final def sequence[M, A](it: Iterator[Result[A]], failFast: Boolean)(implicit fac: Factory[A, M]): Result[M] = {
    traverse(it, identity[Result[A]], failFast)
  }

  final def map2[A, B, C](resultA: Result[A], resultB: => Result[B], f: (A, B) => C, failFast: Boolean): Result[C] = {
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
        case (Value(a), Value(b))           => Value(f(a, b))
        case (Errors(errs1), Errors(errs2)) => Errors(errs1 ++ errs2)
        case (errs1: Errors, _: Value[_])   => errs1
        case (_: Value[_], errs2: Errors)   => errs2
      }
    }
  }

  final def product[A, B](res1: Result[A], res2: => Result[B], failFast: Boolean): Result[(A, B)] =
    map2(res1, res2, (x: A, y: B) => (x, y), failFast)
}
