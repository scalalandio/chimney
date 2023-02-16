package io.scalaland.chimney.partial

import io.scalaland.chimney.internal.NonEmptyErrorsChain

import scala.collection.compat._
import scala.util.{Failure, Success, Try}

/** Data type representing either successfully computed value or collection of path-annotated errors.
  *
  * @tparam T type of success value
  *
  * @since 0.7.0
  */
sealed trait Result[+T] {

  /** Converts a partial result to an optional value.
    *
    * @return `Some` when has a success value, `None` when has errors
    *
    * @since 0.7.0
    */
  final def asOption: Option[T] = this match {
    case Result.Value(value) => Some(value)
    case Result.Errors(_)    => None
  }

  /** Converts a partial result to an `Either`.
    *
    * @return `Right` when has a success value, `Left` when has errors
    *
    * @since 0.7.0
    */
  final def asEither: Either[Result.Errors, T] = this match {
    case Result.Value(value)   => Right(value)
    case errors: Result.Errors => Left(errors)
  }

  /** Returns (possibly empty) collection of tuples with conventional string representation of path
    * and errors message.
    *
    * @since 0.7.0
    */
  def asErrorPathMessages: Iterable[(String, ErrorMessage)]

  /** Returns (possibly empty) collection of tuples with conventional string representation of path
    * and string representation of error message.
    *
    * @since 0.7.0
    */
  final def asErrorPathMessageStrings: Iterable[(String, String)] =
    this.asErrorPathMessages.map { case (path, message) => (path, message.asString) }

  /** Builds a new result by applying a function to a success value.
    *
    * @param f the function to apply to a success value
    * @tparam U the element type of the returned result
    * @return a new result built from applying a function to a success value
    *
    * @since 0.7.0
    */
  final def map[U](f: T => U): Result[U] = this match {
    case Result.Value(value) => Result.Value(f(value))
    case _: Result.Errors    => this.asInstanceOf[Result[U]]
  }

  /** Builds a new result by applying a function to a success value and using result returned by that that function.
    *
    * @param f the function to apply to a success value
    * @tparam U the element type of the returned result
    * @return a new result built from applying a function to a success value
    *         and using the result returned by that function
    *
    * @since 0.7.0
    */
  final def flatMap[U](f: T => Result[U]): Result[U] = this match {
    case Result.Value(value) => f(value)
    case _: Result.Errors    => this.asInstanceOf[Result[U]]
  }

  /** Prepends a path element to all errors represented by this result.
    *
    * @param pathElement path element to be prepended
    * @return a result with path element prepended to all errors
    *
    * @since 0.7.0
    */
  final def prependErrorPath(pathElement: => PathElement): this.type = this match {
    case _: Result.Value[_] => this
    case e: Result.Errors   => e.prependPath(pathElement).asInstanceOf[this.type]
  }
}

object Result {

  /** Success value case representation
    *
    * @param value value of type `T`
    * @tparam T type of success value
    *
    * @since 0.7.0
    */
  final case class Value[T](value: T) extends Result[T] {

    def asErrorPathMessages: Iterable[(String, ErrorMessage)] = Iterable.empty
  }

  /** Errors case representation
    *
    * @param errors non-empty collection of path-annotated errors
    *
    * @since 0.7.0
    * */
  final case class Errors(errors: NonEmptyErrorsChain) extends Result[Nothing] {

    def prependPath(pathElement: PathElement): Errors = Errors(errors.prependPath(pathElement))

    def asErrorPathMessages: Iterable[(String, ErrorMessage)] = errors.map(_.asErrorPathMessage)
  }

  object Errors {
    final def apply(error: Error, errors: Error*): Errors =
      apply(NonEmptyErrorsChain.from(error, errors: _*))

    final def single(error: Error): Errors =
      apply(NonEmptyErrorsChain.from(error))

    final def fromString(message: String): Errors =
      single(Error.ofString(message))

    final def fromStrings(message: String, messages: String*): Errors =
      apply(Error.ofString(message), messages.map(Error.ofString): _*)

    final def merge(errors1: Errors, errors2: Errors): Errors =
      apply(errors1.errors ++ errors2.errors)
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
      var allErrors: NonEmptyErrorsChain = null
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
