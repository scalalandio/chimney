package io.scalaland.chimney.partial

import io.scalaland.chimney.internal.NonEmptyErrorsChain

import scala.collection.compat.*
import scala.util.{Failure, Success, Try}

/** Data type representing either successfully computed value or collection of path-annotated errors.
  *
  * @tparam A type of success value
  *
  * @since 0.7.0
  */
sealed trait Result[+A] {

  /** Converts a partial result to an optional value.
    *
    * @return [[scala.Some]] if success, [[scala.None]] otherwise
    *
    * @since 0.7.0
    */
  final def asOption: Option[A] = this match {
    case Result.Value(value) => Some(value)
    case Result.Errors(_)    => None
  }

  /** Converts a partial result to an [[scala.Either]].
    *
    * @return [[scala.Right]] if success,
    *         [[scala.Left]] containing [[io.scalaland.chimney.partial.Result.Errors]] otherwise
    *
    * @since 0.7.0
    */
  final def asEither: Either[Result.Errors, A] = this match {
    case Result.Value(value)   => Right(value)
    case errors: Result.Errors => Left(errors)
  }

  /** Returns (possibly empty) collection of tuples with conventional string representation of path
    * and errors message.
    *
    * @return empty [[scala.collection.Iterable]] if success,
    *         a sequence of pairs (a path to a failed field, an [[io.scalaland.chimney.partial.ErrorMessage]]) otherwise
    *
    * @since 0.7.0
    */
  def asErrorPathMessages: Iterable[(String, ErrorMessage)]

  /** Returns (possibly empty) collection of tuples with conventional string representation of path
    * and string representation of error message.
    *
    * @return empty [[scala.collection.Iterable]] if success,
    *         a sequence of pairs (a path to a failed field, an errors message as [[java.lang.String]]
    *
    * @since 0.7.0
    */
  final def asErrorPathMessageStrings: Iterable[(String, String)] =
    this.asErrorPathMessages.map { case (path, message) => (path, message.asString) }

  /** Builds a new result by applying a function to a success value.
    *
    * @tparam B the element type of the returned result
    * @param f the function to apply to a success value
    * @return a new result built from applying a function to a success value
    *
    * @since 0.7.0
    */
  final def map[B](f: A => B): Result[B] = this match {
    case Result.Value(value) => Result.Value(f(value))
    case _: Result.Errors    => this.asInstanceOf[Result[B]]
  }

  /** Builds a new result by applying a function to a success value and using result returned by that that function.
    *
    * @tparam B the element type of the returned result
    * @param f the function to apply to a success value
    * @return a new result built from applying a function to a success value
    *         and using the result returned by that function
    *
    * @since 0.7.0
    */
  final def flatMap[B](f: A => Result[B]): Result[B] = this match {
    case Result.Value(value) => f(value)
    case _: Result.Errors    => this.asInstanceOf[Result[B]]
  }

  /** Prepends a path element to all errors represented by this result.
    *
    * @param pathElement path element to be prepended
    * @return a result with path element prepended to all errors
    *
    * @since 0.7.0
    */
  final def prependErrorPath(pathElement: => PathElement): this.type = this match {
    case _: Result.Value[?] => this
    case e: Result.Errors   => e.prependPath(pathElement).asInstanceOf[this.type]
  }
}

object Result {

  /** Success value case representation
    *
    * @tparam A type of success value
    * @param value value of type `T`
    *
    * @since 0.7.0
    */
  final case class Value[A](value: A) extends Result[A] {

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

    /** Creates failed result from one error or more.
      *
      * @param error required head [[io.scalaland.chimney.partial.Error]]
      * @param errors optional tail [[io.scalaland.chimney.partial.Error]]s
      * @return result aggregating all passed errors
      *
      * @since 0.7.0
      */
    final def apply(error: Error, errors: Error*): Errors =
      apply(NonEmptyErrorsChain.from(error, errors*))

    /** Creates failed result from a single error.
      *
      * @param error required error
      * @return result containing one error
      *
      * @since 0.7.0
      */
    final def single(error: Error): Errors =
      apply(NonEmptyErrorsChain.single(error))

    /** Creates failed result from an error message.
      *
      * @param message message to wrap in Error
      * @return result containing one error
      *
      * @since 0.7.0
      */
    final def fromString(message: String): Errors =
      single(Error.fromString(message))

    /** Creates failed result from one error message or more.
      *
      * @param message required message to wrap in Error
      * @param messages optional messages to wrap in Error
      * @return result aggregating all passed errors
      *
      * @since 0.7.0
      */
    final def fromStrings(message: String, messages: String*): Errors =
      apply(Error.fromString(message), messages.map(Error.fromString)*)

    /** Creates new failed result containing all errors of 2 existing failed results.
      *
      * @param errors1 failed result which errors should appear in the beginning
      * @param errors2 failed result which errors should appear in the end
      * @return result aggregating errors from both results
      *
      * @since 0.7.0
      */
    final def merge(errors1: Errors, errors2: Errors): Errors =
      apply(errors1.errors ++ errors2.errors)

    /** Used internally by macro. Please don't use in your code.
      */
    final def __mergeResultNullable[T](errorsNullable: Errors, result: Result[T]): Errors = {
      result match {
        case _: Value[?]    => errorsNullable
        case errors: Errors => if (errorsNullable == null) errors else merge(errorsNullable, errors)
      }
    }
  }

  /** Converts a function that throws Exceptions into function that returns Result.
    *
    * @tparam A input type
    * @tparam B output type
    * @param f function that possibly throws
    * @return function that caches Exceptions as failed results
    *
    * @since 0.7.0
    */
  final def fromFunction[A, B](f: A => B): A => Result[B] = { a =>
    Result.fromCatching(f(a))
  }

  /** Converts a partial function that throws Exceptions into function that returns Result.
    *
    * @tparam A input type
    * @tparam B output type
    * @param pf partial function that possibly throws
    * @return function that caches Exceptions and arguments without defined value as failed Results
    *
    * @since 0.7.0
    */
  final def fromPartialFunction[A, B](pf: PartialFunction[A, B]): A => Result[B] = { a =>
    if (pf.isDefinedAt(a)) {
      Result.fromCatching(pf(a))
    } else {
      Errors.single(Error.fromNotDefinedAt(a))
    }
  }

  /** Creates successful Result from a precomputed value.
    *
    * @tparam A type of sucessful value
    * @param value successful value to return in result
    * @return successful result
    *
    * @since 0.7.0
    */
  final def fromValue[A](value: A): Result[A] = Value(value)

  /** Creates failed Result with an empty value error.
    *
    * @tparam A type of successful result
    * @return failed result
    *
    * @since 0.7.0
    */
  final def fromEmpty[A]: Result[A] = Errors.single(Error.fromEmptyValue)

  /** Creates failed result from a single error.
    *
    * @tparam A type of successful result
    * @param error required error
    * @return result containing one error
    *
    * @since 0.7.0
    */
  final def fromError[A](error: Error): Result[A] = Errors.single(error)

  /** Creates failed result from one error or more.
    *
    * @tparam A type of successful result
    * @param error  required head [[io.scalaland.chimney.partial.Error]]
    * @param errors optional tail [[io.scalaland.chimney.partial.Error]]s
    * @return result aggregating all passed errors
    *
    * @since 0.7.0
    */
  final def fromErrors[A](error: Error, errors: Error*): Result[A] = Errors(error, errors*)

  /** Creates failed result from an error message.
    *
    * @tparam A type of successful result
    * @param message message to wrap in Error
    * @return result containing one error
    *
    * @since 0.7.0
    */
  final def fromErrorString[A](message: String): Result[A] = Errors.fromString(message)

  /** Creates failed result from one error message or more.
    *
    * @tparam A type of successful result
    * @param message  required message to wrap in Error
    * @param messages optional messages to wrap in Error
    * @return result aggregating all passed errors
    *
    * @since 0.7.0
    */
  final def fromErrorStrings[A](message: String, messages: String*): Result[A] =
    Errors.fromStrings(message, messages*)

  /** Creates failed result from argument for which PartialFunction was not defined.
    *
    * @tparam A type of successful result
    * @param value value for which partial function was not defined
    * @return result containing one error
    *
    * @since 0.7.0
    */
  final def fromErrorNotDefinedAt[A](value: Any): Result[A] = Errors.single(Error.fromNotDefinedAt(value))

  /** Creates failed result from Exception that was caught.
    *
    * @tparam W type of successful result
    * @param throwable exception
    * @return result containing one error
    *
    * @since 0.7.0
    */
  final def fromErrorThrowable[W](throwable: Throwable): Result[W] = Errors.single(Error.fromThrowable(throwable))

  /** Converts Option to Result, using EmptyValue error if None.
    *
    * @tparam A type of successful result
    * @param value Option to convert
    * @return successful result if [[scala.Some]], failed result with EmptyValue error if [[None]]
    *
    * @since 0.7.0
    */
  final def fromOption[A](value: Option[A]): Result[A] = value match {
    case Some(value) => fromValue(value)
    case _           => fromEmpty[A]
  }

  /** Converts Option to Result, using provided Error if None.
    *
    * @tparam A type of successful result
    * @param value   Option to convert
    * @param ifEmpty lazy error for [[scala.None]]
    * @return successful result if [[scala.Some]], failed result with provided error if [[scala.None]]
    * @since 0.7.0
    */
  final def fromOptionOrError[A](value: Option[A], ifEmpty: => Error): Result[A] = value match {
    case Some(value) => fromValue(value)
    case _           => fromError(ifEmpty)
  }

  /** Converts Option to Result, using provided error message if None.
    *
    * @tparam A type of successful result
    * @param value   Option to convert
    * @param ifEmpty lazy error message for [[scala.None]]
    * @return successful result if [[scala.Some]], failed result with provided error message if [[scala.None]]
    *
    * @since 0.7.0
    */
  final def fromOptionOrString[A](value: Option[A], ifEmpty: => String): Result[A] = value match {
    case Some(value) => fromValue(value)
    case _           => fromErrorString(ifEmpty)
  }

  /** Converts Option to Result, using provided Throwable if None.
    *
    * @tparam A type of successful result
    * @param value   Option to convert
    * @param ifEmpty lazy error for [[scala.None]]
    * @return successful result if [[scala.Some]], failed result with provided Throwable if [[scala.None]]
    *
    * @since 0.7.0
    */
  final def fromOptionOrThrowable[A](value: Option[A], ifEmpty: => Throwable): Result[A] = value match {
    case Some(value) => fromValue(value)
    case _           => fromErrorThrowable(ifEmpty)
  }

  /** Converts Either to Result, using Errors from Left as failed result.
    *
    * @tparam A type of successful result
    * @param value   Either to convert
    * @return successful result if [[scala.Right]], failed result if [[scala.Left]]
    *
    * @since 0.7.0
    */
  final def fromEither[A](value: Either[Errors, A]): Result[A] = value match {
    case Right(value)         => fromValue(value)
    case Left(errors: Errors) => errors
  }

  /** Converts Either to Result, using an error message from Left as failed result.
    *
    * @tparam T type of successful result
    * @param value Either to convert
    * @return successful result if [[scala.Right]], failed result with an error message if [[scala.Left]]
    *
    * @since 0.7.0
    */
  final def fromEitherString[T](value: Either[String, T]): Result[T] =
    fromEither(value.left.map(Errors.fromString))

  /** Converts Try to Result, using Throwable from Failure as failed result.
    *
    * @tparam A type of successful result
    * @param value Try to convert
    * @return successful result if [[scala.util.Success]], failed result with Throwable if [[scala.util.Failure]]
    *
    * @since 0.7.0
    */
  final def fromTry[A](value: Try[A]): Result[A] = value match {
    case Success(value)     => fromValue(value)
    case Failure(throwable) => fromErrorThrowable(throwable)
  }

  /** Converts possibly throwing computation into Result.
    *
    * @tparam A type of successful result
    * @param value computation to run while catching its exceptions
    * @return successful Result if computation didn't throw, failed Result with caught exception if it threw
    *
    * @since 0.7.0
    */
  final def fromCatching[A](value: => A): Result[A] =
    try {
      fromValue(value)
    } catch {
      case t: Throwable => fromErrorThrowable(t)
    }

  /** Converts an Iterator of input type into selected collection of output type, aggregating errors from conversions
    * of individual elements.
    *
    * @tparam M type of output - output collection of output element
    * @tparam A type of elements of input
    * @tparam B type of elements of output
    * @param it iterator with elements to convert
    * @param f function converting each individual element to Result
    * @param failFast whether conversion should stop at first failed element conversion
    *                 or should it continue to aggregate all errors
    * @param fac factory of output type
    * @return result with user-selected collection of converted elements if all conversions succeeded,
    *         result with conversion errors if at least one conversion failed
    *
    * @since 0.7.0
    */
  final def traverse[M, A, B](it: Iterator[A], f: A => Result[B], failFast: Boolean)(implicit
      fac: Factory[B, M]
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

  /** Converts an Iterator containing Results into a Result with selected collection of successful values.
    *
    * @tparam M type of output - output collection of output element
    * @tparam A type of successful values in Results
    * @param it iterator with Results to aggregate
    * @param failFast whether conversion should stop at first failed element conversion
    *                 or should it continue to aggregate all errors
    * @param fac factory of output type
    * @return result with user-selected collection of converted elements if all conversions succeeded,
    *         result with conversion errors if at least one conversion failed
    *
    * @since 0.7.0
    */
  final def sequence[M, A](it: Iterator[Result[A]], failFast: Boolean)(implicit fac: Factory[A, M]): Result[M] =
    traverse(it, identity[Result[A]], failFast)

  /** Combines 2 Results into 1 by combining their successful values or aggregating errors.
    *
    * @tparam A first successful input type
    * @tparam B second successful input type
    * @tparam C successful output type
    * @param resultA first Result
    * @param resultB second Result
    * @param f function combining 2 successful input values into successful output value
    * @param failFast whether conversion should stop at first failed element
    *                 or should it aggregate errors from both Results
    * @return successful Result of combination if both results were successful,
    *         failed result if at least one of Result were failure
    *
    * @since 0.7.0
    */
  final def map2[A, B, C](resultA: Result[A], resultB: => Result[B], f: (A, B) => C, failFast: Boolean): Result[C] =
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
        case (errs1: Errors, _: Value[?])   => errs1
        case (_: Value[?], errs2: Errors)   => errs2
      }
    }

  /** Combines 2 Results into 1 by tupling their successful values or aggregating errors.
    *
    * @tparam A first successful input type
    * @tparam B second successful input type
    * @param resultA  first Result
    * @param resultB  second Result
    * @param failFast whether conversion should stop at first failed element
    *                 or should it aggregate errors from both Results
    * @return successful Result with a tuple if both results were successful,
    *         failed result if at least one of Result were failure
    *
    * @since 0.7.0
    */
  final def product[A, B](resultA: Result[A], resultB: => Result[B], failFast: Boolean): Result[(A, B)] =
    map2(resultA, resultB, (x: A, y: B) => (x, y), failFast)
}
