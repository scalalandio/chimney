package io.scalaland.chimney.partial

import io.scalaland.chimney.internal.runtime.NonEmptyErrorsChain
import scala.collection.compat.*
import scala.util.{Failure, Success, Try}

/** Data type representing either successfully computed value or non-empty collection of path-annotated errors.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#total-transformers-vs-partialtransformers]]
  *
  * @tparam A
  *   type of success value
  *
  * @since 0.7.0
  */
sealed trait Result[+A] {

  /** Converts a partial result to an optional value.
    *
    * @return
    *   [[scala.Some]] if success, [[scala.None]] otherwise
    *
    * @since 0.7.0
    */
  final def asOption: Option[A] = this match {
    case Result.Value(value) => Some(value)
    case Result.Errors(_)    => None
  }

  /** Converts a partial result to an [[scala.Either]].
    *
    * @return
    *   [[scala.Right]] if success, [[scala.Left]] containing [[io.scalaland.chimney.partial.Result.Errors]] otherwise
    *
    * @since 0.7.0
    */
  final def asEither: Either[Result.Errors, A] = this match {
    case Result.Value(value)   => Right(value)
    case errors: Result.Errors => Left(errors)
  }

  /** Returns (possibly empty) collection of tuples with conventional string representation of path and errors message.
    *
    * @return
    *   empty [[scala.collection.Iterable]] if success, a sequence of pairs (a path to a failed field, an
    *   [[io.scalaland.chimney.partial.ErrorMessage]]) otherwise
    *
    * @since 0.7.0
    */
  def asErrorPathMessages: Iterable[(String, ErrorMessage)]

  /** Returns (possibly empty) collection of tuples with conventional string representation of path and string
    * representation of error message.
    *
    * @return
    *   empty [[scala.collection.Iterable]] if success, a sequence of pairs (a path to a failed field, an errors message
    *   as [[java.lang.String]]
    *
    * @since 0.7.0
    */
  final def asErrorPathMessageStrings: Iterable[(String, String)] =
    this.asErrorPathMessages.map { case (path, message) => (path, message.asString) }

  /** Converts a partial result to an [[scala.Either]] with collection of tuples with conventional string representation
    * of path and errors message as [[scala.Left]].
    *
    * @return
    *   [[scala.Right]] if success, [[scala.Left]] containing a sequence of pairs (a path to a failed field, an
    *   [[io.scalaland.chimney.partial.ErrorMessage]]) otherwise
    *
    * @since 0.8.5
    */
  final def asEitherErrorPathMessages: Either[Iterable[(String, ErrorMessage)], A] = this match {
    case Result.Value(value)   => Right(value)
    case errors: Result.Errors => Left(errors.asErrorPathMessages)
  }

  /** Converts a partial result to an [[scala.Either]] with collection of tuples with conventional string representation
    * of path and string representation of error message as [[scala.Left]].
    *
    * @return
    *   [[scala.Right]] if success, [[scala.Left]] containing a sequence of pairs (a path to a failed field, an errors
    *   message as [[java.lang.String]] otherwise
    *
    * @since 0.8.5
    */
  final def asEitherErrorPathMessageStrings: Either[Iterable[(String, String)], A] = this match {
    case Result.Value(value)   => Right(value)
    case errors: Result.Errors => Left(errors.asErrorPathMessageStrings)
  }

  /** Extracts value from a partial result and applies it to the appropriate function.
    *
    * @tparam B
    *   the type of the folding
    * @param onValue
    *   the function to apply to success value
    * @param onErrors
    *   the function to apply to errors
    * @return
    *   a new value
    *
    * @since 0.8.5
    */
  final def fold[B](onValue: A => B, onErrors: Result.Errors => B): B = this match {
    case Result.Value(value)   => onValue(value)
    case errors: Result.Errors => onErrors(errors)
  }

  /** Builds a new result by applying a function to a success value.
    *
    * @tparam B
    *   the element type of the returned result
    * @param f
    *   the function to apply to a success value
    * @return
    *   a new [[io.scalaland.chimney.partial.Result]] built from applying a function to a success value
    *
    * @since 0.7.0
    */
  final def map[B](f: A => B): Result[B] = this match {
    case Result.Value(value) => Result.Value(f(value))
    case _: Result.Errors    => this.asInstanceOf[Result[B]]
  }

  /** Builds a new result by applying a function to a success value and using result returned by that that function.
    *
    * @tparam B
    *   the element type of the returned result
    * @param f
    *   the function to apply to a success value
    * @return
    *   a new [[io.scalaland.chimney.partial.Result]] built from applying a function to a success value and using the
    *   [[io.scalaland.chimney.partial.Result]] returned by that function
    *
    * @since 0.7.0
    */
  final def flatMap[B](f: A => Result[B]): Result[B] = this match {
    case Result.Value(value) => f(value)
    case _: Result.Errors    => this.asInstanceOf[Result[B]]
  }

  /** Builds a new result by flattening the current value.
    *
    * @tparam B
    *   the element type of the returned result
    * @return
    *   a new [[io.scalaland.chimney.partial.Result]] built from applying a function to a success value and using the
    *   [[io.scalaland.chimney.partial.Result]] returned by that function
    *
    * @since 0.8.4
    */
  final def flatten[B](implicit ev: A <:< Result[B]): Result[B] = this match {
    case Result.Value(value) => ev(value)
    case _: Result.Errors    => this.asInstanceOf[Result[B]]
  }

  /** Prepends a [[io.scalaland.chimney.partial.PathElement]] to all errors represented by this result.
    *
    * @tparam B
    *   the element type of the returned result
    * @param result
    *   lazy [[io.scalaland.chimney.partial.Result]] to compute as a fallback if this one has errors
    * @return
    *   a [[io.scalaland.chimney.partial.Result]] with the first successful value or a failure combining errors from
    *   both results
    *
    * @since 1.0.0
    */
  final def orElse[B >: A](result: => Result[B]): Result[B] = this match {
    case _: Result.Value[?] => this
    case e: Result.Errors =>
      result match {
        case r: Result.Value[?] => r
        case e2: Result.Errors  => Result.Errors.merge(e, e2)
      }
  }

  /** Prepends a [[io.scalaland.chimney.partial.PathElement]] to all errors represented by this result.
    *
    * @param pathElement
    *   [[io.scalaland.chimney.partial.PathElement]] to be prepended
    * @return
    *   a [[io.scalaland.chimney.partial.Result]] with [[io.scalaland.chimney.partial.PathElement]] prepended to all
    *   errors
    *
    * @since 0.7.0
    */
  final def prependErrorPath(pathElement: => PathElement): this.type = this match {
    case _: Result.Value[?] => this
    case e: Result.Errors   => e.prependPath(pathElement).asInstanceOf[this.type]
  }

  /** Unseals the [[io.scalaland.chimney.partial.Path]] of current [[io.scalaland.chimney.partial.Error]].
    *
    * When derivation is building up the result it automatically appends fields/indices/map keys - however values
    * obtained with withFieldComputed(Partial)(From) contains the whole Path already, so [[prependErrorPath]] should be
    * a noop for them.
    *
    * However, this path can only be precomputed only up to the boundaries of a
    * [[io.scalaland.chimney.PartialTransformer]], and when one transformer calls another, path should be appended
    * again. This method allows this.
    *
    * @return
    *   error with a path prepended with provided path element
    *
    * @since 1.6.0
    */
  final def unsealErrorPath: this.type = this match {
    case _: Result.Value[?] => this
    case e: Result.Errors   => e.unsealPath.asInstanceOf[this.type]
  }
}

/** Companion to [[io.scalaland.chimney.partial.Result]].
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#total-transformers-vs-partialtransformers]]
  *
  * @since 0.7.0
  */
object Result {

  /** Success value case representation.
    *
    * @tparam A
    *   type of success value
    * @param value
    *   value of type `A`
    *
    * @since 0.7.0
    */
  final case class Value[A](value: A) extends Result[A] {

    def asErrorPathMessages: Iterable[(String, ErrorMessage)] = Iterable.empty
  }

  /** Errors case representation.
    *
    * @param errors
    *   non-empty collection of path-annotated errors
    *
    * @since 0.7.0
    */
  final case class Errors(errors: NonEmptyErrorsChain) extends Result[Nothing] {

    def prependPath(pathElement: PathElement): Errors = Errors(errors.prependPath(pathElement))

    def unsealPath: Errors = Errors(errors.unsealPath)

    def asErrorPathMessages: Iterable[(String, ErrorMessage)] = errors.map(_.asErrorPathMessage)
  }

  object Errors {

    /** Creates failed result from one [[io.scalaland.chimney.partial.Error]] or more.
      *
      * @param error
      *   required head [[io.scalaland.chimney.partial.Error]]
      * @param errors
      *   optional tail [[io.scalaland.chimney.partial.Error]]s
      * @return
      *   [[io.scalaland.chimney.partial.Result.Errors]] aggregating all passed [[io.scalaland.chimney.partial.Error]]s
      *
      * @since 0.7.0
      */
    final def apply(error: Error, errors: Error*): Errors =
      apply(NonEmptyErrorsChain.from(error, errors*))

    /** Creates failed result from a single [[io.scalaland.chimney.partial.Error]].
      *
      * @param error
      *   required [[io.scalaland.chimney.partial.Error]]
      * @return
      *   [[io.scalaland.chimney.partial.Result.Errors]] containing one [[io.scalaland.chimney.partial.Error]]
      *
      * @since 0.7.0
      */
    final def single(error: Error): Errors =
      apply(NonEmptyErrorsChain.single(error))

    /** Creates failed result from an error message.
      *
      * @param message
      *   message to wrap in [[io.scalaland.chimney.partial.Error]]
      * @return
      *   [[io.scalaland.chimney.partial.Result.Errors]] containing one [[io.scalaland.chimney.partial.Error]]
      *
      * @since 0.7.0
      */
    final def fromString(message: String): Errors =
      single(Error.fromString(message))

    /** Creates failed result from one error message or more.
      *
      * @param message
      *   required message to wrap in [[io.scalaland.chimney.partial.Error]]
      * @param messages
      *   optional messages to wrap in [[io.scalaland.chimney.partial.Error]]
      * @return
      *   [[io.scalaland.chimney.partial.Result.Errors]] aggregating all passed [[io.scalaland.chimney.partial.Error]]s
      *
      * @since 0.7.0
      */
    final def fromStrings(message: String, messages: String*): Errors =
      apply(Error.fromString(message), messages.map(Error.fromString)*)

    /** Creates new failed result containing all errors of 2 existing failed results.
      *
      * @param errors1
      *   failed result which [[io.scalaland.chimney.partial.Error]]s should appear in the beginning
      * @param errors2
      *   failed result which [[io.scalaland.chimney.partial.Error]]s should appear in the end
      * @return
      *   [[io.scalaland.chimney.partial.Result.Errors]] aggregating errors from both results
      *
      * @since 0.7.0
      */
    final def merge(errors1: Errors, errors2: Errors): Errors =
      apply(errors1.errors ++ errors2.errors)
  }

  /** Converts a function that throws Exceptions into function that returns [[io.scalaland.chimney.partial.Result]].
    *
    * @tparam A
    *   input type
    * @tparam B
    *   output type
    * @param f
    *   function that possibly throws
    * @return
    *   function that catches Exceptions as [[io.scalaland.chimney.partial.Result.Errors]]
    *
    * @since 0.7.0
    */
  final def fromFunction[A, B](f: A => B): A => Result[B] = { a =>
    Result.fromCatching(f(a))
  }

  /** Converts a partial function that throws Exceptions into function that returns
    * [[io.scalaland.chimney.partial.Result]].
    *
    * @tparam A
    *   input type
    * @tparam B
    *   output type
    * @param pf
    *   partial function that possibly throws
    * @return
    *   function that catches Exceptions and arguments without defined value as
    *   [[io.scalaland.chimney.partial.Result.Errors]]
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

  /** Creates successful [[io.scalaland.chimney.partial.Result]] from a precomputed value.
    *
    * @tparam A
    *   type of successful value
    * @param value
    *   successful value to return in result
    * @return
    *   successful [[io.scalaland.chimney.partial.Result.Value]]
    *
    * @since 0.7.0
    */
  final def fromValue[A](value: A): Result[A] = Value(value)

  /** Creates failed Result with an empty value error.
    *
    * @tparam A
    *   type of successful result
    * @return
    *   failed [[io.scalaland.chimney.partial.Result.Errors]] containing one
    *   [[io.scalaland.chimney.partial.Error.fromEmptyValue]]
    *
    * @since 0.7.0
    */
  final def fromEmpty[A]: Result[A] = Errors.single(Error.fromEmptyValue)

  /** Creates failed [[io.scalaland.chimney.partial.Result]] from a single error.
    *
    * @tparam A
    *   type of successful result
    * @param error
    *   required error
    * @return
    *   failed [[io.scalaland.chimney.partial.Result.Errors]] containing one [[io.scalaland.chimney.partial.Error]]
    *
    * @since 0.7.0
    */
  final def fromError[A](error: Error): Result[A] = Errors.single(error)

  /** Creates failed [[io.scalaland.chimney.partial.Result]] from one error or more.
    *
    * @tparam A
    *   type of successful result
    * @param error
    *   required head [[io.scalaland.chimney.partial.Error]]
    * @param errors
    *   optional tail [[io.scalaland.chimney.partial.Error]]s
    * @return
    *   failed [[io.scalaland.chimney.partial.Result.Errors]] aggregating all passed
    *   [[io.scalaland.chimney.partial.Error]]s
    *
    * @since 0.7.0
    */
  final def fromErrors[A](error: Error, errors: Error*): Result[A] = Errors(error, errors*)

  /** Creates failed [[io.scalaland.chimney.partial.Result]] from an error message.
    *
    * @tparam A
    *   type of successful result
    * @param message
    *   message to wrap in Error
    * @return
    *   result containing one error
    *
    * @since 0.7.0
    */
  final def fromErrorString[A](message: String): Result[A] = Errors.fromString(message)

  /** Creates failed [[io.scalaland.chimney.partial.Result]] from one error message or more.
    *
    * @tparam A
    *   type of successful result
    * @param message
    *   required message to wrap in Error
    * @param messages
    *   optional messages to wrap in Error
    * @return
    *   result aggregating all passed errors
    *
    * @since 0.7.0
    */
  final def fromErrorStrings[A](message: String, messages: String*): Result[A] =
    Errors.fromStrings(message, messages*)

  /** Creates failed [[io.scalaland.chimney.partial.Result]] from argument for which PartialFunction was not defined.
    *
    * @tparam A
    *   type of successful result
    * @param value
    *   value for which partial function was not defined
    * @return
    *   result containing one error
    *
    * @since 0.7.0
    */
  final def fromErrorNotDefinedAt[A](value: Any): Result[A] = Errors.single(Error.fromNotDefinedAt(value))

  /** Creates failed [[io.scalaland.chimney.partial.Result]] from Exception that was caught.
    *
    * @tparam W
    *   type of successful result
    * @param throwable
    *   exception
    * @return
    *   result containing one error
    *
    * @since 0.7.0
    */
  final def fromErrorThrowable[W](throwable: Throwable): Result[W] = Errors.single(Error.fromThrowable(throwable))

  /** Converts Option to [[io.scalaland.chimney.partial.Result]], using EmptyValue error if None.
    *
    * @tparam A
    *   type of successful result
    * @param value
    *   Option to convert
    * @return
    *   successful result if [[scala.Some]], failed result with EmptyValue error if [[None]]
    *
    * @since 0.7.0
    */
  final def fromOption[A](value: Option[A]): Result[A] = value match {
    case Some(value) => fromValue(value)
    case _           => fromEmpty[A]
  }

  /** Converts Option to [[io.scalaland.chimney.partial.Result]], using provided Error if None.
    *
    * @tparam A
    *   type of successful result
    * @param value
    *   Option to convert
    * @param ifEmpty
    *   lazy error for [[scala.None]]
    * @return
    *   successful result if [[scala.Some]], failed result with provided error if [[scala.None]]
    *
    * @since 0.7.0
    */
  final def fromOptionOrError[A](value: Option[A], ifEmpty: => Error): Result[A] = value match {
    case Some(value) => fromValue(value)
    case _           => fromError(ifEmpty)
  }

  /** Converts Option to [[io.scalaland.chimney.partial.Result]], using provided error message if None.
    *
    * @tparam A
    *   type of successful result
    * @param value
    *   [[scala.Option]] to convert
    * @param ifEmpty
    *   lazy error message for [[scala.None]]
    * @return
    *   successful result if [[scala.Some]], failed result with provided error message if [[scala.None]]
    *
    * @since 0.7.0
    */
  final def fromOptionOrString[A](value: Option[A], ifEmpty: => String): Result[A] = value match {
    case Some(value) => fromValue(value)
    case _           => fromErrorString(ifEmpty)
  }

  /** Converts Option to Result, using provided Throwable if None.
    *
    * @tparam A
    *   type of successful result
    * @param value
    *   Option to convert
    * @param ifEmpty
    *   lazy error for [[scala.None]]
    * @return
    *   successful result if [[scala.Some]], failed result with provided Throwable if [[scala.None]]
    *
    * @since 0.7.0
    */
  final def fromOptionOrThrowable[A](value: Option[A], ifEmpty: => Throwable): Result[A] = value match {
    case Some(value) => fromValue(value)
    case _           => fromErrorThrowable(ifEmpty)
  }

  /** Converts Either to [[io.scalaland.chimney.partial.Result]], using Errors from Left as failed result.
    *
    * @tparam A
    *   type of successful result
    * @param value
    *   Either to convert
    * @return
    *   successful result if [[scala.Right]], failed result if [[scala.Left]]
    *
    * @since 0.7.0
    */
  final def fromEither[A](value: Either[Errors, A]): Result[A] = value match {
    case Right(value)         => fromValue(value)
    case Left(errors: Errors) => errors
  }

  /** Converts Either to [[io.scalaland.chimney.partial.Result]], using an error message from Left as failed result.
    *
    * @tparam A
    *   type of successful result
    * @param value
    *   Either to convert
    * @return
    *   successful result if [[scala.Right]], failed result with an error message if [[scala.Left]]
    *
    * @since 0.7.0
    */
  final def fromEitherString[A](value: Either[String, A]): Result[A] =
    fromEither(value.left.map(Errors.fromString))

  /** Converts Try to [[io.scalaland.chimney.partial.Result]], using Throwable from Failure as failed result.
    *
    * @tparam A
    *   type of successful result
    * @param value
    *   [[scala.util.Try]] to convert
    * @return
    *   successful result if [[scala.util.Success]], failed result with Throwable if [[scala.util.Failure]]
    *
    * @since 0.7.0
    */
  final def fromTry[A](value: Try[A]): Result[A] = value match {
    case Success(value)     => fromValue(value)
    case Failure(throwable) => fromErrorThrowable(throwable)
  }

  /** Converts possibly throwing computation into [[io.scalaland.chimney.partial.Result]].
    *
    * @tparam A
    *   type of successful result
    * @param value
    *   computation to run while catching its Exceptions
    * @return
    *   successful [[io.scalaland.chimney.partial.Result.Value]] if computation didn't throw, failed
    *   [[io.scalaland.chimney.partial.Result.Errors]] with caught Exception if it threw
    *
    * @since 0.7.0
    */
  final def fromCatching[A](value: => A): Result[A] =
    try
      fromValue(value)
    catch {
      case t: Throwable => fromErrorThrowable(t)
    }

  /** Converts possibly throwing computation into [[io.scalaland.chimney.partial.Result]] by catching only
    * [[scala.util.control.NonFatal NonFatal Exceptions]].
    *
    * @tparam A
    *   type of successful result
    * @param value
    *   computation to run while catching its [[scala.util.control.NonFatal NonFatal Exceptions]]
    * @return
    *   successful [[io.scalaland.chimney.partial.Result.Value]] if computation didn't throw, failed
    *   [[io.scalaland.chimney.partial.Result.Errors]] with caught Exception if it threw
    * @since 1.5.0
    */
  final def fromCatchingNonFatal[A](value: => A): Result[A] =
    try
      fromValue(value)
    catch {
      case scala.util.control.NonFatal(t) => fromErrorThrowable(t)
    }

  /** Converts an [[scala.collection.Iterator]] of input type into selected collection of output type, aggregating
    * errors from conversions of individual elements.
    *
    * @tparam M
    *   type of output - output collection of output element
    * @tparam A
    *   type of elements of input
    * @tparam B
    *   type of elements of output
    * @param it
    *   iterator with elements to convert
    * @param f
    *   function converting each individual element to Result
    * @param failFast
    *   whether conversion should stop at first failed element conversion or should it continue to aggregate all errors
    * @param fac
    *   factory of output type
    * @return
    *   [[io.scalaland.chimney.partial.Result.Value]] with user-selected collection of converted elements if all
    *   conversions succeeded, [[io.scalaland.chimney.partial.Result.Errors]] with conversion errors if at least one
    *   conversion failed
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
      while (errors == null && it.hasNext)
        f(it.next()) match {
          case Value(value) => bs += value
          case e: Errors    => errors = e
        }
      if (errors == null) Result.Value(bs.result()) else errors
    } else {
      var allErrors: NonEmptyErrorsChain = null
      while (it.hasNext)
        f(it.next()) match {
          case Value(value) => bs += value
          case Errors(ee) =>
            if (allErrors == null) allErrors = ee
            else allErrors ++= ee
        }
      if (allErrors == null) Result.Value(bs.result()) else Result.Errors(allErrors)
    }
  }

  /** Converts an [[scala.collection.Iterator]] containing [[io.scalaland.chimney.partial.Result]]s into a
    * [[io.scalaland.chimney.partial.Result]] with selected collection of successful values.
    *
    * @tparam M
    *   type of output - output collection of output element
    * @tparam A
    *   type of successful values in Results
    * @param it
    *   iterator with Results to aggregate
    * @param failFast
    *   whether conversion should stop at first failed element conversion or should it continue to aggregate all errors
    * @param fac
    *   factory of output type
    * @return
    *   result with user-selected collection of converted elements if all conversions succeeded, result with conversion
    *   errors if at least one conversion failed
    *
    * @since 0.7.0
    */
  final def sequence[M, A](it: Iterator[Result[A]], failFast: Boolean)(implicit fac: Factory[A, M]): Result[M] =
    traverse(it, identity[Result[A]], failFast)

  /** Combines 2 [[io.scalaland.chimney.partial.Result]]s into 1 by combining their successful values or aggregating
    * errors.
    *
    * @tparam A
    *   first successful input type
    * @tparam B
    *   second successful input type
    * @tparam C
    *   successful output type
    * @param resultA
    *   first Result
    * @param resultB
    *   second Result
    * @param f
    *   function combining 2 successful input values into successful output value
    * @param failFast
    *   whether conversion should stop at first failed element or should it aggregate errors from both Results
    * @return
    *   successful [[io.scalaland.chimney.partial.Result.Value]] of combination if both
    *   [[io.scalaland.chimney.partial.Result]]s were successful, failed [[io.scalaland.chimney.partial.Result.Errors]]
    *   if at least one of [[io.scalaland.chimney.partial.Result]]s were failure
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

  /** Combines 2 [[io.scalaland.chimney.partial.Result]]s into 1 by tupling their successful values or aggregating
    * errors.
    *
    * @tparam A
    *   first successful input type
    * @tparam B
    *   second successful input type
    * @param resultA
    *   first [[io.scalaland.chimney.partial.Result]]
    * @param resultB
    *   second [[io.scalaland.chimney.partial.Result]]
    * @param failFast
    *   whether conversion should stop at first failed element or should it aggregate errors from both
    *   [[io.scalaland.chimney.partial.Result]]s
    * @return
    *   successful [[io.scalaland.chimney.partial.Result.Value]] with a tuple if both
    *   [[io.scalaland.chimney.partial.Result]]s were successful, failed [[io.scalaland.chimney.partial.Result.Errors]]
    *   if at least one of [[io.scalaland.chimney.partial.Result]] were failure
    *
    * @since 0.7.0
    */
  final def product[A, B](resultA: Result[A], resultB: => Result[B], failFast: Boolean): Result[(A, B)] =
    map2(resultA, resultB, (x: A, y: B) => (x, y), failFast)
}
