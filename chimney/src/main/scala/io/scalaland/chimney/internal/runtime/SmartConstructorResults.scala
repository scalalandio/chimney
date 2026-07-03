package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.partial

/** Adapts the result shapes of Hearth `StandardMacroExtension` smart constructors (`hearth.std.CtorLikeOf`:
  * `EitherStringOrValue`/`EitherIterableStringOrValue`/`EitherThrowableOrValue`/`EitherIterableThrowableOrValue`) to
  * [[io.scalaland.chimney.partial.Result]].
  *
  * Referenced by Chimney-GENERATED code (the engine's `PartiallyBuildIterable`/`PartialWrapperClass` extension
  * fallbacks) - it has to be public but it is not meant to be used directly.
  *
  * @since 2.0.0
  */
object SmartConstructorResults {

  /** Maps `hearth.std.CtorLikeOf.EitherStringOrValue`'s result to [[partial.Result]]. */
  def fromEitherString[A](result: Either[String, A]): partial.Result[A] =
    partial.Result.fromEitherString(result)

  /** Maps `hearth.std.CtorLikeOf.EitherIterableStringOrValue`'s result to [[partial.Result]]. */
  def fromEitherStrings[A](result: Either[Iterable[String], A]): partial.Result[A] = result match {
    case Right(value) => partial.Result.fromValue(value)
    case Left(errors) =>
      errors.toList match {
        case head :: tail => partial.Result.fromErrorStrings(head, tail*)
        // $COVERAGE-OFF$Left means failure even if the provider passed no messages (malformed provider).
        case Nil => partial.Result.fromErrorString("Smart constructor failed without providing an error message")
        // $COVERAGE-ON$
      }
  }

  /** Maps `hearth.std.CtorLikeOf.EitherThrowableOrValue`'s result to [[partial.Result]]. */
  def fromEitherThrowable[A](result: Either[Throwable, A]): partial.Result[A] = result match {
    case Right(value)    => partial.Result.fromValue(value)
    case Left(throwable) => partial.Result.fromErrorThrowable(throwable)
  }

  /** Maps `hearth.std.CtorLikeOf.EitherIterableThrowableOrValue`'s result to [[partial.Result]]. */
  def fromEitherThrowables[A](result: Either[Iterable[Throwable], A]): partial.Result[A] = result match {
    case Right(value)     => partial.Result.fromValue(value)
    case Left(throwables) =>
      throwables.toList match {
        case head :: tail =>
          partial.Result.Errors(
            partial.Error.fromThrowable(head),
            tail.map(partial.Error.fromThrowable)*
          )
        // $COVERAGE-OFF$Left means failure even if the provider passed no throwables (malformed provider).
        case Nil => partial.Result.fromErrorString("Smart constructor failed without providing an error message")
        // $COVERAGE-ON$
      }
  }
}
