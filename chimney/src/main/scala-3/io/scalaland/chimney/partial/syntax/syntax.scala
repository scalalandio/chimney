package io.scalaland.chimney.partial.syntax

import io.scalaland.chimney.partial.{AsResult, Error, Result}

/** Provides operations lifting [[scala.Option]] to [[io.scalaland.chimney.partial.Result]].
  *
  * @tparam A successful value type
  * @param option value to convert
  *
  * @since 0.8.5
  */
extension [A](option: Option[A])

  /** Converts [[scala.Some]] to [[io.scalaland.chimney.partial.Result.Value]] and uses user-provided
    * [[io.scalaland.chimney.partial.Error]] on [[scala.None]].
    *
    * @param onEmpty thunk creating error on [[scala.None]]
    * @return        result with [[scala.None]] handled
    *
    * @since 0.8.5
    */
  def orErrorAsResult(onEmpty: => Error): Result[A] = Result.fromOptionOrError(option, onEmpty)

  /** Converts [[scala.Some]] to [[io.scalaland.chimney.partial.Result.Value]] and uses user-provided
    * [[java.lang.String]] on [[scala.None]].
    *
    * @param onEmpty thunk creating error message on [[scala.None]]
    * @return        result with [[scala.None]] handled
    *
    * @since 0.8.5
    */
  def orStringAsResult(onEmpty: => String): Result[A] = Result.fromOptionOrString(option, onEmpty)

  /** Converts [[scala.Some]] to [[io.scalaland.chimney.partial.Result.Value]] and uses user-provided
    * [[java.lang.Throwable]] on [[scala.None]].
    *
    * @param onEmpty thunk creating exception on [[scala.None]]
    * @return        result with [[scala.None]] handled
    *
    * @since 0.8.5
    */
  def orThrowableAsResult(onEmpty: => Throwable): Result[A] = Result.fromOptionOrThrowable(option, onEmpty)

/** Provides operations lifting `F[A]` to [[io.scalaland.chimney.partial.Result]].
  *
  * @tparam F wrapper type
  * @tparam A successful value type
  * @param fa value to convert
  *
  * @since 0.8.5
  */
extension [F[_], A](fa: F[A])
  /** Converts `F[A]` to [[io.scalaland.chimney.partial.Result]].
    *
    * @return result with error values handled
    *
    * @since 0.8.5
    */
  def asResult(using F: AsResult[F]): Result[A] = F.asResult(fa)
