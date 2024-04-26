package io.scalaland.chimney.partial

import scala.util.Try

/** Utility allowing conversion from some type into [[io.scalaland.chimney.partial.Result]].
  *
  * Should define logic what is considered successful value, what is considered failed value and how to convert it into
  * [[io.scalaland.chimney.partial.Result]].
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#total-transformers-vs-partialtransformers]]
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#partialresult-utilities]]
  * @tparam F
  *   generic data type which should be convertible to `Result` for all possible values
  *
  * @since 0.8.5
  */
@FunctionalInterface
trait AsResult[F[_]] {

  /** Converts `F[A]` into `Result[A]`.
    *
    * @tparam A
    *   type of successful value
    * @param fa
    *   converted value
    * @return
    *   converted value
    *
    * @since 0.8.5
    */
  def asResult[A](fa: F[A]): Result[A]
}

/** Companion of [[io.scalaland.chimney.partial.AsResult]].
  *
  * @since 0.8.5
  */
object AsResult {

  /** @since 0.8.5 */
  implicit val optionAsResult: AsResult[Option] = new AsResult[Option] {
    def asResult[A](fa: Option[A]): Result[A] = Result.fromOption(fa)
  }

  /** @since 0.8.5 */
  implicit val eitherErrorAsResult: AsResult[Either[Result.Errors, *]] = new AsResult[Either[Result.Errors, *]] {
    def asResult[A](fa: Either[Result.Errors, A]): Result[A] = Result.fromEither(fa)
  }

  /** @since 0.8.5 */
  implicit val eitherStringAsResult: AsResult[Either[String, *]] = new AsResult[Either[String, *]] {
    def asResult[A](fa: Either[String, A]): Result[A] = Result.fromEitherString(fa)
  }

  /** @since 0.8.5 */
  implicit val tryAsResult: AsResult[Try] = new AsResult[Try] {
    def asResult[A](fa: Try[A]): Result[A] = Result.fromTry(fa)
  }
}
