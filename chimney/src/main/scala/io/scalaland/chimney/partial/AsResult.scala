package io.scalaland.chimney.partial

import scala.util.Try

// TODO: docs

trait AsResult[F[_]] {

  def asResult[A](fa: F[A]): Result[A]
}
object AsResult {

  implicit val optionAsResult: AsResult[Option] = new AsResult[Option] {
    def asResult[A](fa: Option[A]): Result[A] = Result.fromOption(fa)
  }

  implicit val eitherErrorAsResult: AsResult[Either[Result.Errors, *]] = new AsResult[Either[Result.Errors, *]] {
    def asResult[A](fa: Either[Result.Errors, A]): Result[A] = Result.fromEither(fa)
  }

  implicit val eitherStringAsResult: AsResult[Either[String, *]] = new AsResult[Either[String, *]] {
    def asResult[A](fa: Either[String, A]): Result[A] = Result.fromEitherString(fa)
  }
  
  implicit val tryAsResult: AsResult[Try] = new AsResult[Try] {
    def asResult[A](fa: Try[A]): Result[A] = Result.fromTry(fa)
  }
}
