package io.scalaland.chimney.utils

object EitherUtils {

  implicit final class OptionOps[A](private val opt: Option[A]) extends AnyVal {
    def toEither(err: => String): Either[String, A] =
      opt match {
        case Some(value) => Right(value)
        case None        => Left(err)
      }

    def toEitherList(err: => String): Either[List[String], A] =
      opt match {
        case Some(value) => Right(value)
        case None        => Left(List(err))
      }
  }

}
