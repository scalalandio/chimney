package io.scalaland.chimney.utils

object EitherUtils {

  implicit class OptionOps[T](val opt: Option[T]) extends AnyVal {
    def toEither(err: => String): Either[List[String], T] = {
      opt match {
        case Some(value) => Right(value)
        case None        => Left(List(err))
      }
    }
  }

}
