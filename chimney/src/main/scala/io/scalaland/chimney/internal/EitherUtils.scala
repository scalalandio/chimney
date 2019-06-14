package io.scalaland.chimney.internal

trait EitherUtils {

  implicit class EitherOps[A, B](either: Either[A, B]) {

    def mapRight[C](f: B => C): Either[A, C] = {
      either match {
        case Right(value) => Right(f(value))
        case Left(value)  => Left(value)
      }
    }

    def mapLeft[C](f: A => C): Either[C, B] = {
      either match {
        case Right(value) => Right(value)
        case Left(value)  => Left(f(value))
      }
    }

    def getRight: B = {
      either match {
        case Right(value) =>
          value
        case _ =>
          throw new NoSuchElementException
      }
    }

    def getLeft: A = {
      either match {
        case Left(value) =>
          value
        case _ =>
          throw new NoSuchElementException
      }
    }
  }
}
