package io.scalaland.chimney.internal.utils

trait EitherOrElseCompat {

  implicit class EitherOps[A, B](val either: Either[A, B]) {
    def orElse[A1 >: A, B1 >: B](or: => Either[A1, B1]): Either[A1, B1] = either match {
      case Right(_) => either
      case _        => or
    }
  }
}
