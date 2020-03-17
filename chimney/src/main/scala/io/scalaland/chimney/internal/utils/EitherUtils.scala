package io.scalaland.chimney.internal.utils

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

    def flatMapRight[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = {
      either match {
        case Right(value) => f(value)
        case left         => left.asInstanceOf[Either[A1, B1]]
      }
    }

    def getRight: B = {
      either match {
        case Right(value) =>
          value
        case _ =>
          // $COVERAGE-OFF$
          throw new NoSuchElementException
        // $COVERAGE-ON$
      }
    }

    def getLeft: A = {
      either match {
        case Left(value) =>
          value
        case _ =>
          // $COVERAGE-OFF$
          throw new NoSuchElementException
        // $COVERAGE-ON$
      }
    }

    def rightOrElse[A1 >: A, B1 >: B](or: => Either[A1, B1]): Either[A1, B1] = {
      either match {
        case Right(_) => either
        case _        => or
      }
    }

  }

  implicit class MapOps[K, E, V](map: Map[K, Either[E, V]]) {

    def partitionEitherValues: (Map[K, E], Map[K, V]) = {
      val (lefts, rights) = map.partition(_._2.isLeft)
      (
        lefts.map { case (k, v)  => k -> v.getLeft },
        rights.map { case (k, v) => k -> v.getRight }
      )
    }
  }
}

object EitherUtils extends EitherUtils
