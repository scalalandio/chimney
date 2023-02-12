package io.scalaland.chimney.cats.utils

import cats.InvariantMonoidal
import cats.data.{Ior, IorNec, IorNel, IorNes, NonEmptyChain, NonEmptyList, NonEmptySet, Validated, ValidatedNec, ValidatedNel}

object ValidatedUtils {

  implicit class OptionOps[T](private val opt: Option[T]) extends AnyVal {

    def toValidated[EE[_]: InvariantMonoidal](err: => String): Validated[EE[String], T] = {
      opt match {
        case Some(value) => Validated.Valid(value)
        case None        => Validated.Invalid(InvariantMonoidal[EE].point(err))
      }
    }

    def toValidatedNec(err: => String): ValidatedNec[String, T] =
      toValidated[NonEmptyChain](err)(implicitly)

    def toValidatedNel(err: => String): ValidatedNel[String, T] =
      toValidated[NonEmptyList](err)(implicitly)

    def toIor[EE[_] : InvariantMonoidal](err: => String): Ior[EE[String], T] = {
      opt match {
        case Some(value) => Ior.Right(value)
        case None => Ior.Left(InvariantMonoidal[EE].point(err))
      }
    }

    def toIorNec(err: => String): IorNec[String, T] =
      toIor[NonEmptyChain](err)(implicitly)

    def toIorNel(err: => String): IorNel[String, T] =
      toIor[NonEmptyList](err)(implicitly)

    def toIorNes(err: => String): IorNes[String, T] =
      opt match {
        case Some(value) => Ior.Right(value)
        case None => Ior.Left(NonEmptySet.one(err))
      }
  }

  implicit class ValidatedOps[+E, +A](private val validated: Validated[E, A]) extends AnyVal {

    def getValid: A = {
      validated.valueOr(_ => throw new NoSuchElementException)
    }
  }
}
