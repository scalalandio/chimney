package io.scalaland.chimney.cats.utils

import cats.InvariantMonoidal
import cats.data.{
  Ior,
  IorNec,
  IorNel,
  IorNes,
  NonEmptyChain,
  NonEmptyList,
  NonEmptySet,
  Validated,
  ValidatedNec,
  ValidatedNel
}

object ValidatedUtils {

  implicit class OptionOps[A](private val opt: Option[A]) extends AnyVal {

    def toValidated[EE[_]: InvariantMonoidal](err: => String): Validated[EE[String], A] =
      opt match {
        case Some(value) => Validated.Valid(value)
        case None        => Validated.Invalid(InvariantMonoidal[EE].point(err))
      }

    def toValidatedNec(err: => String): ValidatedNec[String, A] =
      toValidated[NonEmptyChain](err)(implicitly)

    def toValidatedNel(err: => String): ValidatedNel[String, A] =
      toValidated[NonEmptyList](err)(implicitly)

    def toIor[EE[_]: InvariantMonoidal](err: => String): Ior[EE[String], A] =
      opt match {
        case Some(value) => Ior.Right(value)
        case None        => Ior.Left(InvariantMonoidal[EE].point(err))
      }

    def toIorNec(err: => String): IorNec[String, A] =
      toIor[NonEmptyChain](err)(implicitly)

    def toIorNel(err: => String): IorNel[String, A] =
      toIor[NonEmptyList](err)(implicitly)

    def toIorNes(err: => String): IorNes[String, A] =
      opt match {
        case Some(value) => Ior.Right(value)
        case None        => Ior.Left(NonEmptySet.one(err))
      }
  }

  implicit class ValidatedOps[+E, +A](private val validated: Validated[E, A]) extends AnyVal {

    def getValid: A =
      validated.valueOr(_ => throw new NoSuchElementException)
  }
}
