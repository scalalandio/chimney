package io.scalaland.chimney.cats

import _root_.cats.data._
import _root_.cats.kernel._
import _root_.cats.Semigroupal
import io.scalaland.chimney._

import scala.collection.compat._
import language.implicitConversions

trait CatsPartialTransformerImplicits extends CatsPartialTransformerLowPriorityImplicits1 {

  implicit final val monoidPartialTransformerResultErrors: Semigroup[partial.Result.Errors] = {
    Semigroup.instance(partial.Result.Errors.merge)
  }

  implicit final val semigroupalPartialTransformerResult: Semigroupal[partial.Result] = {
    new Semigroupal[partial.Result] {
      override def product[A, B](
          fa: partial.Result[A],
          fb: partial.Result[B]
      ): partial.Result[(A, B)] = {
        partial.Result.product(fa, fb, failFast = false)
      }
    }
  }

  implicit final def catsPartialTransformerResultOps[T](
      ptr: partial.Result[T]
  ): CatsPartialTransformerResultOps[T] =
    new CatsPartialTransformerResultOps(ptr)

  implicit final def catsValidatedPartialTransformerOps[E <: partial.Result.Errors, T](
      validated: Validated[E, T]
  ): CatsValidatedPartialTransformerOps[E, T] =
    new CatsValidatedPartialTransformerOps(validated)
}

private[cats] trait CatsPartialTransformerLowPriorityImplicits1 {

  implicit final def catsValidatedNelErrorPartialTransformerOps[E <: partial.Error, T](
      validated: ValidatedNel[E, T]
  ): CatsValidatedNelErrorPartialTransformerOps[E, T] =
    new CatsValidatedNelErrorPartialTransformerOps(validated)

  implicit final def catsValidatedNelStringPartialTransformerOps[E <: String, T](
      validated: ValidatedNel[E, T]
  ): CatsValidatedNelStringPartialTransformerOps[E, T] =
    new CatsValidatedNelStringPartialTransformerOps(validated)

  implicit final def catsValidatedNecErrorPartialTransformerOps[E <: partial.Error, T](
      validated: ValidatedNec[E, T]
  ): CatsValidatedNecErrorPartialTransformerOps[E, T] =
    new CatsValidatedNecErrorPartialTransformerOps(validated)

  implicit final def catsValidatedNecStringPartialTransformerOps[E <: String, T](
      validated: ValidatedNec[E, T]
  ): CatsValidatedNecStringPartialTransformerOps[E, T] =
    new CatsValidatedNecStringPartialTransformerOps(validated)

}

final class CatsPartialTransformerResultOps[T](private val ptr: partial.Result[T]) extends AnyVal {

  def asValidated: Validated[partial.Result.Errors, T] = {
    Validated.fromEither(ptr.asEither)
  }

  def asValidatedList: Validated[List[partial.Error], T] = {
    ptr.asValidated.leftMap(_.errors.toList)
  }

  def asValidatedChain: Validated[Chain[partial.Error], T] = {
    ptr.asValidated.leftMap(errs => Chain.fromIterableOnce(errs.errors))
  }

  def asValidatedNel: ValidatedNel[partial.Error, T] = {
    ptr.asValidated.leftMap { errs =>
      // errors collection is non-empty by design
      NonEmptyList.fromListUnsafe(errs.errors.iterator.toList)
    }
  }

  def asValidatedNec: ValidatedNec[partial.Error, T] = {
    ptr.asValidatedNel.leftMap(NonEmptyChain.fromNonEmptyList)
  }

}

final class CatsValidatedPartialTransformerOps[E <: partial.Result.Errors, T](
    private val validated: Validated[E, T]
) extends AnyVal {
  def toPartialTransformerResult: partial.Result[T] = {
    partial.Result.fromEither(validated.toEither)
  }
}

final class CatsValidatedNelErrorPartialTransformerOps[E <: partial.Error, T](
    private val validated: ValidatedNel[E, T]
) extends AnyVal {
  def toPartialTransformerResult: partial.Result[T] = {
    validated.leftMap(errs => partial.Result.Errors(errs.head, errs.tail: _*)).toPartialTransformerResult
  }
}

final class CatsValidatedNelStringPartialTransformerOps[E <: String, T](private val validated: ValidatedNel[E, T])
    extends AnyVal {
  def toPartialTransformerResult: partial.Result[T] = {
    validated
      .leftMap(errs => partial.Result.Errors.fromStrings(errs.head, errs.tail: _*))
      .toPartialTransformerResult
  }
}

final class CatsValidatedNecErrorPartialTransformerOps[E <: partial.Error, T](
    private val validated: ValidatedNec[E, T]
) extends AnyVal {
  def toPartialTransformerResult: partial.Result[T] = {
    validated
      .leftMap(errs => partial.Result.Errors(errs.head, errs.tail.toList: _*))
      .toPartialTransformerResult
  }
}

final class CatsValidatedNecStringPartialTransformerOps[E <: String, T](private val validated: ValidatedNec[E, T])
    extends AnyVal {
  def toPartialTransformerResult: partial.Result[T] = {
    validated
      .leftMap(errs => partial.Result.Errors.fromStrings(errs.head, errs.tail.toList: _*))
      .toPartialTransformerResult
  }
}
