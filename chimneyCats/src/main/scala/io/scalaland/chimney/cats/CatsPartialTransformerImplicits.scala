package io.scalaland.chimney.cats

import _root_.cats.data._
import _root_.cats.kernel._
import io.scalaland.chimney._
import _root_.cats.Semigroupal

import scala.collection.compat._
import language.implicitConversions

trait CatsPartialTransformerImplicits extends Prio1 {

  implicit final val monoidPartialTransformerResultErrors: Semigroup[PartialTransformer.Result.Errors] = {
    Semigroup.instance(PartialTransformer.Result.Errors.merge)
  }

  implicit final val semigroupalPartialTransformerResult: Semigroupal[PartialTransformer.Result] = {
    new Semigroupal[PartialTransformer.Result] {
      override def product[A, B](
          fa: PartialTransformer.Result[A],
          fb: PartialTransformer.Result[B]
      ): PartialTransformer.Result[(A, B)] = {
        PartialTransformer.Result.product(fa, fb, failFast = false)
      }
    }
  }

  implicit final def catsPartialTransformerResultOps[T](
      ptr: PartialTransformer.Result[T]
  ): CatsPartialTransformerResultOps[T] =
    new CatsPartialTransformerResultOps(ptr)

  implicit final def catsValidatedPartialTransformerOps[E <: PartialTransformer.Result.Errors, T](
      validated: Validated[E, T]
  ): CatsValidatedPartialTransformerOps[E, T] =
    new CatsValidatedPartialTransformerOps(validated)

//  implicit final def catsValidatedListPartialTransformerOps[T](
//      validated: Validated[List[PartialTransformer.Error], T]
//  ): CatsValidatedListPartialTransformerOps[T] =
//    new CatsValidatedListPartialTransformerOps(validated)
//
//  implicit final def catsValidatedChainPartialTransformerOps[T](
//      validated: Validated[Chain[PartialTransformer.Error], T]
//  ): CatsValidatedChainPartialTransformerOps[T] =
//    new CatsValidatedChainPartialTransformerOps(validated)

}

trait Prio1 {
  implicit final def catsValidatedNelErrorPartialTransformerOps[T](
      validated: ValidatedNel[PartialTransformer.Error, T]
  ): CatsValidatedNelErrorPartialTransformerOps[T] =
    new CatsValidatedNelErrorPartialTransformerOps(validated)

  implicit final def catsValidatedNelStringPartialTransformerOps[E <: String, T](
      validated: ValidatedNel[E, T]
  ): CatsValidatedNelStringPartialTransformerOps[E, T] =
    new CatsValidatedNelStringPartialTransformerOps(validated)

  implicit final def catsValidatedNecPartialTransformerOps[T](
      validated: ValidatedNec[PartialTransformer.Error, T]
  ): CatsValidatedNecPartialTransformerOps[T] =
    new CatsValidatedNecPartialTransformerOps(validated)

}

final class CatsPartialTransformerResultOps[T](private val ptr: PartialTransformer.Result[T]) extends AnyVal {

  def asValidated: Validated[PartialTransformer.Result.Errors, T] = {
    Validated.fromEither(ptr.asEither)
  }

  def asValidatedList: Validated[List[PartialTransformer.Error], T] = {
    ptr.asValidated.leftMap(_.errors.toList)
  }

  def asValidatedChain: Validated[Chain[PartialTransformer.Error], T] = {
    ptr.asValidated.leftMap(errs => Chain.fromIterableOnce(errs.errors))
  }

  def asValidatedNel: ValidatedNel[PartialTransformer.Error, T] = {
    ptr.asValidated.leftMap { errs =>
      // errors collection is non-empty by design
      NonEmptyList.fromListUnsafe(errs.errors.iterator.toList)
    }
  }

  def asValidatedNec: ValidatedNec[PartialTransformer.Error, T] = {
    ptr.asValidatedNel.leftMap(NonEmptyChain.fromNonEmptyList)
  }

}

final class CatsValidatedPartialTransformerOps[E <: PartialTransformer.Result.Errors, T](
    private val validated: Validated[E, T]
) extends AnyVal {
  def toPartialTransformerResult: PartialTransformer.Result[T] = {
    PartialTransformer.Result.fromEither(validated.toEither)
  }
}

final class CatsValidatedNelErrorPartialTransformerOps[T](
    private val validated: ValidatedNel[PartialTransformer.Error, T]
) extends AnyVal {
  def toPartialTransformerResult: PartialTransformer.Result[T] = {
    validated.leftMap(errs => PartialTransformer.Result.Errors(errs.head, errs.tail: _*)).toPartialTransformerResult
  }
}

final class CatsValidatedNelStringPartialTransformerOps[E <: String, T](private val validated: ValidatedNel[E, T])
    extends AnyVal {
  def toPartialTransformerResult: PartialTransformer.Result[T] = {
    validated
      .leftMap(errs => PartialTransformer.Result.Errors.fromStrings(errs.head, errs.tail: _*))
      .toPartialTransformerResult
  }
}

final class CatsValidatedNecPartialTransformerOps[T](private val validated: ValidatedNec[PartialTransformer.Error, T])
    extends AnyVal {
  def toPartialTransformerResult: PartialTransformer.Result[T] = {
    validated
      .leftMap(errs => PartialTransformer.Result.Errors(errs.head, errs.tail.toList: _*))
      .toPartialTransformerResult
  }
}
