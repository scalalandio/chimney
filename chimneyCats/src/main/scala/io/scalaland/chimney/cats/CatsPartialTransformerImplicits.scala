package io.scalaland.chimney.cats

import _root_.cats.data._
import _root_.cats.kernel._
import io.scalaland.chimney._
import _root_.cats.Semigroupal

import scala.collection.compat._
import language.implicitConversions

trait CatsPartialTransformerImplicits {

  implicit final val monoidPartialTransformerResultErrors: Monoid[PartialTransformer.Result.Errors] = {
    Monoid.instance(
      PartialTransformer.Result.Errors(Iterable.empty),
      PartialTransformer.Result.Errors.merge
    )
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

  implicit final def catsValidatedPartialTransformerOps[T](
      validated: Validated[PartialTransformer.Result.Errors, T]
  ): CatsValidatedPartialTransformerOps[T] =
    new CatsValidatedPartialTransformerOps(validated)

  implicit final def catsValidatedListPartialTransformerOps[T](
      validated: Validated[List[PartialTransformer.Error], T]
  ): CatsValidatedListPartialTransformerOps[T] =
    new CatsValidatedListPartialTransformerOps(validated)

  implicit final def catsValidatedChainPartialTransformerOps[T](
      validated: Validated[Chain[PartialTransformer.Error], T]
  ): CatsValidatedChainPartialTransformerOps[T] =
    new CatsValidatedChainPartialTransformerOps(validated)

  implicit final def catsValidatedNelPartialTransformerOps[T](
      validated: ValidatedNel[PartialTransformer.Error, T]
  ): CatsValidatedNelPartialTransformerOps[T] =
    new CatsValidatedNelPartialTransformerOps(validated)

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
}

final class CatsValidatedPartialTransformerOps[T](private val validated: Validated[PartialTransformer.Result.Errors, T])
    extends AnyVal {
  def toPartialTransformerResult: PartialTransformer.Result[T] = {
    PartialTransformer.Result.fromEither(validated.toEither)
  }
}

final class CatsValidatedListPartialTransformerOps[T](private val validated: Validated[List[PartialTransformer.Error], T])
    extends AnyVal {
  def toPartialTransformerResult: PartialTransformer.Result[T] = {
    validated.leftMap(errs => PartialTransformer.Result.Errors(errs)).toPartialTransformerResult
  }
}

final class CatsValidatedChainPartialTransformerOps[T](private val validated: Validated[Chain[PartialTransformer.Error], T])
    extends AnyVal {
  def toPartialTransformerResult: PartialTransformer.Result[T] = {
    validated.leftMap(errs => PartialTransformer.Result.Errors(errs.iterator.to(Iterable))).toPartialTransformerResult
  }
}

final class CatsValidatedNelPartialTransformerOps[T](private val validated: ValidatedNel[PartialTransformer.Error, T])
    extends AnyVal {
  def toPartialTransformerResult: PartialTransformer.Result[T] = {
    validated.leftMap(errs => PartialTransformer.Result.Errors(errs.iterator.to(Iterable))).toPartialTransformerResult
  }
}

final class CatsValidatedNecPartialTransformerOps[T](private val validated: ValidatedNec[PartialTransformer.Error, T])
    extends AnyVal {
  def toPartialTransformerResult: PartialTransformer.Result[T] = {
    validated.leftMap(errs => PartialTransformer.Result.Errors(errs.iterator.to(Iterable))).toPartialTransformerResult
  }
}
