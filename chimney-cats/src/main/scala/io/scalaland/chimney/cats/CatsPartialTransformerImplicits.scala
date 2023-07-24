package io.scalaland.chimney.cats

import _root_.cats.data.{Chain, NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import _root_.cats.kernel.Semigroup
import _root_.cats.Applicative
import io.scalaland.chimney.partial

import language.implicitConversions

/** @since 0.7.0 */
trait CatsPartialTransformerImplicits extends CatsPartialTransformerLowPriorityImplicits1 {

  /** @since 0.7.0 */
  implicit final val semigroupPartialResultErrors: Semigroup[partial.Result.Errors] =
    Semigroup.instance(partial.Result.Errors.merge)

  /** @since 0.7.0 */
  implicit final val applicativePartialResult: Applicative[partial.Result] =
    new Applicative[partial.Result] {
      final override def pure[A](x: A): partial.Result[A] =
        partial.Result.fromValue(x)

      final override def ap[A, B](ff: partial.Result[A => B])(fa: partial.Result[A]): partial.Result[B] =
        partial.Result.map2[A => B, A, B](ff, fa, { case (f, a) => f(a) }, failFast = false)
    }

  /** @since 0.7.0 */
  implicit final def catsPartialTransformerResultOps[T](
      ptr: partial.Result[T]
  ): CatsPartialTransformerResultOps[T] =
    new CatsPartialTransformerResultOps(ptr)

  /** @since 0.7.0 */
  implicit final def catsValidatedPartialTransformerOps[E <: partial.Result.Errors, T](
      validated: Validated[E, T]
  ): CatsValidatedPartialTransformerOps[E, T] =
    new CatsValidatedPartialTransformerOps(validated)
}

private[cats] trait CatsPartialTransformerLowPriorityImplicits1 {

  /** @since 0.7.0 */
  implicit final def catsValidatedNelErrorPartialTransformerOps[E <: partial.Error, T](
      validated: ValidatedNel[E, T]
  ): CatsValidatedNelErrorPartialTransformerOps[E, T] =
    new CatsValidatedNelErrorPartialTransformerOps(validated)

  /** @since 0.7.0 */
  implicit final def catsValidatedNelStringPartialTransformerOps[E <: String, T](
      validated: ValidatedNel[E, T]
  ): CatsValidatedNelStringPartialTransformerOps[E, T] =
    new CatsValidatedNelStringPartialTransformerOps(validated)

  /** @since 0.7.0 */
  implicit final def catsValidatedNecErrorPartialTransformerOps[E <: partial.Error, T](
      validated: ValidatedNec[E, T]
  ): CatsValidatedNecErrorPartialTransformerOps[E, T] =
    new CatsValidatedNecErrorPartialTransformerOps(validated)

  /** @since 0.7.0 */
  implicit final def catsValidatedNecStringPartialTransformerOps[E <: String, T](
      validated: ValidatedNec[E, T]
  ): CatsValidatedNecStringPartialTransformerOps[E, T] =
    new CatsValidatedNecStringPartialTransformerOps(validated)
}

/** @since 0.7.0 */
final class CatsPartialTransformerResultOps[T](private val ptr: partial.Result[T]) extends AnyVal {

  /** @since 0.7.0 */
  def asValidated: Validated[partial.Result.Errors, T] =
    Validated.fromEither(ptr.asEither)

  /** @since 0.7.0 */
  def asValidatedList: Validated[List[partial.Error], T] =
    asValidated.leftMap(_.errors.toList)

  /** @since 0.7.0 */
  def asValidatedChain: Validated[Chain[partial.Error], T] =
    asValidated.leftMap(errs => Chain.fromIterableOnce(errs.errors))

  /** @since 0.7.0 */
  def asValidatedNel: ValidatedNel[partial.Error, T] =
    asValidated.leftMap { errs =>
      // errors collection is non-empty by design
      NonEmptyList.fromListUnsafe(errs.errors.iterator.toList)
    }

  /** @since 0.7.0 */
  def asValidatedNec: ValidatedNec[partial.Error, T] =
    asValidatedNel.leftMap(NonEmptyChain.fromNonEmptyList)
}

/** @since 0.7.0 */
final class CatsValidatedPartialTransformerOps[E <: partial.Result.Errors, T](
    private val validated: Validated[E, T]
) extends AnyVal {

  /** @since 0.7.0 */
  def toPartialResult: partial.Result[T] =
    partial.Result.fromEither(validated.toEither)
}

/** @since 0.7.0 */
final class CatsValidatedNelErrorPartialTransformerOps[E <: partial.Error, T](
    private val validated: ValidatedNel[E, T]
) extends AnyVal {

  /** @since 0.7.0 */
  def toPartialResult: partial.Result[T] =
    validated.leftMap(errs => partial.Result.Errors(errs.head, errs.tail*)).toPartialResult
}

/** @since 0.7.0 */
final class CatsValidatedNelStringPartialTransformerOps[E <: String, T](private val validated: ValidatedNel[E, T])
    extends AnyVal {

  /** @since 0.7.0 */
  def toPartialResult: partial.Result[T] =
    validated
      .leftMap(errs => partial.Result.Errors.fromStrings(errs.head, errs.tail*))
      .toPartialResult
}

/** @since 0.7.0 */
final class CatsValidatedNecErrorPartialTransformerOps[E <: partial.Error, T](
    private val validated: ValidatedNec[E, T]
) extends AnyVal {

  /** @since 0.7.0 */
  def toPartialResult: partial.Result[T] =
    validated
      .leftMap(errs => partial.Result.Errors(errs.head, errs.tail.toList*))
      .toPartialResult
}

/** @since 0.7.0 */
final class CatsValidatedNecStringPartialTransformerOps[E <: String, T](private val validated: ValidatedNec[E, T])
    extends AnyVal {

  /** @since 0.7.0 */
  def toPartialResult: partial.Result[T] =
    validated
      .leftMap(errs => partial.Result.Errors.fromStrings(errs.head, errs.tail.toList*))
      .toPartialResult
}
