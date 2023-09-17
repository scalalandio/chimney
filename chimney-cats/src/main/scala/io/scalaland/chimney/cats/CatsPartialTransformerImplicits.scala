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
  implicit final def catsPartialTransformerResultOps[A](
      ptr: partial.Result[A]
  ): CatsPartialTransformerResultOps[A] =
    new CatsPartialTransformerResultOps(ptr)

  /** @since 0.7.0 */
  implicit final def catsValidatedPartialTransformerOps[E <: partial.Result.Errors, A](
      validated: Validated[E, A]
  ): CatsValidatedPartialTransformerOps[E, A] =
    new CatsValidatedPartialTransformerOps(validated)
}

private[cats] trait CatsPartialTransformerLowPriorityImplicits1 {

  /** @since 0.7.0 */
  implicit final def catsValidatedNelErrorPartialTransformerOps[E <: partial.Error, A](
      validated: ValidatedNel[E, A]
  ): CatsValidatedNelErrorPartialTransformerOps[E, A] =
    new CatsValidatedNelErrorPartialTransformerOps(validated)

  /** @since 0.7.0 */
  implicit final def catsValidatedNelStringPartialTransformerOps[E <: String, A](
      validated: ValidatedNel[E, A]
  ): CatsValidatedNelStringPartialTransformerOps[E, A] =
    new CatsValidatedNelStringPartialTransformerOps(validated)

  /** @since 0.7.0 */
  implicit final def catsValidatedNecErrorPartialTransformerOps[E <: partial.Error, A](
      validated: ValidatedNec[E, A]
  ): CatsValidatedNecErrorPartialTransformerOps[E, A] =
    new CatsValidatedNecErrorPartialTransformerOps(validated)

  /** @since 0.7.0 */
  implicit final def catsValidatedNecStringPartialTransformerOps[E <: String, A](
      validated: ValidatedNec[E, A]
  ): CatsValidatedNecStringPartialTransformerOps[E, A] =
    new CatsValidatedNecStringPartialTransformerOps(validated)
}

/** @since 0.7.0 */
final class CatsPartialTransformerResultOps[A](private val ptr: partial.Result[A]) extends AnyVal {

  /** @since 0.7.0 */
  def asValidated: Validated[partial.Result.Errors, A] =
    Validated.fromEither(ptr.asEither)

  /** @since 0.7.0 */
  def asValidatedList: Validated[List[partial.Error], A] =
    asValidated.leftMap(_.errors.toList)

  /** @since 0.7.0 */
  def asValidatedChain: Validated[Chain[partial.Error], A] =
    asValidated.leftMap(errs => Chain.fromIterableOnce(errs.errors))

  /** @since 0.7.0 */
  def asValidatedNel: ValidatedNel[partial.Error, A] =
    asValidated.leftMap { errs =>
      // errors collection is non-empty by design
      NonEmptyList.fromListUnsafe(errs.errors.iterator.toList)
    }

  /** @since 0.7.0 */
  def asValidatedNec: ValidatedNec[partial.Error, A] =
    asValidatedNel.leftMap(NonEmptyChain.fromNonEmptyList)
}

/** @since 0.7.0 */
final class CatsValidatedPartialTransformerOps[E <: partial.Result.Errors, A](
    private val validated: Validated[E, A]
) extends AnyVal {

  /** @since 0.7.0 */
  def toPartialResult: partial.Result[A] =
    partial.Result.fromEither(validated.toEither)
}

/** @since 0.7.0 */
final class CatsValidatedNelErrorPartialTransformerOps[E <: partial.Error, A](
    private val validated: ValidatedNel[E, A]
) extends AnyVal {

  /** @since 0.7.0 */
  def toPartialResult: partial.Result[A] =
    validated.leftMap(errs => partial.Result.Errors(errs.head, errs.tail*)).toPartialResult
}

/** @since 0.7.0 */
final class CatsValidatedNelStringPartialTransformerOps[E <: String, A](private val validated: ValidatedNel[E, A])
    extends AnyVal {

  /** @since 0.7.0 */
  def toPartialResult: partial.Result[A] =
    validated
      .leftMap(errs => partial.Result.Errors.fromStrings(errs.head, errs.tail*))
      .toPartialResult
}

/** @since 0.7.0 */
final class CatsValidatedNecErrorPartialTransformerOps[E <: partial.Error, A](
    private val validated: ValidatedNec[E, A]
) extends AnyVal {

  /** @since 0.7.0 */
  def toPartialResult: partial.Result[A] =
    validated
      .leftMap(errs => partial.Result.Errors(errs.head, errs.tail.toList*))
      .toPartialResult
}

/** @since 0.7.0 */
final class CatsValidatedNecStringPartialTransformerOps[E <: String, A](private val validated: ValidatedNec[E, A])
    extends AnyVal {

  /** @since 0.7.0 */
  def toPartialResult: partial.Result[A] =
    validated
      .leftMap(errs => partial.Result.Errors.fromStrings(errs.head, errs.tail.toList*))
      .toPartialResult
}
