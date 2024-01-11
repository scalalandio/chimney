package io.scalaland.chimney.cats

import _root_.cats.{~>, Applicative, CoflatMap, Eval, Monad, MonadError, Parallel, Traverse}
import _root_.cats.arrow.FunctionK
import _root_.cats.data.{Chain, NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import _root_.cats.kernel.{Eq, Semigroup}
import io.scalaland.chimney.partial
import io.scalaland.chimney.partial.{AsResult, Result}

import language.implicitConversions

/** @since 1.0.0 */
trait CatsPartialResultImplicits {

  /** @since 0.7.0 */
  implicit final val monadErrorCoflatMapTraversePartialResult
      : MonadError[partial.Result, partial.Result.Errors] & CoflatMap[partial.Result] & Traverse[partial.Result] =
    new MonadError[partial.Result, partial.Result.Errors] with CoflatMap[partial.Result] with Traverse[partial.Result] {
      override def pure[A](x: A): partial.Result[A] = partial.Result.Value(x)

      override def flatMap[A, B](fa: partial.Result[A])(f: A => partial.Result[B]): partial.Result[B] = fa.flatMap(f)

      @scala.annotation.tailrec
      override def tailRecM[A, B](a: A)(f: A => partial.Result[Either[A, B]]): partial.Result[B] = f(a) match {
        case partial.Result.Value(Left(a))  => tailRecM(a)(f)
        case partial.Result.Value(Right(b)) => partial.Result.Value(b)
        case errors                         => errors.asInstanceOf[partial.Result[B]]
      }

      override def raiseError[A](e: partial.Result.Errors): partial.Result[A] = e

      override def handleErrorWith[A](
          fa: partial.Result[A]
      )(f: partial.Result.Errors => partial.Result[A]): partial.Result[A] =
        fa match {
          case ee: partial.Result.Errors => f(ee)
          case result                    => result
        }

      override def coflatMap[A, B](fa: partial.Result[A])(f: partial.Result[A] => B): partial.Result[B] =
        partial.Result.fromCatching(f(fa))

      override def traverse[G[_]: Applicative, A, B](fa: partial.Result[A])(f: A => G[B]): G[partial.Result[B]] =
        fa match {
          case partial.Result.Value(value) => Applicative[G].map(f(value))(pure)
          case errors                      => Applicative[G].pure(errors.asInstanceOf[partial.Result[B]])
        }

      override def foldLeft[A, B](fa: partial.Result[A], b: B)(f: (B, A) => B): B = fa match {
        case partial.Result.Value(value) => f(b, value)
        case _                           => b
      }

      override def foldRight[A, B](fa: partial.Result[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
        case partial.Result.Value(value) => f(value, lb)
        case _                           => lb
      }
    }

  /** @since 1.0.0 */
  implicit final val parallelSemigroupalPartialResult: Parallel[partial.Result] {
    type F[A] = partial.Result[A]
  } = new Parallel[partial.Result] {
    override type F[A] = partial.Result[A]

    override val sequential: partial.Result ~> partial.Result = FunctionK.id
    override val parallel: partial.Result ~> partial.Result = FunctionK.id

    override val applicative: Applicative[partial.Result] = new Applicative[partial.Result] {
      override def pure[A](x: A): partial.Result[A] = partial.Result.Value(x)

      override def ap[A, B](ff: partial.Result[A => B])(fa: partial.Result[A]): partial.Result[B] =
        partial.Result.map2[A => B, A, B](ff, fa, (f, a) => f(a), failFast = false)
    }

    override val monad: Monad[partial.Result] = monadErrorCoflatMapTraversePartialResult
  }

  /** @since 0.7.0 */
  implicit final val semigroupPartialResultErrors: Semigroup[partial.Result.Errors] =
    Semigroup.instance(partial.Result.Errors.merge)

  /** @since 1.0.0 */
  implicit final def eqPartialResult[A: Eq]: Eq[partial.Result[A]] = {
    case (partial.Result.Value(a1), partial.Result.Value(a2)) => Eq[A].eqv(a1, a2)
    case (e1: partial.Result.Errors, e2: partial.Result.Errors) =>
      e1.asErrorPathMessages.iterator.sameElements(e2.asErrorPathMessages.iterator)
    case _ => false
  }

  /** @since 1.0.0 */
  implicit final def eqPartialResultErrors: Eq[partial.Result.Errors] = (e1, e2) =>
    e1.asErrorPathMessages.iterator.sameElements(e2.asErrorPathMessages.iterator)

  /** @since 0.7.0 */
  implicit final def catsPartialTransformerResultOps[A](ptr: partial.Result[A]): CatsPartialTransformerResultOps[A] =
    new CatsPartialTransformerResultOps(ptr)

  /** @since 1.0.0 */
  implicit def validatedPartialResultErrorsAsResult[E <: partial.Result.Errors]: AsResult[Validated[E, *]] =
    new AsResult[Validated[E, *]] {
      def asResult[A](fa: Validated[E, A]): Result[A] = fa match {
        case Validated.Valid(a)   => partial.Result.fromValue(a)
        case Validated.Invalid(e) => e
      }
    }

  /** @since 1.0.0 */
  implicit def validatedNecPartialErrorAsResult[E <: partial.Error]: AsResult[ValidatedNec[E, *]] =
    new AsResult[ValidatedNec[E, *]] {
      def asResult[A](fa: ValidatedNec[E, A]): Result[A] = fa match {
        case Validated.Valid(a)   => partial.Result.fromValue(a)
        case Validated.Invalid(e) => partial.Result.Errors(e.head, e.tail.toList*)
      }
    }

  /** @since 1.0.0 */
  implicit def validatedNelPartialErrorAsResult[E <: partial.Error]: AsResult[ValidatedNel[E, *]] =
    new AsResult[ValidatedNel[E, *]] {
      def asResult[A](fa: ValidatedNel[E, A]): Result[A] = fa match {
        case Validated.Valid(a)   => partial.Result.fromValue(a)
        case Validated.Invalid(e) => partial.Result.Errors(e.head, e.tail*)
      }
    }

  /** @since 1.0.0 */
  implicit def validatedNecStringAsResult[E <: String]: AsResult[ValidatedNec[E, *]] =
    new AsResult[ValidatedNec[E, *]] {
      def asResult[A](fa: ValidatedNec[E, A]): Result[A] = fa match {
        case Validated.Valid(a)   => partial.Result.fromValue(a)
        case Validated.Invalid(e) => partial.Result.fromErrorStrings(e.head, e.tail.toList*)
      }
    }

  /** @since 1.0.0 */
  implicit def validatedNelStringAsResult[E <: String]: AsResult[ValidatedNel[E, *]] =
    new AsResult[ValidatedNel[E, *]] {
      def asResult[A](fa: ValidatedNel[E, A]): Result[A] = fa match {
        case Validated.Valid(a)   => partial.Result.fromValue(a)
        case Validated.Invalid(e) => partial.Result.fromErrorStrings(e.head, e.tail*)
      }
    }
}

/** @since 0.7.0 */
final class CatsPartialTransformerResultOps[A](private val ptr: partial.Result[A]) extends AnyVal {

  /** @since 0.7.0 */
  def asValidated: Validated[partial.Result.Errors, A] =
    Validated.fromEither(ptr.asEither)

  /** @since 0.7.0 */
  def asValidatedNec: ValidatedNec[partial.Error, A] =
    asValidatedNel.leftMap(NonEmptyChain.fromNonEmptyList)

  /** @since 0.7.0 */
  def asValidatedNel: ValidatedNel[partial.Error, A] =
    // errors collection is non-empty by design
    asValidated.leftMap(errs => NonEmptyList.fromListUnsafe(errs.errors.iterator.toList))

  /** @since 0.7.0 */
  def asValidatedChain: Validated[Chain[partial.Error], A] =
    asValidated.leftMap(errs => Chain.fromIterableOnce(errs.errors))

  /** @since 0.7.0 */
  def asValidatedList: Validated[List[partial.Error], A] =
    asValidated.leftMap(_.errors.toList)
}
