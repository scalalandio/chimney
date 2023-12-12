package io.scalaland.chimney.cats

import _root_.cats.{~>, Applicative, CoflatMap, Contravariant, Eval, Monad, MonadError, Parallel, Semigroupal, Traverse}
import _root_.cats.arrow.{Arrow, FunctionK}
import _root_.cats.data.{Chain, NonEmptyChain, NonEmptyList, Validated, ValidatedNec, ValidatedNel}
import _root_.cats.kernel.{Eq, Semigroup}
import io.scalaland.chimney.partial
import io.scalaland.chimney.PartialTransformer

import language.implicitConversions

/** @since 0.7.0 */
trait CatsPartialTransformerImplicits extends CatsPartialTransformerLowPriorityImplicits1 {

  // TODO: @since
  implicit final val arrowPartialTransformer: Arrow[PartialTransformer] = new Arrow[PartialTransformer] {
    override def lift[A, B](f: A => B): PartialTransformer[A, B] = PartialTransformer.fromFunction(f)

    override def first[A, B, C](fa: PartialTransformer[A, B]): PartialTransformer[(A, C), (B, C)] =
      (pair, failFast) => fa.transform(pair._1, failFast).map(_ -> pair._2)

    override def compose[A, B, C](f: PartialTransformer[B, C], g: PartialTransformer[A, B]): PartialTransformer[A, C] =
      (a, failFast) => g.transform(a, failFast).flatMap(b => f.transform(b, failFast))
  }

  // TODO: @since
  implicit final def monadPartialTransformer[Source]: Monad[PartialTransformer[Source, *]] =
    new Monad[PartialTransformer[Source, *]] {
      override def pure[A](x: A): PartialTransformer[Source, A] = (_, _) => partial.Result.Value(x)

      override def flatMap[A, B](fa: PartialTransformer[Source, A])(
          f: A => PartialTransformer[Source, B]
      ): PartialTransformer[Source, B] =
        (src, failFast) => fa.transform(src, failFast).flatMap(a => f(a).transform(src, failFast))

      override def tailRecM[A, B](a: A)(
          f: A => PartialTransformer[Source, Either[A, B]]
      ): PartialTransformer[Source, B] =
        (src, failFast) => {
          def run(a1: A) = partial.Result.fromCatching(f(a1).transform(src, failFast)).flatMap(identity)
          @scala.annotation.tailrec
          def loop(a1: A): partial.Result[B] = run(a1) match {
            case partial.Result.Value(Left(a2)) => loop(a2)
            case partial.Result.Value(Right(b)) => partial.Result.Value(b)
            case errors                         => errors.asInstanceOf[partial.Result[B]]
          }
          loop(a)
        }
    }

  // TODO: @since
  implicit final def parallelPartialTransformer[Source]: Parallel[PartialTransformer[Source, *]] =
    new Parallel[PartialTransformer[Source, *]] {
      override type F[A] = PartialTransformer[Source, A]

      def parallel: PartialTransformer[Source, *] ~> PartialTransformer[Source, *] = FunctionK.id
      def sequential: PartialTransformer[Source, *] ~> PartialTransformer[Source, *] = FunctionK.id

      val applicative: Applicative[PartialTransformer[Source, *]] = new Applicative[PartialTransformer[Source, *]] {
        def pure[A](x: A): PartialTransformer[Source, A] = (_, _) => partial.Result.Value(x)

        def ap[A, B](ff: PartialTransformer[Source, A => B])(
            fa: PartialTransformer[Source, A]
        ): PartialTransformer[Source, B] =
          (src, failFast) =>
            partial.Result.map2[A => B, A, B](
              ff.transform(src, failFast),
              fa.transform(src, failFast),
              (f, a) => f(a),
              failFast
            )
      }

      val monad: Monad[PartialTransformer[Source, *]] = monadPartialTransformer[Source]
    }

  // TODO: @since
  implicit final def contravariantPartialTransformer[Target]: Contravariant[PartialTransformer[*, Target]] =
    new Contravariant[PartialTransformer[*, Target]] {
      def contramap[A, B](fa: PartialTransformer[A, Target])(f: B => A): PartialTransformer[B, Target] =
        (b, failFast) => partial.Result.fromCatching(f(b)).flatMap(a => fa.transform(a, failFast))
    }

  // TODO: move partial.Result to separate traitge)

  // TODO: @since
  implicit final val parallelPartialResult: Parallel[partial.Result] & Semigroupal[partial.Result] =
    new Parallel[partial.Result] with Semigroupal[partial.Result] {
      override type F[A] = partial.Result[A]

      override val sequential: partial.Result ~> partial.Result = FunctionK.id
      override val parallel: partial.Result ~> partial.Result = FunctionK.id

      override val applicative: Applicative[partial.Result] = new Applicative[partial.Result] {
        override def pure[A](x: A): partial.Result[A] = partial.Result.Value(x)

        override def ap[A, B](ff: partial.Result[A => B])(fa: partial.Result[A]): partial.Result[B] =
          partial.Result.map2[A => B, A, B](ff, fa, (f, a) => f(a), failFast = false)
      }

      override val monad: Monad[partial.Result] = applicativePartialResult

      override def product[A, B](
          fa: partial.Result[A],
          fb: partial.Result[B]
      ): partial.Result[(A, B)] = partial.Result.product(fa, fb, failFast = false)
    }

  /** @since 0.7.0 */
  implicit final val semigroupPartialResultErrors: Semigroup[partial.Result.Errors] =
    Semigroup.instance(partial.Result.Errors.merge)

  // TODO: @since
  implicit final def eqPartialResult[A: Eq]: Eq[partial.Result[A]] = {
    case (partial.Result.Value(a1), partial.Result.Value(a2)) => Eq[A].eqv(a1, a2)
    case (e1: partial.Result.Errors, e2: partial.Result.Errors) =>
      e1.asErrorPathMessages.iterator.sameElements(e2.asErrorPathMessages.iterator)
    case _ => false
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

  // TODO: rename in 1.0.0
  /** @since 0.7.0 */
  implicit final val applicativePartialResult
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
