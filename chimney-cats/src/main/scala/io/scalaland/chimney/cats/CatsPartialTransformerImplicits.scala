package io.scalaland.chimney.cats

import _root_.cats.{~>, Applicative, CoflatMap, Contravariant, Monad, MonadError, Parallel}
import _root_.cats.arrow.{ArrowChoice, CommutativeArrow, FunctionK}
import io.scalaland.chimney.partial
import io.scalaland.chimney.PartialTransformer

/** @since 0.7.0 */
trait CatsPartialTransformerImplicits {

  /** @since 1.0.0 */
  implicit final val commutativeArrowChoiceForPartialTransformer
      : ArrowChoice[PartialTransformer] & CommutativeArrow[PartialTransformer] =
    new ArrowChoice[PartialTransformer] with CommutativeArrow[PartialTransformer] {
      override def lift[A, B](f: A => B): PartialTransformer[A, B] = PartialTransformer.fromFunction(f)

      override def first[A, B, C](fa: PartialTransformer[A, B]): PartialTransformer[(A, C), (B, C)] =
        (pair, failFast) => fa.transform(pair._1, failFast).map(_ -> pair._2)

      override def compose[A, B, C](
          f: PartialTransformer[B, C],
          g: PartialTransformer[A, B]
      ): PartialTransformer[A, C] =
        (a, failFast) => g.transform(a, failFast).flatMap(b => f.transform(b, failFast))

      override def choose[A, B, C, D](
          f: PartialTransformer[A, C]
      )(
          g: PartialTransformer[B, D]
      ): PartialTransformer[Either[A, B], Either[C, D]] = (ab: Either[A, B], failFast: Boolean) =>
        ab match {
          case Left(a)  => f.transform(a, failFast).map(Left(_))
          case Right(b) => g.transform(b, failFast).map(Right(_))
        }
    }

  /** @since 1.0.0 */
  implicit final def monadErrorCoflatMapForPartialTransformer[Source]
      : MonadError[PartialTransformer[Source, *], partial.Result.Errors] & CoflatMap[PartialTransformer[Source, *]] =
    new MonadError[PartialTransformer[Source, *], partial.Result.Errors] with CoflatMap[PartialTransformer[Source, *]] {
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

      override def raiseError[A](e: partial.Result.Errors): PartialTransformer[Source, A] = (_, _) => e

      override def handleErrorWith[A](fa: PartialTransformer[Source, A])(
          f: partial.Result.Errors => PartialTransformer[Source, A]
      ): PartialTransformer[Source, A] = (a, failFast) =>
        try
          fa.transform(a, failFast) match {
            case errors: partial.Result.Errors => f(errors).transform(a, failFast)
            case value                         => value
          }
        catch {
          case err: Throwable => partial.Result.fromErrorThrowable(err)
        }

      override def coflatMap[A, B](
          fa: PartialTransformer[Source, A]
      )(
          f: PartialTransformer[Source, A] => B
      ): PartialTransformer[Source, B] = (src, _) => partial.Result.fromCatching(f(fa))
    }

  /** @since 1.0.0 */
  implicit final def parallelForPartialTransformer[Source]: Parallel[PartialTransformer[Source, *]] {
    type F[A] = PartialTransformer[Source, A]
  } =
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

      val monad: Monad[PartialTransformer[Source, *]] = monadErrorCoflatMapForPartialTransformer[Source]
    }

  /** @since 1.0.0 */
  implicit final def contravariantForPartialTransformer[Target]: Contravariant[PartialTransformer[*, Target]] =
    new Contravariant[PartialTransformer[*, Target]] {
      def contramap[A, B](fa: PartialTransformer[A, Target])(f: B => A): PartialTransformer[B, Target] =
        (b, failFast) => partial.Result.fromCatching(f(b)).flatMap(a => fa.transform(a, failFast))
    }
}
