package io.scalaland.chimney

import _root_.cats.data._
import _root_.cats.kernel.Semigroup
import _root_.cats.{Applicative, ApplicativeError}

import scala.collection.compat._

package object cats extends LowPriorityImplicits {

  // normally, these instances are not needed, but few tests with .enableUnsafeOption fail to compile without them
  implicit def TransformerFValidatedNecSupport[E]: TransformerFSupport[ValidatedNec[E, +*]] =
    TransformerFValidatedSupport[NonEmptyChain[E]]

  implicit def TransformerFValidatedNelSupport[E]: TransformerFSupport[ValidatedNel[E, +*]] =
    TransformerFValidatedSupport[NonEmptyList[E]]

  implicit def TransformerFErrorValidatedNecSupport[M]
      : TransformerFErrorPathSupport[ValidatedNec[TransformationError[M], +*]] =
    TransformerFValidatedErrorPathSupport[ValidatedNec[TransformationError[M], +*], NonEmptyChain, M]

  implicit def TransformerFErrorValidatedNelSupport[M]
      : TransformerFErrorPathSupport[ValidatedNel[TransformationError[M], +*]] =
    TransformerFValidatedErrorPathSupport[ValidatedNel[TransformationError[M], +*], NonEmptyList, M]
}

trait LowPriorityImplicits {

  implicit def TransformerFValidatedSupport[EE: Semigroup]: TransformerFSupport[Validated[EE, +*]] =
    new TransformerFSupport[Validated[EE, +*]] {
      def pure[A](value: A): Validated[EE, A] =
        Validated.Valid(value)

      def product[A, B](fa: Validated[EE, A], fb: => Validated[EE, B]): Validated[EE, (A, B)] =
        fa.product(fb)

      def map[A, B](fa: Validated[EE, A], f: A => B): Validated[EE, B] =
        fa.map(f)

      def traverse[M, A, B](it: Iterator[A], f: A => Validated[EE, B])(
          implicit fac: Factory[B, M]
      ): Validated[EE, M] = {
        val (valid, invalid) = it.map(f).partition(_.isValid)
        Semigroup[EE].combineAllOption(invalid.collect { case Validated.Invalid(e) => e }) match {
          case Some(errors) =>
            Validated.Invalid(errors)
          case None =>
            val b = fac.newBuilder
            b ++= valid.collect { case Validated.Valid(b) => b }
            Validated.Valid(b.result())
        }
      }
    }

  implicit def TransformerFValidatedErrorPathSupport[F[+_], EE[_]: Applicative, M](
      implicit applicativeError: ApplicativeError[F, EE[TransformationError[M]]]
  ): TransformerFErrorPathSupport[F] =
    new TransformerFErrorPathSupport[F] {
      override def addPath[A](fa: F[A], node: ErrorPathNode): F[A] =
        applicativeError.handleErrorWith(fa)(ee => applicativeError.raiseError(Applicative[EE].map(ee)(_.prepend(node)))
        )
    }
}
