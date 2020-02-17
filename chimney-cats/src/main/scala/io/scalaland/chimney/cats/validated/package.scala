package io.scalaland.chimney.cats

import cats.instances.{ListInstances, VectorInstances}
import io.scalaland.chimney.typeclasses.Traverse
import io.scalaland.chimney.{TransformationContext, TransformationException}
import cats.data.Validated

package object validated extends ListInstances with VectorInstances {
  type NEC[+A] = cats.data.NonEmptyChain[A]
  val NEC = cats.data.NonEmptyChain

  type V[+A] = Validated[NEC[TransformationError], A]
  val V = Validated

  implicit val vContext: TransformationContext[V] = new TransformationContext[V] {
    override def error[A](message: String): V[A] =
      V.Invalid(NEC(TransformationError(message)))

    override def pure[A](x: A): V[A] =
      V.Valid(x)

    override def map[A, B](fa: V[A])(f: A => B): V[B] =
      fa.map(f)

    override def product[A, B](fa: V[A], fb: V[B]): V[(A, B)] =
      fa.product(fb)

    override def addPrefix[A](fa: V[A], prefix: String): V[A] =
      fa.leftMap(_.map(_.addPrefix(prefix)))
  }

  implicit def contextApplicative[F[_]](implicit ctx: TransformationContext[F]): cats.Applicative[F] =
    new cats.Applicative[F] {
      override def pure[A](x: A): F[A] =
        ctx.pure(x)

      override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
        ctx.map(ctx.product(ff, fa)) { case (func, value) => func(value) }
    }

  implicit def fromCatsTraverse[F[_]](implicit catz: cats.Traverse[F]): Traverse[F] = new Traverse[F] {
    override def mapWithIndex[A, B](fa: F[A])(f: (A, Int) => B): F[B] =
      catz.mapWithIndex(fa)(f)

    override def sequence[G[_]: TransformationContext, A](fga: F[G[A]]): G[F[A]] =
      catz.sequence(fga)
  }

  implicit class vOps[A](v: V[A]) {
    def formatErrors: Validated[NEC[String], A] =
      v.leftMap(_.map(err => s"${err.message} on ${err.errorPath.iterator.mkString(".")}"))

    def squash: Either[TransformationException, A] =
      v.formatErrors.leftMap(nec => TransformationException(nec.iterator.mkString(", "))).toEither
  }
}
