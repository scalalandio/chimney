package io.scalaland.chimney.internal.compiletime.fp

private[compiletime] trait Traverse[F[_]] extends Functor[F] {

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Applicative.Id, A, B](fa)(f)(Applicative.IdentityApplicative)
}
private[compiletime] object Traverse {

  def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F

  final class Ops[F[_], A](private val fa: F[A]) extends AnyVal {

    def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[F[B]] =
      F.traverse(fa)(f)
  }

  final class SequenceOps[F[_], G[_], A](private val fga: F[G[A]]) extends AnyVal {

    def sequence(implicit F: Traverse[F], G: Applicative[G]): G[F[A]] =
      F.traverse(fga)(identity)
  }
}
