package io.scalaland.chimney.internal.compiletime.fp

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]
}
object Functor {

  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  final class Ops[F[_], A](private val fa: F[A]) extends AnyVal {

    def map[B](f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)
  }
}
