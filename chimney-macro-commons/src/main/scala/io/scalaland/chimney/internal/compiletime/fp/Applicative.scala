package io.scalaland.chimney.internal.compiletime.fp

trait Applicative[F[_]] extends Functor[F] {

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def pure[A](a: A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, pure(()))((a, _) => f(a))
}
object Applicative {

  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  type Id[A] = A
  implicit val IdentityApplicative: Applicative[Id] = new Applicative[Id] {

    def map2[A, B, C](fa: A, fb: B)(f: (A, B) => C): C = f(fa, fb)

    def pure[A](a: A): A = a
  }

  final class PureOps[A](private val a: A) extends AnyVal {

    def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
  }

  final class Ops[F[_], A](private val fa: F[A]) extends AnyVal {

    def map2[B, C](fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] = F.map2(fa, fb)(f)
  }
}
