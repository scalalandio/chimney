package io.scalaland.chimney.internal.compiletime.fp

trait Parallel[F[_]] extends Applicative[F] {

  def parMap2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
}
object Parallel {

  def apply[F[_]](implicit F: Parallel[F]): Parallel[F] = F

  final class Ops[F[_], A](private val fa: F[A]) extends AnyVal {

    def parMap2[B, C](fb: F[B])(f: (A, B) => C)(implicit F: Parallel[F]): F[C] = F.parMap2(fa, fb)(f)
  }
}
