package io.scalaland.chimney.typeclasses

import io.scalaland.chimney.TransformationContext

trait Traverse[F[_]] {
  def mapWithIndex[A, B](fa: F[A])(f: (A, Int) => B): F[B]

  def sequence[G[_]: TransformationContext, A](fga: F[G[A]]): G[F[A]]

  def traverseWithPrefix[G[_]: TransformationContext, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(mapWithIndex(fa)((a, index) => TransformationContext[G].addPrefix(f(a), s"[$index]")))
}

object Traverse {
  def apply[F[_]](implicit inst: Traverse[F]): Traverse[F] =
    inst
}
