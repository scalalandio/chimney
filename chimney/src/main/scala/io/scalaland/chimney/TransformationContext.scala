package io.scalaland.chimney

trait TransformationContext[F[_]] {
  def pure[A](x: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  def addPrefix[A](fa: F[A], prefix: String): F[A]

  def error[A](message: String): F[A]
}

object TransformationContext {
  @inline
  def apply[F[_]](implicit inst: TransformationContext[F]): TransformationContext[F] =
    inst
}
