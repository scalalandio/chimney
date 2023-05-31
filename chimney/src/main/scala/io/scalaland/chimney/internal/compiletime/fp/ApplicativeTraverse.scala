package io.scalaland.chimney.internal.compiletime.fp

private[compiletime] trait ApplicativeTraverse[F[_]] extends Traverse[F] with Applicative[F] {

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Applicative.Id, A, B](fa)(f)(Applicative.IdentityApplicative)
}
private[compiletime] object ApplicativeTraverse {

  def apply[F[_]](implicit F: ApplicativeTraverse[F]): ApplicativeTraverse[F] = F
}
