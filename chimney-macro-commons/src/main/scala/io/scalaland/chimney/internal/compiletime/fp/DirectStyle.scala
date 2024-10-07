package io.scalaland.chimney.internal.compiletime.fp

import scala.util.control.NoStackTrace

trait DirectStyle[F[_]] {

  def asyncUnsafe[A](thunk: => A): F[A]
  def awaitUnsafe[A](value: F[A]): A
  def async[A, B](await: (F[A] => A) => B): F[B] = asyncUnsafe(await(awaitUnsafe))
}
object DirectStyle {

  def apply[F[_]](implicit F: DirectStyle[F]): DirectStyle[F] = F

  def directStyleForEither[Errors]: DirectStyle[Either[Errors, *]] = new DirectStyle[Either[Errors, *]] {
    private case class PassErrors(error: Errors) extends NoStackTrace

    def asyncUnsafe[A](thunk: => A): Either[Errors, A] = try
      Right(thunk)
    catch {
      case PassErrors(error) => Left(error)
    }
    def awaitUnsafe[A](value: Either[Errors, A]): A = value match {
      case Left(error)  => throw PassErrors(error)
      case Right(value) => value
    }
  }
}
