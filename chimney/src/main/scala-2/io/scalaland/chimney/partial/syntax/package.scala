package io.scalaland.chimney.partial

// TODO: docs

package object syntax {

  implicit class PartialResultOptionOps[A](private val option: Option[A]) extends AnyVal {

    def orErrorAsResult(onEmpty: => Error): Result[A] = Result.fromOptionOrError(option, onEmpty)

    def orStringAsResult(onEmpty: => String): Result[A] = Result.fromOptionOrString(option, onEmpty)

    def orThrowableAsResult(onEmpty: => Throwable): Result[A] = Result.fromOptionOrThrowable(option, onEmpty)
  }

  implicit class PartialResultAsResultOps[F[_], A](private val fa: F[A]) extends AnyVal {

    def asResult(implicit F: AsResult[F]): Result[A] = F.asResult(fa)
  }
}
