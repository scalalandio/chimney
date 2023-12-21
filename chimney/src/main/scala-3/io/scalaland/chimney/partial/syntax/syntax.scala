package io.scalaland.chimney.partial.syntax

import io.scalaland.chimney.partial.{AsResult, Error, Result}

// TODO: docs

extension [A](option: Option[A])
  
  def orErrorAsResult(onEmpty: => Error): Result[A] = Result.fromOptionOrError(option, onEmpty)
  
  def orStringAsResult(onEmpty: => String): Result[A] = Result.fromOptionOrString(option, onEmpty)
  
  def orThrowableAsResult(onEmpty: => Throwable): Result[A] = Result.fromOptionOrThrowable(option, onEmpty)

extension [F[_], A](fa: F[A])
  def asResult(using F: AsResult[F]): Result[A] = F.asResult(fa)
