package io.scalaland.chimney

sealed trait TransformationError[+M]

object TransformationError {
  case object OptionUnwrappingError extends TransformationError[Nothing]

  case class WithMessage[+M](message: M) extends TransformationError[M]
}
