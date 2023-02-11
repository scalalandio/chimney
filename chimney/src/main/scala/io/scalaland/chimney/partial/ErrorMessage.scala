package io.scalaland.chimney.partial

sealed trait ErrorMessage {
  final def asString: String = this match {
    case ErrorMessage.EmptyValue                  => "empty value"
    case ErrorMessage.NotDefinedAt(value)         => s"not defined at $value"
    case ErrorMessage.StringMessage(message)      => message
    case ErrorMessage.ThrowableMessage(throwable) => throwable.getMessage
  }
}

object ErrorMessage {
  final case object EmptyValue extends ErrorMessage

  final case class NotDefinedAt(value: Any) extends ErrorMessage

  final case class StringMessage(message: String) extends ErrorMessage

  final case class ThrowableMessage(throwable: Throwable) extends ErrorMessage
}
