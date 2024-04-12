package io.scalaland.chimney.partial

/** Representation of partial transformation error message
  *
  * @since 0.7.0
  */
sealed trait ErrorMessage {

  /** Returns string representation of error message
    *
    * @since 0.7.0
    */
  final def asString: String = this match {
    case ErrorMessage.EmptyValue                  => "empty value"
    case ErrorMessage.NotDefinedAt(value)         => s"not defined at $value"
    case ErrorMessage.StringMessage(message)      => message
    case ErrorMessage.ThrowableMessage(throwable) => throwable.getMessage
  }
}

object ErrorMessage {

  /** Represents empty value error
    *
    * @since 0.7.0
    */
  case object EmptyValue extends ErrorMessage

  /** Represents partial function is not defined for specific argument error
    *
    * @param arg
    *   argument where partial function is not defined for
    *
    * @since 0.7.0
    */
  final case class NotDefinedAt(arg: Any) extends ErrorMessage

  /** Represents custom string error
    *
    * @param message
    *   custom string error message
    *
    * @since 0.7.0
    */
  final case class StringMessage(message: String) extends ErrorMessage

  /** Represents throwable error
    *
    * @param throwable
    *   custom throwable object
    *
    * @since 0.7.0
    */
  final case class ThrowableMessage(throwable: Throwable) extends ErrorMessage
}
