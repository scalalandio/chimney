package io.scalaland.chimney.partial

final case class Error(message: ErrorMessage, path: ErrorPath = ErrorPath.Empty) {
  def asErrorPathMessageString: (String, String) = (path.asString, message.asString)

  def prependErrorPath(pathElement: PathElement): Error = Error(message, path.prepend(pathElement))
}

object Error {
  final def ofEmptyValue: Error =
    Error(ErrorMessage.EmptyValue)

  final def ofNotDefinedAt(value: Any): Error =
    Error(ErrorMessage.NotDefinedAt(value))

  final def ofString(message: String): Error =
    Error(ErrorMessage.StringMessage(message))

  final def ofThrowable(throwable: Throwable): Error =
    Error(ErrorMessage.ThrowableMessage(throwable))
}
