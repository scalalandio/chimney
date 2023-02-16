package io.scalaland.chimney.partial

/** Data structure for representing path-annotated error
  *
  * @param message error message
  * @param path error path annotation
  *
  * @since 0.7.0
  */
final case class Error(message: ErrorMessage, path: Path = Path.Empty) {

  /** Returns a tuple with conventional string representation of a path and error message
    *
    * @since 0.7.0
    */
  def asErrorPathMessage: (String, ErrorMessage) = (path.asString, message)

  /** Prepends path with a given path element
    *
    * @param pathElement path element to be prepended
    * @return error with a path prepended with provided path element
    *
    * @since 0.7.0
    */
  def prependErrorPath(pathElement: PathElement): Error = Error(message, path.prepend(pathElement))
}

object Error {

  /** Empty value error with an empty path
    *
    * @since 0.7.0
    */
  final def ofEmptyValue: Error =
    Error(ErrorMessage.EmptyValue)

  /** Partial function not defined at given argument error with an empty path
    *
    * @param arg argument where partial function is not defined for
    *
    * @since 0.7.0
    */
  final def ofNotDefinedAt(arg: Any): Error =
    Error(ErrorMessage.NotDefinedAt(arg))

  /** Custom string error with an empty path
    *
    * @param message custom string error message
    *
    * @since 0.7.0
    */
  final def ofString(message: String): Error =
    Error(ErrorMessage.StringMessage(message))

  /** Throwable error with an empty path
    *
    * @param throwable custom throwable object
    *
    * @since 0.7.0
    */
  final def ofThrowable(throwable: Throwable): Error =
    Error(ErrorMessage.ThrowableMessage(throwable))
}
