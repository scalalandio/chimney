package io.scalaland.chimney.partial

/** Data structure for representing path-annotated error
  *
  * @param message
  *   error message
  * @param path
  *   error path annotation
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
    * @param pathElement
    *   path element to be prepended
    * @return
    *   error with a path prepended with provided path element
    *
    * @since 0.7.0
    */
  def prependErrorPath(pathElement: PathElement): Error = Error(message, path.prepend(pathElement))

  /** Unseals the [[io.scalaland.chimney.partial.Path]] of current [[io.scalaland.chimney.partial.Error]].
    *
    * When derivation is building up the result it automatically appends fields/indices/map keys - however values
    * obtained with withFieldComputed(Partial)(From) contains the whole Path already, so [[prependErrorPath]] should be
    * a noop for them.
    *
    * However, this path can only be precomputed only up to the boundaries of a
    * [[io.scalaland.chimney.PartialTransformer]], and when one transformer calls another, path should be appended
    * again. This method allows this.
    *
    * @return
    *   error with a path prepended with provided path element
    *
    * @since 1.6.0
    */
  def unsealErrorPath(): Error = { path.unsealPath(); this }
}

object Error {

  /** Empty value error with an empty path
    *
    * @since 0.7.0
    */
  final def fromEmptyValue: Error =
    Error(ErrorMessage.EmptyValue)

  /** Partial function not defined at given argument error with an empty path
    *
    * @param arg
    *   argument where partial function is not defined for
    *
    * @since 0.7.0
    */
  final def fromNotDefinedAt(arg: Any): Error =
    Error(ErrorMessage.NotDefinedAt(arg))

  /** Custom string error with an empty path
    *
    * @param message
    *   custom string error message
    *
    * @since 0.7.0
    */
  final def fromString(message: String): Error =
    Error(ErrorMessage.StringMessage(message))

  /** Throwable error with an empty path
    *
    * @param throwable
    *   custom throwable object
    *
    * @since 0.7.0
    */
  final def fromThrowable(throwable: Throwable): Error =
    Error(ErrorMessage.ThrowableMessage(throwable))
}
