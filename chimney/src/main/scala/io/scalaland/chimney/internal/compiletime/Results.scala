package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Results { this: Definitions =>

  protected def reportInfo(info: String): Unit

  protected def reportError(errors: String): Nothing
}
