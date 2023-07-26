package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Results { this: Definitions =>

  /** Prints info at current macro expansion - assume it can only be called once */
  protected def reportInfo(info: String): Unit

  /** Prints error at current macro expansion AND throw exception for aborting macro expansion */
  protected def reportError(errors: String): Nothing

  /** Throws AssertionFailed exception */
  protected def assertionFailed(assertion: String): Nothing = throw new AssertionError(assertion)
}
