package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ResultsPlatform extends Results { this: DefinitionsPlatform =>

  // TODO: proper error generation
  protected def reportError(errors: String): Nothing = c.abort(c.enclosingPosition, errors)
}
