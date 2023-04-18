package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ResultsPlatform extends Results { this: DefinitionsPlatform =>
  
  protected def reportError(errors: String): Nothing = c.abort(c.enclosingPosition, errors)
}
