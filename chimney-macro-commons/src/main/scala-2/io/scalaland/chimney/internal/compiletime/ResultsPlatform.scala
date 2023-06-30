package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ResultsPlatform extends Results { this: DefinitionsPlatform =>

  protected def reportInfo(info: String): Unit = c.echo(c.enclosingPosition, info)

  protected def reportError(errors: String): Nothing = c.abort(c.enclosingPosition, errors)
}
