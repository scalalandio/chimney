package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ResultsPlatform extends Results { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  protected def reportInfo(info: String): Unit = report.info(info, Position.ofMacroExpansion)

  protected def reportError(errors: String): Nothing = report.errorAndAbort(errors, Position.ofMacroExpansion)
}
