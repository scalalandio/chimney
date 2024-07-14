package io.scalaland.chimney.internal.compiletime

import scala.reflect.macros.blackbox

trait DefinitionsPlatform
    extends Definitions
    with TypesPlatform
    with ExprsPlatform
    with ExprPromisesPlatform
    with ResultsPlatform {

  val c: blackbox.Context

  protected val XMacroSettings: List[String] = c.settings

  /** Useful for distinction between 2.12 and 2.13, when necessary. */
  protected val isScala212: Boolean = scala.util.Properties.versionNumberString < "2.13"
}
