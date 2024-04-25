package io.scalaland.chimney

import scala.language.experimental.macros
import munit.internal.MacroCompatScala2

trait VersionCompat {

  // TODO: I wish to remove these one day

  def compileErrorsScala2(code: String): String =
    macro MacroCompatScala2.compileErrorsImpl

  def isScala3: Boolean = false
}
