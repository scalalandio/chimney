package io.scalaland.chimney

import scala.language.experimental.macros
import munit.internal.MacroCompatScala2

trait VersionCompat {

  def isScala3 = false

  /* Directly used compileErrors from munit.
   * For reasoning, see the Scala 3 version of the file.
   */
  def compileErrorsFixed(code: String): String =
    macro MacroCompatScala2.compileErrorsImpl

  /* Compilation errors that should be checked in Scala 2-only as currently Scala 3 has some issues
   * (which we don't want to comment out but also which we don't want to fail compilation and prevent us for other checks)
   */
  def compileErrorsScala2(code: String): String =
    macro MacroCompatScala2.compileErrorsImpl
}
