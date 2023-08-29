package io.scalaland.chimney

import scala.language.experimental.macros
import munit.internal.MacroCompatScala2

trait VersionCompat {

  /* Directly used compileErrors from munit.
   * For reasoning, see the Scala 3 version of the file.
   */
  def compileErrorsFixed(code: String): String =
    macro MacroCompatScala2.compileErrorsImpl

  def compileErrorsScala2(code: String): String =
    macro MacroCompatScala2.compileErrorsImpl
}
