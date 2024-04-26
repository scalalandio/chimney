package io.scalaland.chimney

import scala.language.experimental.macros
import munit.internal.MacroCompatScala2

trait VersionCompat {

  // Workaround for https://github.com/scala/scala3/issues/18484, running the test only on Scala 2:

  def compileErrorsScala2(code: String): String =
    macro MacroCompatScala2.compileErrorsImpl

  def isScala3: Boolean = false
}
