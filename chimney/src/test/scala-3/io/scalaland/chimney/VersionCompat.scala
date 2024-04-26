package io.scalaland.chimney

trait VersionCompat {

  // Workaround for https://github.com/scala/scala3/issues/18484, running the test only on Scala 2:

  transparent inline def compileErrorsScala2(inline code: String): String = ""

  def isScala3: Boolean = true
}
