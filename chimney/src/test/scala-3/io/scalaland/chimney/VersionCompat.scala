package io.scalaland.chimney

trait VersionCompat {

  // TODO: I wish to remove these one day :/

  transparent inline def compileErrorsScala2(inline code: String): String = ""

  def isScala3: Boolean = true
}
