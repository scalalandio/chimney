package io.scalaland.chimney

trait VersionCompat {

  def isScala3 = true

  /* Copy/Paste from munit, with transparent keyword added.
   * Without the keyword some unexpected error reports would be collected
   */
  transparent inline def compileErrorsFixed(inline code: String): String = {
    val errors = scala.compiletime.testing.typeCheckErrors(code)
    errors
      .map { error =>
        val indent = " " * (error.column - 1)
        val trimMessage = error.message.linesIterator
          .map { line =>
            if line.matches(" +") then ""
            else line
          }
          .mkString("\n")
        val separator = if error.message.contains('\n') then "\n" else " "
        s"error:${separator}${trimMessage}\n${error.lineContent}\n${indent}^"
      }
      .mkString("\n")
  }

  transparent inline def compileErrorsScala2(inline code: String): String = ""
}
