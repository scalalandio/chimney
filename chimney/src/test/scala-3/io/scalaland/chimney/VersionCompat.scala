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

  /* Compilation errors that should be checked in Scala 2-only as currently Scala 3 has some issues
   * (which we don't want to comment out but also which we don't want to fail compilation and prevent us for other checks)
   */
  transparent inline def compileErrorsScala2(inline code: String): String = ""
}
