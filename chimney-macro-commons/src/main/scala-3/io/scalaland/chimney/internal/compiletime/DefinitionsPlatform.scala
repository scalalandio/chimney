package io.scalaland.chimney.internal.compiletime

import scala.quoted

abstract class DefinitionsPlatform(using val quotes: quoted.Quotes)
    extends Definitions
    with TypesPlatform
    with ExprsPlatform
    with ExprPromisesPlatform
    with ResultsPlatform {

  protected val XMacroSettings: List[String] = {
    // workaround to contain @experimental from polluting the whole codebase
    val info = quotes.reflect.CompilationInfo
    info.getClass.getMethod("XmacroSettings").invoke(info).asInstanceOf[List[String]]
  }
}
