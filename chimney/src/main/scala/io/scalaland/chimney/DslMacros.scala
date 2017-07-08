package io.scalaland.chimney

import scala.language.experimental.macros

private[chimney] object DslMacros {
  def constFieldSelector(c: scala.reflect.macros.whitebox.Context)(selector: c.Tree, value: c.Tree): c.Tree = {
    import c.universe._
    selector match {
      case q"($_) => $_.${fieldName: Name}" =>
        val sym = Symbol(fieldName.decodedName.toString)
        q"{${c.prefix}}.withFieldConst($sym, $value)"
      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

  def computedFieldSelector(c: scala.reflect.macros.whitebox.Context)(selector: c.Tree, map: c.Tree): c.Tree = {
    import c.universe._
    selector match {
      case q"($_) => $_.${fieldName: Name}" =>
        val sym = Symbol(fieldName.decodedName.toString)
        q"{${c.prefix}}.withFieldComputed($sym, $map)"
      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

  def renamedFieldSelector(c: scala.reflect.macros.whitebox.Context)(selectorFrom: c.Tree,
                                                                     selectorTo: c.Tree): c.Tree = {
    import c.universe._
    (selectorFrom, selectorTo) match {
      case (q"($_) => $_.${fromFieldName: Name}", q"($_) => $_.${toFieldName: Name}") =>
        val symFrom = Symbol(fromFieldName.decodedName.toString)
        val symTo = Symbol(toFieldName.decodedName.toString)
        q"{${c.prefix}}.withFieldRenamed($symFrom, $symTo)"
      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

}
