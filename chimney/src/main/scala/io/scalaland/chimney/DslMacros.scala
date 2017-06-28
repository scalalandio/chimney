package io.scalaland.chimney


import scala.language.experimental.macros



private[chimney] object DslMacros {
  def constFieldSelector(c: scala.reflect.macros.whitebox.Context)
                        (selector: c.Tree, value: c.Tree): c.Tree = {
    import c.universe._
    selector match {
      case q"($_) => $_.${fieldName: Name}" =>
        val sym = Symbol(fieldName.decodedName.toString)
        println(sym)
        q"{${c.prefix}}.withFieldConst($sym, $value)"
      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

  def computedFieldSelector(c: scala.reflect.macros.whitebox.Context)
                        (selector: c.Tree, value: c.Tree): c.Tree = {
    import c.universe._
    selector match {
      case q"($_) => $_.${fieldName: Name}" =>
        val sym = Symbol(fieldName.decodedName.toString)
        println(sym)
        q"{${c.prefix}}.withFieldComputed($sym, $value)"
      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

  def renamedFieldSelector(c: scala.reflect.macros.whitebox.Context)
                           (selector: c.Tree, value: c.Tree): c.Tree = {
    import c.universe._
    selector match {
      case q"($_) => $_.${fieldName: Name}" =>
        val sym = Symbol(fieldName.decodedName.toString)
        println(sym)
        q"{${c.prefix}}.withFieldRenamed($sym, $value)"
      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }


}