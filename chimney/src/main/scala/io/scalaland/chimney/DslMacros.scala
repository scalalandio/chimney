package io.scalaland.chimney

import scala.language.experimental.macros

private[chimney] object DslMacros {
  def constFieldSelector(c: scala.reflect.macros.whitebox.Context)(selector: c.Tree, value: c.Tree): c.Tree = {
    import c.universe._
    selector match {

      case q"(${_: ValDef}) => ${_: Ident}.${fieldName: Name}" =>
        val sym = Symbol(fieldName.decodedName.toString)
        q"{${c.prefix}}.withFieldConst($sym, $value)"
      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

  def computedFieldSelector(c: scala.reflect.macros.whitebox.Context)(selector: c.Tree, map: c.Tree): c.Tree = {
    import c.universe._
    selector match {
      case q"(${_: ValDef}) => ${_: Ident}.${fieldName: Name}" =>
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
      case (q"(${_: ValDef}) => ${_: Ident}.${fromFieldName: Name}", q"(${_: ValDef}) => ${_: Ident}.${toFieldName: Name}") =>
        val symFrom = Symbol(fromFieldName.decodedName.toString)
        val symTo = Symbol(toFieldName.decodedName.toString)
        q"{${c.prefix}}.withFieldRenamed($symFrom, $symTo)"
      case (q"(${_: ValDef}) => ${_: Ident}.${_: Name}", sel@_) =>
        c.abort(c.enclosingPosition, s"Selector of type ${sel.tpe} is not valid: $sel")
      case (sel@_, q"(${_: ValDef}) => ${_: Ident}.${_: Name}") =>
        c.abort(c.enclosingPosition, s"Selector of type ${sel.tpe} is not valid: $sel")
      case (sel1, sel2) =>
        val inv1 = s"Selector of type ${sel1.tpe} is not valid: $sel1"
        val inv2 = s"Selector of type ${sel2.tpe} is not valid: $sel2"
        c.abort(c.enclosingPosition, s"Invalid selectors:\n$inv1\n$inv2")
    }
  }

}
