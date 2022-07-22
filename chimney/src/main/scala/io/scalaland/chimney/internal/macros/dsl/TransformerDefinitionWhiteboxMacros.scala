package io.scalaland.chimney.internal.macros.dsl

import io.scalaland.chimney.internal.utils.DslMacroUtils

import scala.reflect.macros.whitebox

class TransformerDefinitionWhiteboxMacros(val c: whitebox.Context) extends DslMacroUtils {

  import CfgTpes._
  import c.universe._

  def withFieldConstImpl[
      T: WeakTypeTag,
      U: WeakTypeTag,
      C: WeakTypeTag
  ](selector: Tree, value: Tree): Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(weakTypeOf[U] <:< weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Value passed to `withFieldConst` is of type: ${weakTypeOf[U]}
           |Type required by '$fieldName' field: ${weakTypeOf[T]}
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree.overrideField(fieldName, value, fieldConstT, weakTypeOf[C])
    }
  }

  def withFieldConstFImpl[F[+_]](selector: Tree, value: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    q"${c.prefix}.lift[$F].withFieldConstF($selector, $value)"
  }

  def withFieldComputedImpl[
      T: WeakTypeTag,
      U: WeakTypeTag,
      C: WeakTypeTag
  ](selector: Tree, map: Tree): Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(weakTypeOf[U] <:< weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Function passed to `withFieldComputed` returns type: ${weakTypeOf[U]}
           |Type required by '$fieldName' field: ${weakTypeOf[T]}
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree.overrideField(fieldName, map, fieldComputedT, weakTypeOf[C])
    }
  }

  def withFieldComputedFImpl[F[+_]](selector: Tree, map: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    q"${c.prefix}.lift[$F].withFieldComputedF($selector, $map)"
  }

  def withFieldRenamedImpl[C: WeakTypeTag](selectorFrom: Tree, selectorTo: Tree): Tree = {

    val fieldNameFromOpt = selectorFrom.extractSelectorFieldNameOpt
    val fieldNameToOpt = selectorTo.extractSelectorFieldNameOpt

    (fieldNameFromOpt, fieldNameToOpt) match {
      case (Some(fieldNameFrom), Some(fieldNameTo)) =>
        c.prefix.tree.renameField(fieldNameFrom, fieldNameTo, weakTypeOf[C])
      case (Some(_), None) =>
        c.abort(c.enclosingPosition, s"Selector of type ${selectorTo.tpe} is not valid: $selectorTo")
      case (None, Some(_)) =>
        c.abort(c.enclosingPosition, s"Selector of type ${selectorFrom.tpe} is not valid: $selectorFrom")
      case (None, None) =>
        val inv1 = s"Selector of type ${selectorFrom.tpe} is not valid: $selectorFrom"
        val inv2 = s"Selector of type ${selectorTo.tpe} is not valid: $selectorTo"
        c.abort(c.enclosingPosition, s"Invalid selectors:\n$inv1\n$inv2")
    }
  }

  def withCoproductInstanceImpl[
      To: WeakTypeTag,
      Inst: WeakTypeTag,
      C: WeakTypeTag
  ](f: Tree): Tree = {
    c.prefix.tree.overrideCoproductInstance(weakTypeOf[Inst], weakTypeOf[To], f, coproductInstanceT, weakTypeOf[C])
  }

  def withCoproductInstanceFImpl[F[+_]](f: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    q"${c.prefix}.lift[$F].withCoproductInstanceF($f)"
  }

}
