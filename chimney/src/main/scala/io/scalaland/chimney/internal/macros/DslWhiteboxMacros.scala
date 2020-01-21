package io.scalaland.chimney.internal.macros

import scala.reflect.macros.whitebox

trait DslWhiteboxMacros {

  val c: whitebox.Context

  import c.universe._

  def expandWithFieldConst[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag](
      fieldName: Name,
      value: c.Tree
  ): c.Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]

    val fieldNameConst = Constant(fieldName.decodedName.toString)
    val fieldNameLit = Literal(fieldNameConst)
    val singletonFieldTpe = c.internal.constantType(fieldNameConst)
    val newCfgTpe = tq"_root_.io.scalaland.chimney.internal.TransformerCfg.FieldConst[$singletonFieldTpe, $C]"
    val fn = TermName(c.freshName("ti"))

    q"""
      {
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.internal.dsl.TransformerDefinition[$From, $To, $newCfgTpe]($fn.overrides.updated($fieldNameLit, $value), $fn.instances)
      }
    """
  }

  def expandFieldComputed[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag](
      fieldName: Name,
      map: c.Tree
  ): c.Tree = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]

    val fieldNameConst = Constant(fieldName.decodedName.toString)
    val fieldNameLit = Literal(fieldNameConst)
    val singletonFieldTpe = c.internal.constantType(fieldNameConst)
    val newCfgTpe = tq"_root_.io.scalaland.chimney.internal.TransformerCfg.FieldComputed[$singletonFieldTpe, $C]"
    val fn = TermName(c.freshName("td"))

    q"""
      {
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.internal.dsl.TransformerDefinition[$From, $To, $newCfgTpe]($fn.overrides.updated($fieldNameLit, $map), $fn.instances)
      }
    """
  }

  def expandFieldRenamed[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag](
      fieldNameFrom: Name,
      fieldNameTo: Name
  ): c.Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]

    val singletonFromTpe = c.internal.constantType(Constant(fieldNameFrom.decodedName.toString))
    val singletonToTpe = c.internal.constantType(Constant(fieldNameTo.decodedName.toString))

    val newCfgTpe =
      tq"_root_.io.scalaland.chimney.internal.TransformerCfg.FieldRelabelled[$singletonFromTpe, $singletonToTpe, $C]"

    q"${c.prefix.tree}.asInstanceOf[_root_.io.scalaland.chimney.internal.dsl.TransformerDefinition[$From, $To, $newCfgTpe]]"
  }

  def expandCoproductInstance[From: c.WeakTypeTag, To: c.WeakTypeTag, Inst: c.WeakTypeTag, C: c.WeakTypeTag](
      f: c.Tree
  ): c.Tree = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val Inst = weakTypeOf[Inst]
    val C = weakTypeOf[C]

    val newCfgTpe = tq"_root_.io.scalaland.chimney.internal.TransformerCfg.CoproductInstance[$Inst, $To, $C]"
    val fn = TermName(c.freshName("td"))

    val fullInstName = Inst.typeSymbol.fullName.toString
    val fullTargetName = To.typeSymbol.fullName.toString

    q"""
      {
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.internal.dsl.TransformerDefinition[$From, $To, $newCfgTpe]($fn.overrides, $fn.instances.updated(($fullInstName, $fullTargetName), $f))
      }
    """
  }

}
