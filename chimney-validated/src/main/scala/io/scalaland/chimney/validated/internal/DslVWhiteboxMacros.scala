package io.scalaland.chimney.validated.internal

import scala.reflect.macros.whitebox

trait DslVWhiteboxMacros {

  val c: whitebox.Context

  import c.universe._

  def expandWithFieldConst[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag, VC: c.WeakTypeTag](
    fieldName: Name,
    value: c.Tree
  ): c.Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]
    val VC = weakTypeOf[VC]

    val fieldNameConst = Constant(fieldName.decodedName.toString)
    val fieldNameLit = Literal(fieldNameConst)
    val singletonFieldTpe = c.internal.constantType(fieldNameConst)
    val newCfgTpe = tq"_root_.io.scalaland.chimney.internal.FieldConst[$singletonFieldTpe, $C]"
    val fn = TermName(c.freshName("ti"))

    q"""
      {
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.validated.dsl.TransformerVInto[$From, $To, $newCfgTpe, $VC]($fn.source, $fn.overrides.updated($fieldNameLit, $value), $fn.instances, $fn.overridesV)
      }
    """
  }

  def expandFieldComputed[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag, VC: c.WeakTypeTag](
    fieldName: Name,
    map: c.Tree
  ): c.Tree = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]
    val VC = weakTypeOf[VC]

    val fieldNameConst = Constant(fieldName.decodedName.toString)
    val fieldNameLit = Literal(fieldNameConst)
    val singletonFieldTpe = c.internal.constantType(fieldNameConst)
    val newCfgTpe = tq"_root_.io.scalaland.chimney.internal.FieldComputed[$singletonFieldTpe, $C]"
    val fn = TermName(c.freshName("ti"))

    q"""
      {
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.validated.dsl.TransformerVInto[$From, $To, $newCfgTpe, $VC]($fn.source, $fn.overrides.updated($fieldNameLit, $map($fn.source)), $fn.instances, $fn.overridesV)
      }
    """
  }

  def expandFieldRenamed[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag, VC: c.WeakTypeTag](
    fieldNameFrom: Name,
    fieldNameTo: Name
  ): c.Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]
    val VC = weakTypeOf[VC]

    val singletonFromTpe = c.internal.constantType(Constant(fieldNameFrom.decodedName.toString))
    val singletonToTpe = c.internal.constantType(Constant(fieldNameTo.decodedName.toString))

    val newCfgTpe = tq"_root_.io.scalaland.chimney.internal.FieldRelabelled[$singletonFromTpe, $singletonToTpe, $C]"

    q"${c.prefix.tree}.asInstanceOf[_root_.io.scalaland.chimney.validated.dsl.TransformerVInto[$From, $To, $newCfgTpe, $VC]]"
  }

  def expandWithFieldConstV[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag, VC: c.WeakTypeTag](
    fieldName: Name,
    value: c.Tree
  ): c.Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]
    val VC = weakTypeOf[VC]

    val fieldNameConst = Constant(fieldName.decodedName.toString)
    val fieldNameLit = Literal(fieldNameConst)
    val singletonFieldTpe = c.internal.constantType(fieldNameConst)
    val newCfgTpe = tq"_root_.io.scalaland.chimney.validated.internal.FieldConstV[$singletonFieldTpe, $VC]"
    val fn = TermName(c.freshName("ti"))

    q"""
      {
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.validated.dsl.TransformerVInto[$From, $To, $C, $newCfgTpe]($fn.source, $fn.overrides, $fn.instances, $fn.overridesV.updated($fieldNameLit, $value))
      }
    """
  }

  def expandFieldComputedV[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag, VC: c.WeakTypeTag](
    fieldName: Name,
    map: c.Tree
  ): c.Tree = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]
    val C = weakTypeOf[C]
    val VC = weakTypeOf[VC]

    val fieldNameConst = Constant(fieldName.decodedName.toString)
    val fieldNameLit = Literal(fieldNameConst)
    val singletonFieldTpe = c.internal.constantType(fieldNameConst)
    val newCfgTpe = tq"_root_.io.scalaland.chimney.validated.internal.FieldComputedV[$singletonFieldTpe, $VC]"
    val fn = TermName(c.freshName("ti"))

    q"""
      {
        val $fn = ${c.prefix.tree}
        new _root_.io.scalaland.chimney.validated.dsl.TransformerVInto[$From, $To, $C, $newCfgTpe]($fn.source, $fn.overrides, $fn.instances, $fn.overridesV.updated($fieldNameLit, $map($fn.source)))
      }
    """
  }
}
