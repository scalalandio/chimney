package io.scalaland.chimney.internal


import scala.reflect.macros.whitebox

class ChimneyMacros(val c: whitebox.Context)
    extends TransformerMacros
    with DslMacros
    with MacroUtils
    with DerivationConfig
    with Prefixes {

  import c.universe._

  def withFieldConstImpl2[From: c.WeakTypeTag, To: c.WeakTypeTag, T: c.WeakTypeTag, Overrides: c.WeakTypeTag](
    selector: c.Tree,
    value: c.Tree
  ): c.Tree = {

    selector match {
      case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: Name}" if vd.name == idt.name =>

        val From = weakTypeOf[From]
        val To = weakTypeOf[To]
        val Overrides = weakTypeOf[Overrides]

        val fieldNameConst = Constant(fieldName.decodedName.toString)
        val fieldNameLit = Literal(fieldNameConst)
        val singletonFieldTpe = c.internal.constantType(fieldNameConst)
        val fieldConstTpe = tq"_root_.io.scalaland.chimney.internal.FieldConst[$singletonFieldTpe]"
        val overridesTpe = tq"_root_.io.scalaland.chimney.internal.Cns[$fieldConstTpe, $Overrides]"
        val fn = TermName(c.freshName("ti"))

        q"""
           {
             val $fn = ${c.prefix.tree}
             new TransformerInto[$From, $To, $overridesTpe]($fn.source, $fn.overrides.updated($fieldNameLit, $value))
           }
         """.debug
      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

  def withFieldComputedImpl2[From: c.WeakTypeTag, To: c.WeakTypeTag, T: c.WeakTypeTag, Overrides: c.WeakTypeTag](
    selector: c.Tree,
    map: c.Tree
  ): c.Tree = {
    selector match {
      case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: Name}" if vd.name == idt.name =>

        val From = weakTypeOf[From]
        val To = weakTypeOf[To]
        val Overrides = weakTypeOf[Overrides]

        val fieldNameConst = Constant(fieldName.decodedName.toString)
        val fieldNameLit = Literal(fieldNameConst)
        val singletonFieldTpe = c.internal.constantType(fieldNameConst)
        val fieldComputedTpe = tq"_root_.io.scalaland.chimney.internal.FieldComputed[$singletonFieldTpe]"
        val overridesTpe = tq"_root_.io.scalaland.chimney.internal.Cns[$fieldComputedTpe, $Overrides]"
        val fn = TermName(c.freshName("ti"))

        q"""
           {
             val $fn = ${c.prefix.tree}
             new TransformerInto[$From, $To, $overridesTpe]($fn.source, $fn.overrides.updated($fieldNameLit, $map))
           }
         """.debug

      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

  def transformImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, Overrides: c.WeakTypeTag]: c.Expr[To] = {
    c.Expr[To](expandTansform[From, To, Overrides])
  }

  def genImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[io.scalaland.chimney.Transformer[From, To]] = {
    val config = Config(disableDefaultValues = false, overridenFields = Set.empty)
    genTransformer[From, To](config)
  }
}
