package io.scalaland.chimney.internal

import io.scalaland.chimney.dsl2.TransformerInto

import scala.reflect.macros.blackbox

class ChimneyMacros(val c: blackbox.Context)
    extends TransformerMacros
    with Dsl2Macros
    with MacroUtils
    with DerivationConfig {

  import c.universe._

  def disableDefaultValuesImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[TransformerInto[From, To]] = {
    c.Expr[TransformerInto[From, To]](expandDisableDefaultValues.debug)
  }

  def withFieldConstImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, T: c.WeakTypeTag](
    selector: c.Expr[To => T],
    value: c.Expr[T]
  ): c.Expr[TransformerInto[From, To]] = {
    selector.tree match {
      case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: Name}" if vd.name == idt.name =>
        c.Expr[TransformerInto[From, To]](expandWithFieldConst(fieldName, value.tree))
      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

  def withFieldComputedImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, T: c.WeakTypeTag](
    selector: c.Expr[To => T],
    map: c.Expr[From => T]
  ): c.Expr[TransformerInto[From, To]] = {
    selector.tree match {
      case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: Name}" if vd.name == idt.name =>
        c.Expr[TransformerInto[From, To]](expandWithFieldComputed(fieldName, map.tree))
      case _ =>
        c.abort(c.enclosingPosition, "Invalid selector!")
    }
  }

  def transformImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[To] = {
    c.Expr[To](expandTansform[From, To])
  }

  def genImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[io.scalaland.chimney.Transformer[From, To]] = {
    val config = Config(disableDefaultValues = false, consts = Map.empty, funs = Map.empty)
    genTransformer[From, To](config)
  }
}
