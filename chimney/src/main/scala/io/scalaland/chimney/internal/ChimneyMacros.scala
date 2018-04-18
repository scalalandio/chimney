package io.scalaland.chimney.internal

import scala.reflect.macros.whitebox

class ChimneyMacros(val c: whitebox.Context)
    extends TransformerMacros
    with DslMacros
    with MacroUtils
    with DerivationConfig
    with Prefixes {

  import c.universe._

  def withFieldConstImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, T: c.WeakTypeTag, C: c.WeakTypeTag](
    selector: c.Tree,
    value: c.Tree
  ): c.Tree = {

    val fieldName = selector.extractSelectorFieldName
    expandWithFieldConst[From, To, C](fieldName, value)
  }

  def withFieldComputedImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, T: c.WeakTypeTag, C: c.WeakTypeTag](
    selector: c.Tree,
    map: c.Tree
  ): c.Tree = {
    val fieldName = selector.extractSelectorFieldName
    expandFieldComputed[From, To, C](fieldName, map)
  }

  def transformIntoImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[To] = {
    c.Expr[To](expandTansformInto[From, To])
  }

  def transformImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Expr[To] = {
    c.Expr[To](expandTansform[From, To, C])
  }

  def deriveTransformerImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]
    : c.Expr[io.scalaland.chimney.Transformer[From, To]] = {
    genTransformer[From, To](Config())
  }
}
