package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

class ChimneyBlackboxMacros(val c: blackbox.Context)
    extends TransformerMacros
    with DslBlackboxMacros
    with DerivationGuards
    with MacroUtils
    with DerivationConfig {

  def transformImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Expr[To] = {
    c.Expr[To](expandTansform[From, To, C])
  }

  def deriveTransformerImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]
    : c.Expr[io.scalaland.chimney.Transformer[From, To]] = {
    genTransformer[From, To](Config())
  }
}
