package io.scalaland.chimney.validated.internal

import io.scalaland.chimney.internal._
import scala.reflect.macros.blackbox

class ChimneyVBlackboxMacros(val c: blackbox.Context)
    extends PatcherMacros
    with TransformerMacros
    with VTransformerMacros
    with DslVBlackboxMacros
    with DslBlackboxMacros
    with DerivationGuards
    with MacroUtils
    with DerivationVConfig
    with DerivationConfig
    with EitherUtils {

  def deriveVTransformerImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]
    : c.Expr[io.scalaland.chimney.validated.VTransformer[From, To]] = {
    genVTransformer[From, To](VConfig())
  }

  def transformVImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag, VC: c.WeakTypeTag]: c.Expr[To] = {
    c.Expr[To](expandVTransform[From, To, C, VC])
  }
}
