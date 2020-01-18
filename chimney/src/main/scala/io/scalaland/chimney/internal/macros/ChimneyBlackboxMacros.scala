package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.DerivationConfig
import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}
import io.scalaland.chimney.{Patcher, Transformer}

import scala.reflect.macros.blackbox

class ChimneyBlackboxMacros(val c: blackbox.Context)
    extends PatcherMacros
    with TransformerMacros
    with DslBlackboxMacros
    with DerivationGuards
    with MacroUtils
    with DerivationConfig
    with EitherUtils {

  def buildTransformerImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Expr[Transformer[From, To]] = {
    c.Expr[Transformer[From, To]](buildDefinedTransformer[From, To, C])
  }

  def transformImpl[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Expr[To] = {
    c.Expr[To](expandTransform[From, To, C])
  }

  def deriveTransformerImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[Transformer[From, To]] = {
    import c.universe._
    genTransformer[From, To](TransformerConfig(definitionScope = Some((weakTypeOf[From], weakTypeOf[To]))))
  }

  def derivePatcherImpl[T: c.WeakTypeTag, Patch: c.WeakTypeTag]: c.Expr[Patcher[T, Patch]] = {
    genPatcher[T, Patch]()
  }
}
