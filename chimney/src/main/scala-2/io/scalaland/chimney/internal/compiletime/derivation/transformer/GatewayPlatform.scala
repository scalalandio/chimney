package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform
import io.scalaland.chimney.{internal, partial, PartialTransformer, Transformer}

import scala.annotation.unused

private[compiletime] trait GatewayPlatform extends Gateway {
  this: DefinitionsPlatform & DerivationPlatform =>

  import c.universe.{internal as _, Transformer as _, *}
  import typeUtils.fromWeakConversion.*

  // converts WeakTypeTags into our internal Types

  final def deriveTotalTransformerImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: internal.TransformerCfg: WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: WeakTypeTag,
      SharedFlags <: internal.TransformerFlags: WeakTypeTag
  ](@unused tc: c.Tree): Expr[Transformer[From, To]] =
    deriveTotalTransformerUnsafe[From, To, Cfg, InstanceFlags, SharedFlags]

  final def derivePartialTransformerImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: internal.TransformerCfg: WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: WeakTypeTag,
      SharedFlags <: internal.TransformerFlags: WeakTypeTag
  ](@unused tc: c.Tree): Expr[PartialTransformer[From, To]] =
    derivePartialTransformerUnsafe[From, To, Cfg, InstanceFlags, SharedFlags]

  override protected def instantiateTotalTransformer[From: Type, To: Type](
      toExpr: Expr[From] => Expr[To]
  ): Expr[Transformer[From, To]] = {
    val src: String = c.freshName("src") // TODO: copy-paste solution/utility from old macros
    val srcExpr: Expr[From] = c.Expr[From](q"${TermName(src)}")
    c.Expr[Transformer[From, To]](
      q"""new _root_.io.scalaland.chimney.Transformer[${Type[From]}, ${Type[To]}] {
        def transform($src: ${Type[From]}): ${Type[To]} = {
          ${toExpr(srcExpr)}
        }
      }"""
    )
  }

  override protected def instantiatePartialTransformer[From: Type, To: Type](
      toExpr: (Expr[From], Expr[Boolean]) => Expr[partial.Result[To]]
  ): Expr[PartialTransformer[From, To]] = {
    val src: String = c.freshName("src") // TODO: copy-paste solution/utility from old macros
    val srcExpr: Expr[From] = c.Expr[From](q"${TermName(src)}")
    val failFast: String = c.freshName("failFast") // TODO: copy-paste solution/utility from old macros
    val failFastExpr: Expr[Boolean] = c.Expr[Boolean](q"${TermName(failFast)}")
    c.Expr[PartialTransformer[From, To]](
      q"""new _root_.io.scalaland.chimney.PartialTransformer[${Type[From]}, ${Type[To]}] {
          def transform($src: ${Type[From]}, $failFast): _root_.io.scalaland.chimney.partial.Result[${Type[To]}] = {
            ${toExpr(srcExpr, failFastExpr)}
          }
        }"""
    )
  }
}
