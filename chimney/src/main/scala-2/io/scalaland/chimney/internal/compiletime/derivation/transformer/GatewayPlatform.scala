package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform
import io.scalaland.chimney.{internal, PartialTransformer, Transformer}

import scala.annotation.unused

private[compiletime] trait GatewayPlatform extends Gateway {
  this: DefinitionsPlatform & DerivationPlatform =>

  import c.universe.{internal as _, Transformer as _, *}
  import typeUtils.fromWeakConversion.*

  def deriveTotalTransformerImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: internal.TransformerCfg: WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: WeakTypeTag,
      SharedFlags <: internal.TransformerFlags: WeakTypeTag
  ](@unused tc: c.Tree): Expr[Transformer[From, To]] =
    deriveTotalTransformerUnsafe[From, To, Cfg, InstanceFlags, SharedFlags]

  def derivePartialTransformerImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: internal.TransformerCfg: WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: WeakTypeTag,
      SharedFlags <: internal.TransformerFlags: WeakTypeTag
  ](@unused tc: c.Tree): Expr[PartialTransformer[From, To]] =
    derivePartialTransformerUnsafe[From, To, Cfg, InstanceFlags, SharedFlags]
}
