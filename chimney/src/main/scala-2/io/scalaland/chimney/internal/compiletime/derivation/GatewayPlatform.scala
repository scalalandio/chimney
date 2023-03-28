package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.{internal, PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.annotation.unused

private[compiletime] trait GatewayPlatform extends Gateway {
  this: DefinitionsPlatform & DerivationPlatform & LegacyPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  implicit private def typeFromWeak[T: WeakTypeTag]: Type[T] = typeImpl.fromWeak

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
