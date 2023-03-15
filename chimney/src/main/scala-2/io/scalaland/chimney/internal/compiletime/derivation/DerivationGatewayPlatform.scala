package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.{internal, PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.annotation.unused

private[compiletime] trait DerivationGatewayPlatform extends DerivationGateway {
  this: DefinitionsPlatform & DerivationDefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  def deriveTotalTransformerImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: internal.TransformerCfg: WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: WeakTypeTag,
      SharedFlags <: internal.TransformerFlags: WeakTypeTag
  ](@unused tc: c.Tree): Expr[Transformer[From, To]] = {
    implicit val From: Type[From] = typeImpl.fromWeak[From]
    implicit val To: Type[To] = typeImpl.fromWeak[To]
    implicit val Cfg: Type[Cfg] = typeImpl.fromWeak[Cfg]
    implicit val InstanceFlags: Type[InstanceFlags] = typeImpl.fromWeak[InstanceFlags]
    implicit val SharedFlags: Type[SharedFlags] = typeImpl.fromWeak[SharedFlags]
    deriveTotalTransformerUnsafe[From, To, Cfg, InstanceFlags, SharedFlags]
  }

  def derivePartialTransformerImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Cfg <: internal.TransformerCfg: WeakTypeTag,
      InstanceFlags <: internal.TransformerFlags: WeakTypeTag,
      SharedFlags <: internal.TransformerFlags: WeakTypeTag
  ](@unused tc: c.Tree): Expr[PartialTransformer[From, To]] = {
    implicit val From: Type[From] = typeImpl.fromWeak[From]
    implicit val To: Type[To] = typeImpl.fromWeak[To]
    implicit val Cfg: Type[Cfg] = typeImpl.fromWeak[Cfg]
    implicit val InstanceFlags: Type[InstanceFlags] = typeImpl.fromWeak[InstanceFlags]
    implicit val SharedFlags: Type[SharedFlags] = typeImpl.fromWeak[SharedFlags]
    derivePartialTransformerUnsafe[From, To, Cfg, InstanceFlags, SharedFlags]
  }
}
