package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal
import io.scalaland.chimney.internal.compiletime.Definitions

private[compiletime] trait Gateway { this: Definitions & Derivation & Legacy =>

  /** Intended for: being called from *Unsafe method to return final Expr; recursive derivation */
  final protected def deriveTotalTransformer[From: Type, To: Type](
      config: TransformerConfig[From, To]
  ): DerivationResult[Expr[Transformer[From, To]]] =
    instantiateTotalTransformer[From, To] { (src: Expr[From]) =>
      deriveTransformerBody(Context.ForTotal.create[From, To](src = src, config))
    }

  /** Intended for: being called from platform-specific code which returns Expr directly to splicing site */
  final protected def deriveTotalTransformerUnsafe[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ]: Expr[Transformer[From, To]] =
    deriveTotalTransformer[From, To](
      configurationsImpl.readTransformerConfig[From, To, Cfg, InstanceFlags, SharedFlags]
    ).unsafeGet

  /** Intended for: being called from *Unsafe method to return final Expr; recursive derivation */
  final protected def derivePartialTransformer[From: Type, To: Type](
      config: TransformerConfig[From, To]
  ): DerivationResult[Expr[PartialTransformer[From, To]]] =
    instantiatePartialTransformer[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
      deriveTransformerBody(Context.ForPartial.create[From, To](src = src, failFast = failFast, config))
    }

  /** Intended for: being called from platform-specific code which returns Expr directly to splicing site */
  final protected def derivePartialTransformerUnsafe[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ]: Expr[PartialTransformer[From, To]] =
    derivePartialTransformer[From, To](
      configurationsImpl.readTransformerConfig[From, To, Cfg, InstanceFlags, SharedFlags]
    ).unsafeGet

  // TODO
  private def deriveTransformerBody[From, To](implicit
      ctx: Context.ForTransformer[From, To]
  ): DerivationResult[Expr[ctx.Target]] =
    DerivationResult.notYetImplemented("Actual derivation")
}
