package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.{internal, partial, PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.quoted.{Expr, Quotes, Type}

final class NewTransformerMacros(q: Quotes)
    extends DefinitionsPlatform(using q)
    with DerivationPlatform
    with GatewayPlatform

object NewTransformerMacros {

  final def deriveTotalTransformationResult[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ](src: Expr[From])(using quotes: Quotes): Expr[To] =
    new NewTransformerMacros(quotes).deriveTotalTransformationResult[From, To, Cfg, InstanceFlags, SharedFlags](src)

  final def deriveTotalTransformer[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ](using quotes: Quotes): Expr[Transformer[From, To]] =
    new NewTransformerMacros(quotes).deriveTotalTransformer[From, To, Cfg, InstanceFlags, SharedFlags]

  final def derivePartialTransformationFullResult[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ](src: Expr[From])(using quotes: Quotes): Expr[partial.Result[To]] =
    new NewTransformerMacros(quotes).derivePartialTransformationResult[From, To, Cfg, InstanceFlags, SharedFlags](
      src,
      '{ false }
    )

  final def derivePartialTransformationFailFastResult[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ](src: Expr[From])(using quotes: Quotes): Expr[partial.Result[To]] =
    new NewTransformerMacros(quotes).derivePartialTransformationResult[From, To, Cfg, InstanceFlags, SharedFlags](
      src,
      '{ true }
    )

  final def derivePartialTransformer[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ](using quotes: Quotes): Expr[PartialTransformer[From, To]] =
    new NewTransformerMacros(quotes).derivePartialTransformer[From, To, Cfg, InstanceFlags, SharedFlags]
}