package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.{PartialTransformerDefinition, TransformerDefinition}
import io.scalaland.chimney.internal.TransformerCfg.Empty
import io.scalaland.chimney.internal.TransformerFlags.Default
import io.scalaland.chimney.{internal, partial, PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.quoted.{Expr, Quotes, Type}

final class TransformerMacros(q: Quotes)
    extends DefinitionsPlatform(using q)
    with DerivationPlatform
    with GatewayPlatform {

  type ImplicitScopeFlagsType <: internal.TransformerFlags

  final def deriveTotalTransformerWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[Transformer[From, To]] =
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicit ImplicitScopeFlagsType =>
      deriveTotalTransformer[From, To, Empty, Default, ImplicitScopeFlagsType](
        runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty
      )
    }

  final def deriveTotalTransformerWithConfig[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      Flags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]]
  )(using quotes: Quotes): Expr[Transformer[From, To]] =
    deriveTotalTransformer[From, To, Cfg, Flags, ImplicitScopeFlags](runtimeDataStore = '{ ${ td }.runtimeData })

  final def derivePartialTransformerWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[PartialTransformer[From, To]] =
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicit ImplicitScopeFlagsType =>
      derivePartialTransformer[From, To, Empty, Default, ImplicitScopeFlagsType](
        runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty
      )
    }

  final def derivePartialTransformerWithConfig[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      Flags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]]
  )(using quotes: Quotes): Expr[PartialTransformer[From, To]] =
    derivePartialTransformer[From, To, Cfg, Flags, ImplicitScopeFlags](runtimeDataStore = '{ ${ td }.runtimeData })

  private def findImplicitScopeTransformerConfiguration(using
      quotes: Quotes
  ): Expr[io.scalaland.chimney.dsl.TransformerConfiguration[? <: io.scalaland.chimney.internal.TransformerFlags]] =
    scala.quoted.Expr
      .summon[io.scalaland.chimney.dsl.TransformerConfiguration[? <: io.scalaland.chimney.internal.TransformerFlags]]
      .getOrElse {
        // $COVERAGE-OFF$
        reportError("Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: Type[ImplicitScopeFlagsType] => Expr[A]
  ): Expr[A] = {
    import quotes.*
    import quotes.reflect.*

    val implicitScopeConfig = findImplicitScopeTransformerConfiguration
    val implicitScopeConfigType = implicitScopeConfig.asTerm.tpe.widen.typeArgs.head.asType
      .asInstanceOf[Type[ImplicitScopeFlagsType]]

    '{
      val _ = $implicitScopeConfig
      ${ useImplicitScopeFlags(implicitScopeConfigType) }
    }
  }
}

object TransformerMacros {

  final def deriveTotalTransformerWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[Transformer[From, To]] =
    new TransformerMacros(quotes).deriveTotalTransformerWithDefaults[From, To]

  final def deriveTotalTransformerWithConfig[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      Flags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]]
  )(using quotes: Quotes): Expr[Transformer[From, To]] =
    new TransformerMacros(quotes).deriveTotalTransformerWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](td)

  final def deriveTotalTransformerResultWithConfig[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      Flags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](source: Expr[From], td: Expr[TransformerDefinition[From, To, Cfg, Flags]])(using quotes: Quotes): Expr[To] =
    new TransformerMacros(quotes).deriveTotalTransformationResult[From, To, Cfg, Flags, ImplicitScopeFlags](
      source,
      '{ ${ td }.runtimeData }
    )

  final def derivePartialTransformerWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[PartialTransformer[From, To]] =
    new TransformerMacros(quotes).derivePartialTransformerWithDefaults[From, To]

  final def derivePartialTransformerWithConfig[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      Flags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]]
  )(using quotes: Quotes): Expr[PartialTransformer[From, To]] =
    new TransformerMacros(quotes).derivePartialTransformerWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](td)

  final def derivePartialTransformerResultWithConfig[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      Flags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](source: Expr[From], td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]], failFast: Boolean)(using
      quotes: Quotes
  ): Expr[partial.Result[To]] =
    new TransformerMacros(quotes).derivePartialTransformationResult[From, To, Cfg, Flags, ImplicitScopeFlags](
      source,
      Expr(failFast),
      '{ ${ td }.runtimeData }
    )

}
