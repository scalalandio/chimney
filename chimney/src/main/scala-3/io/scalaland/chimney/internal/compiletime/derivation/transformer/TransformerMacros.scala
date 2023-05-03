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
    resolveImplicitScopeFlagsAndMuteUnusedConfigWarnings { implicit ImplicitScopeFlagsType =>
      deriveTotalTransformer[From, To, Empty, Default, ImplicitScopeFlagsType](runtimeDataStore = None)
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
    deriveTotalTransformer[From, To, Cfg, Flags, ImplicitScopeFlags](runtimeDataStore = Some('{ ${ td }.runtimeData }))

  final def derivePartialTransformerWithDefaults[
      From: Type,
      To: Type
  ](using quotes: Quotes): Expr[PartialTransformer[From, To]] =
    resolveImplicitScopeFlagsAndMuteUnusedConfigWarnings { implicit ImplicitScopeFlagsType =>
      derivePartialTransformer[From, To, Empty, Default, ImplicitScopeFlagsType](runtimeDataStore = None)
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
    derivePartialTransformer[From, To, Cfg, Flags, ImplicitScopeFlags](runtimeDataStore = Some('{
      ${ td }.runtimeData
    }))

  private def findImplicitScopeFlags(using
      quotes: Quotes
  ): Expr[io.scalaland.chimney.dsl.TransformerConfiguration[? <: io.scalaland.chimney.internal.TransformerFlags]] =
    scala.quoted.Expr
      .summon[io.scalaland.chimney.dsl.TransformerConfiguration[? <: io.scalaland.chimney.internal.TransformerFlags]]
      .getOrElse {
        // $COVERAGE-OFF$
        reportError("Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }

  private def resolveImplicitScopeFlagsAndMuteUnusedConfigWarnings[A: Type](
      useLocalConfig: Type[ImplicitScopeFlagsType] => Expr[A]
  ): Expr[A] = {
    import quotes.*
    import quotes.reflect.*

    val localConfig = findImplicitScopeFlags
    val localConfigType = findImplicitScopeFlags.asTerm.tpe.widen.typeArgs.head.asType
      .asInstanceOf[Type[ImplicitScopeFlagsType]]

    '{
      val _ = $localConfig
      ${ useLocalConfig(localConfigType) }
    }
  }

  implicit private val EmptyConfigType: Type[Empty] = ChimneyType.TransformerCfg.Empty
  implicit private val DefaultFlagsType: Type[Default] = ChimneyType.TransformerFlags.Default
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
      Some('{ ${ td }.runtimeData })
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
      Some('{ ${ td }.runtimeData })
    )

}
