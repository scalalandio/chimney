package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.{PartialTransformerDefinition, TransformerDefinition}
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.quoted.{Expr, Quotes, Type}

final class TransformerMacros(q: Quotes) extends DerivationPlatform(q) with Gateway {

  import quotes.*, quotes.reflect.*

  def deriveTotalTransformerWithConfig[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]]
  ): Expr[Transformer[From, To]] =
    deriveTotalTransformer[From, To, Cfg, Flags, ImplicitScopeFlags](runtimeDataStore = '{ ${ td }.runtimeData })

  def deriveTotalTransformerWithDefaults[
      From: Type,
      To: Type
  ]: Expr[Transformer[From, To]] =
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
      deriveTotalTransformer[
        From,
        To,
        runtime.TransformerOverrides.Empty,
        runtime.TransformerFlags.Default,
        ImplicitScopeFlags
      ](runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty)
    }

  def derivePartialTransformerWithDefaults[
      From: Type,
      To: Type
  ]: Expr[PartialTransformer[From, To]] =
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
      derivePartialTransformer[
        From,
        To,
        runtime.TransformerOverrides.Empty,
        runtime.TransformerFlags.Default,
        ImplicitScopeFlags
      ](runtimeDataStore = ChimneyExpr.RuntimeDataStore.empty)
    }

  def derivePartialTransformerWithConfig[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]]
  ): Expr[PartialTransformer[From, To]] =
    derivePartialTransformer[From, To, Cfg, Flags, ImplicitScopeFlags](runtimeDataStore = '{ ${ td }.runtimeData })

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ?<[runtime.TransformerFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = scala.quoted.Expr
      .summon[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]]
      .getOrElse {
        // $COVERAGE-OFF$
        reportError("Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }
    val implicitScopeFlagsType = implicitScopeConfig.asTerm.tpe.widen.typeArgs.head.asType
      .asInstanceOf[Type[runtime.TransformerFlags]]
      .as_?<[runtime.TransformerFlags]

    Expr.block(
      List(Expr.suppressUnused(implicitScopeConfig)),
      useImplicitScopeFlags(implicitScopeFlagsType)
    )
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
      Cfg <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[TransformerDefinition[From, To, Cfg, Flags]]
  )(using quotes: Quotes): Expr[Transformer[From, To]] =
    new TransformerMacros(quotes).deriveTotalTransformerWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](td)

  final def deriveTotalTransformerResultWithConfig[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
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
      Cfg <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]]
  )(using quotes: Quotes): Expr[PartialTransformer[From, To]] =
    new TransformerMacros(quotes).derivePartialTransformerWithConfig[From, To, Cfg, Flags, ImplicitScopeFlags](td)

  final def derivePartialTransformerResultWithConfig[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](source: Expr[From], td: Expr[PartialTransformerDefinition[From, To, Cfg, Flags]], failFast: Boolean)(using
      quotes: Quotes
  ): Expr[partial.Result[To]] =
    new TransformerMacros(quotes).derivePartialTransformationResult[From, To, Cfg, Flags, ImplicitScopeFlags](
      source,
      Expr(failFast),
      '{ ${ td }.runtimeData }
    )
}
