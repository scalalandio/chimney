package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.{PartialTransformerDefinition, TransformerDefinition}
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.quoted.{Expr, Quotes, Type}

final class TransformerMacros(q: Quotes) extends DerivationPlatform(q) with Gateway {

  import quotes.*, quotes.reflect.*

  def deriveTotalTransformerWithConfig[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]]
  ): Expr[Transformer[From, To]] = suppressWarnings {
    deriveTotalTransformer[From, To, Overrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{ ${ td }.runtimeData })
  }

  def deriveTotalTransformerWithDefaults[
      From: Type,
      To: Type
  ]: Expr[Transformer[From, To]] = suppressWarnings {
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
  }

  def derivePartialTransformerWithDefaults[
      From: Type,
      To: Type
  ]: Expr[PartialTransformer[From, To]] = suppressWarnings {
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
  }

  def derivePartialTransformerWithConfig[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]]
  ): Expr[PartialTransformer[From, To]] = suppressWarnings {
    derivePartialTransformer[From, To, Overrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{
      ${ td }.runtimeData
    })
  }

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ?<[runtime.TransformerFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = scala.quoted.Expr
      .summon[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]]
      .getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
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

  private def suppressWarnings[A: Type](expr: Expr[A]): Expr[A] = '{
    @SuppressWarnings(Array("org.wartremover.warts.All", "all"))
    val result = ${ expr }
    result
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
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]]
  )(using quotes: Quotes): Expr[Transformer[From, To]] =
    new TransformerMacros(quotes).deriveTotalTransformerWithConfig[From, To, Overrides, Flags, ImplicitScopeFlags](td)

  final def deriveTotalTransformerResultWithConfig[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](source: Expr[From], td: Expr[TransformerDefinition[From, To, Overrides, Flags]])(using quotes: Quotes): Expr[To] =
    new TransformerMacros(quotes).deriveTotalTransformationResult[From, To, Overrides, Flags, ImplicitScopeFlags](
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
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]]
  )(using quotes: Quotes): Expr[PartialTransformer[From, To]] =
    new TransformerMacros(quotes).derivePartialTransformerWithConfig[From, To, Overrides, Flags, ImplicitScopeFlags](td)

  final def derivePartialTransformerResultWithConfig[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](source: Expr[From], td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]], failFast: Boolean)(using
      quotes: Quotes
  ): Expr[partial.Result[To]] =
    new TransformerMacros(quotes).derivePartialTransformationResult[From, To, Overrides, Flags, ImplicitScopeFlags](
      source,
      Expr(failFast),
      '{ ${ td }.runtimeData }
    )
}
