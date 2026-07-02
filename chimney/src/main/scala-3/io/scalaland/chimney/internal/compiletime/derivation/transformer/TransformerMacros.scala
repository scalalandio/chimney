package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.{PartialTransformerDefinition, TransformerDefinition}
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.quoted.{Expr, Quotes, Type}

/** Hearth-based port of `...compiletime.derivation.transformer.TransformerMacros` (Scala 3).
  *
  * Public methods (names, signatures, type params) of both the class and the companion mirror the old ones 1:1 so that
  * the binding sites in `io.scalaland.chimney.dsl.*` can flip packages mechanically in the next phase.
  *
  * Differences vs the old version:
  *   - extends the `compiletime` [[PlatformBridge]] (Hearth cake) + the now-shared `Derivation` instead of the old
  *     per-platform `DerivationPlatform` (rules and `summon*Unchecked` were de-platformed in earlier phases); inside
  *     the class `Type`/`Expr` are Hearth's members, which on Scala 3 are transparent aliases of the `scala.quoted`
  *     ones, so the companion's static signatures stay identical,
  *   - `Expr.block` -> `blockExpr` compat helper (Hearth has no `Expr.block`; pairwise-nested blocks, semantically
  *     identical),
  *   - `?<[A]`/`.as_?<` -> Hearth's `??<:[A]`/`.as_??<:`.
  */
final class TransformerMacros(q: Quotes) extends PlatformBridge(q) with Derivation with Gateway {

  import quotes.*, quotes.reflect.*

  def deriveTotalTransformerWithConfig[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[TransformerDefinition[From, To, Overrides, Flags]]
  ): Expr[Transformer[From, To]] =
    deriveTotalTransformer[From, To, Overrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{ $td.runtimeData })

  def deriveTotalTransformerWithDefaults[
      From: Type,
      To: Type
  ]: Expr[Transformer[From, To]] = resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
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
  ]: Expr[PartialTransformer[From, To]] = resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
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
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      td: Expr[PartialTransformerDefinition[From, To, Overrides, Flags]]
  ): Expr[PartialTransformer[From, To]] =
    derivePartialTransformer[From, To, Overrides, Flags, ImplicitScopeFlags](runtimeDataStore = '{
      $td.runtimeData
    })

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ??<:[runtime.TransformerFlags] => Expr[A]
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
      .as_??<:[runtime.TransformerFlags]

    blockExpr(
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
      '{ $td.runtimeData }
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
      '{ $td.runtimeData }
    )
}
