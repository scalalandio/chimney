package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

import scala.reflect.macros.blackbox

/** Hearth-based port of `...compiletime.derivation.transformer.TransformerMacros` (Scala 2).
  *
  * Public methods (names, signatures, type params) mirror the old macro bundle 1:1 so that the binding sites in
  * `io.scalaland.chimney.dsl.*` can flip packages mechanically in the next phase.
  *
  * Differences vs the old version:
  *   - extends the `compiletime` [[PlatformBridge]] (Hearth cake) + the now-shared `Derivation` instead of the old
  *     per-platform `DerivationPlatform` (rules and `summon*Unchecked` were de-platformed in earlier phases),
  *   - the constructor parameter is passed through to [[PlatformBridge]] (which declares `val c`) instead of being
  *     declared here; it is deliberately NOT named `c` so that body references resolve to the inherited member (the
  *     same path Hearth's own internals use) instead of the constructor parameter,
  *   - `Expr.block` -> `blockExpr` compat helper (Hearth has no `Expr.block`; pairwise-nested blocks, semantically
  *     identical),
  *   - `Type.platformSpecific.fromUntyped[A](tpe)` -> `c.WeakTypeTag[A](tpe)` (Hearth's `Type[A]` IS
  *     `c.WeakTypeTag[A]`),
  *   - `?<[A]`/`.as_?<` -> Hearth's `??<:[A]`/`.as_??<:`,
  *   - `Expr.summonImplicit(...)` returns Hearth's `SummoningResult` -> `.toOption` added.
  */
final class TransformerMacros(ctx: blackbox.Context) extends PlatformBridge(ctx) with Derivation with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def deriveTotalTransformationWithConfig[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[To] = retypecheck(
    // Called by TransformerInto => prefix is TransformerInto
    // We're caching it because it is used twice: once for RuntimeDataStore and once for source
    cacheDefinition(c.Expr[dsl.TransformerInto[From, To, Overrides, InstanceFlags]](c.prefix.tree)) { ti =>
      blockExpr(
        List(Expr.suppressUnused(tc)),
        deriveTotalTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](
          src = c.Expr[From](q"$ti.source"),
          runtimeDataStore = c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"$ti.td.runtimeData")
        )
      )
    }
  )

  def deriveTotalTransformerWithDefaults[
      From: WeakTypeTag,
      To: WeakTypeTag
  ]: Expr[Transformer[From, To]] = retypecheck(
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
      deriveTotalTransformer[
        From,
        To,
        runtime.TransformerOverrides.Empty,
        runtime.TransformerFlags.Default,
        ImplicitScopeFlags
      ](ChimneyExpr.RuntimeDataStore.empty)
    }
  )

  def deriveTotalTransformerWithConfig[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[Transformer[From, To]] = retypecheck(
    blockExpr(
      List(Expr.suppressUnused(tc)),
      deriveTotalTransformer[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](
        // Called by TransformerDefinition => prefix is TransformerDefinition
        c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.runtimeData")
      )
    )
  )

  def derivePartialTransformationWithConfigNoFailFast[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[partial.Result[To]] = retypecheck(
    // Called by PartialTransformerInto => prefix is PartialTransformerInto
    // We're caching it because it is used twice: once for RuntimeDataStore and once for source
    cacheDefinition(c.Expr[dsl.PartialTransformerInto[From, To, Overrides, InstanceFlags]](c.prefix.tree)) { pti =>
      blockExpr(
        List(Expr.suppressUnused(tc)),
        derivePartialTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](
          src = c.Expr[From](q"$pti.source"),
          failFast = c.Expr[Boolean](q"false"),
          runtimeDataStore = c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"$pti.td.runtimeData")
        )
      )
    }
  )

  def derivePartialTransformationWithConfigFailFast[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[partial.Result[To]] = retypecheck(
    // Called by PartialTransformerInto => prefix is PartialTransformerInto
    // We're caching it because it is used twice: once for RuntimeDataStore and once for source
    cacheDefinition(c.Expr[dsl.PartialTransformerInto[From, To, Overrides, InstanceFlags]](c.prefix.tree)) { pti =>
      blockExpr(
        List(Expr.suppressUnused(tc)),
        derivePartialTransformationResult[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](
          src = c.Expr[From](q"$pti.source"),
          failFast = c.Expr[Boolean](q"true"),
          runtimeDataStore = c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"$pti.td.runtimeData")
        )
      )
    }
  )

  def derivePartialTransformerWithDefaults[
      From: WeakTypeTag,
      To: WeakTypeTag
  ]: c.universe.Expr[PartialTransformer[From, To]] = retypecheck(
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying
      derivePartialTransformer[
        From,
        To,
        runtime.TransformerOverrides.Empty,
        runtime.TransformerFlags.Default,
        implicitScopeFlagsType.Underlying
      ](ChimneyExpr.RuntimeDataStore.empty)
    }
  )

  def derivePartialTransformerWithConfig[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Overrides <: runtime.TransformerOverrides: WeakTypeTag,
      InstanceFlags <: runtime.TransformerFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.TransformerFlags: WeakTypeTag
  ](
      tc: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[ImplicitScopeFlags]]
  ): Expr[PartialTransformer[From, To]] = retypecheck(
    blockExpr(
      List(Expr.suppressUnused(tc)),
      derivePartialTransformer[From, To, Overrides, InstanceFlags, ImplicitScopeFlags](
        // Called by PartialTransformerDefinition => prefix is PartialTransformerDefinition
        c.Expr[dsl.TransformerDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.runtimeData")
      )
    )
  )

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ??<:[runtime.TransformerFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = {
      val transformerConfigurationType =
        c.WeakTypeTag[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]](
          c.typecheck(
            tree = tq"${typeOf[io.scalaland.chimney.dsl.TransformerConfiguration[? <: runtime.TransformerFlags]]}",
            silent = true,
            mode = c.TYPEmode,
            withImplicitViewsDisabled = true,
            withMacrosDisabled = false
          ).tpe
        )

      Expr.summonImplicit(transformerConfigurationType).toOption.getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        reportError("Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }
    }
    val implicitScopeFlagsType = c
      .WeakTypeTag[runtime.TransformerFlags](implicitScopeConfig.tpe.tpe.typeArgs.head)
      .as_??<:[runtime.TransformerFlags]

    blockExpr(
      List(Expr.suppressUnused(implicitScopeConfig)),
      useImplicitScopeFlags(implicitScopeFlagsType)
    )
  }

  private def retypecheck[A: Type](expr: c.Expr[A]): c.Expr[A] = try {
    val res = c.typecheck(tree = c.untypecheck(expr.tree))
    // expr->untypecheck->typecheck can loose precision, e.g. upcast literal-based singleton type to its parent
    c.Expr[A](if (res.tpe <:< Type[A].tpe) res else q"$res.asInstanceOf[${Type[A]}]")
  } catch {
    case scala.reflect.macros.TypecheckException(_, msg) => c.abort(c.enclosingPosition, msg)
  }
}
