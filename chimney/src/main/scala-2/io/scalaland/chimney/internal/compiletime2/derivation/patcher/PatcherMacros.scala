package io.scalaland.chimney.internal.compiletime2.derivation.patcher

import io.scalaland.chimney.{dsl, Patcher}
import io.scalaland.chimney.internal.compiletime2.PlatformBridge
import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.blackbox

/** Hearth-based port of `...compiletime.derivation.patcher.PatcherMacros` (Scala 2).
  *
  * Public methods (names, signatures, type params) mirror the old macro bundle 1:1 so that the binding sites in
  * `io.scalaland.chimney.dsl.*` can flip packages mechanically in the next phase.
  *
  * Differences vs the old version: same as the transformer's
  * [[io.scalaland.chimney.internal.compiletime2.derivation.transformer.TransformerMacros]] - extends
  * [[PlatformBridge]] + the now-shared `Derivation`/`Gateway` instead of the old per-platform `DerivationPlatform`,
  * `Expr.block` -> `blockExpr`, `Type.platformSpecific.fromUntyped[A](tpe)` -> `c.WeakTypeTag[A](tpe)`,
  * `?<`/`.as_?<` -> `??<:`/`.as_??<:`, `Expr.summonImplicit(...)` -> `.toOption` added.
  */
final class PatcherMacros(ctx: blackbox.Context) extends PlatformBridge(ctx) with Derivation with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def derivePatchWithConfig[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: runtime.PatcherOverrides: WeakTypeTag,
      Flags <: runtime.PatcherFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.PatcherFlags: WeakTypeTag
  ](
      pc: Expr[io.scalaland.chimney.dsl.PatcherConfiguration[ImplicitScopeFlags]]
  ): c.Expr[A] = retypecheck(
    // Called by PatcherUsing => prefix is PatcherUsing
    cacheDefinition(c.Expr[dsl.PatcherUsing[A, Patch, Overrides, Flags]](c.prefix.tree)) { pu =>
      blockExpr(
        List(Expr.suppressUnused(pc)),
        derivePatcherResult[A, Patch, Overrides, Flags, ImplicitScopeFlags](
          obj = c.Expr[A](q"$pu.obj"),
          patch = c.Expr[Patch](q"$pu.objPatch"),
          runtimeDataStore = c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore](q"$pu.pd.runtimeData")
        )
      )
    }
  )

  def derivePatcherWithConfig[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: runtime.PatcherOverrides: WeakTypeTag,
      InstanceFlags <: runtime.PatcherFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.PatcherFlags: WeakTypeTag
  ](
      pc: Expr[io.scalaland.chimney.dsl.PatcherConfiguration[ImplicitScopeFlags]]
  ): Expr[Patcher[A, Patch]] = retypecheck(
    blockExpr(
      List(Expr.suppressUnused(pc)),
      derivePatcher[A, Patch, Overrides, InstanceFlags, ImplicitScopeFlags](
        // Called by PatcherDefinition => prefix is PatcherDefinition
        c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.runtimeData")
      )
    )
  )

  def derivePatcherWithDefaults[
      A: WeakTypeTag,
      Patch: WeakTypeTag
  ]: Expr[Patcher[A, Patch]] = retypecheck(
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
      derivePatcher[A, Patch, runtime.PatcherOverrides.Empty, runtime.PatcherFlags.Default, ImplicitScopeFlags](
        ChimneyExpr.RuntimeDataStore.empty
      )
    }
  )

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ??<:[runtime.PatcherFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = {
      val patcherConfigurationType =
        c.WeakTypeTag[io.scalaland.chimney.dsl.PatcherConfiguration[? <: runtime.PatcherFlags]](
          c.typecheck(
            tree = tq"${typeOf[io.scalaland.chimney.dsl.PatcherConfiguration[? <: runtime.PatcherFlags]]}",
            silent = true,
            mode = c.TYPEmode,
            withImplicitViewsDisabled = true,
            withMacrosDisabled = false
          ).tpe
        )

      Expr.summonImplicit(patcherConfigurationType).toOption.getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        reportError("Can't locate implicit PatcherConfiguration!")
        // $COVERAGE-ON$
      }
    }
    val implicitScopeFlagsType = c
      .WeakTypeTag[runtime.PatcherFlags](implicitScopeConfig.tpe.tpe.typeArgs.head)
      .as_??<:[runtime.PatcherFlags]

    blockExpr(
      List(Expr.suppressUnused(implicitScopeConfig)),
      useImplicitScopeFlags(implicitScopeFlagsType)
    )
  }

  private def retypecheck[A: Type](expr: c.Expr[A]): c.Expr[A] = try
    c.Expr[A](c.typecheck(tree = c.untypecheck(expr.tree)))
  catch {
    case scala.reflect.macros.TypecheckException(_, msg) => c.abort(c.enclosingPosition, msg)
  }
}
