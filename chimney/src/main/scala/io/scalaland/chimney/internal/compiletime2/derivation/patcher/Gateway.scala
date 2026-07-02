package io.scalaland.chimney.internal.compiletime2.derivation.patcher

import io.scalaland.chimney.dsl.PatcherDefinitionCommons
import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.GatewayCommons
import io.scalaland.chimney.internal.runtime

/** Hearth-based port of `...compiletime.derivation.patcher.Gateway`.
  *
  * Differences vs the old version: same as the transformer's
  * [[io.scalaland.chimney.internal.compiletime2.derivation.transformer.Gateway]] - `ensureStandardExtensionsLoaded()`
  * on entry, no `DerivationResult.catchFatalErrors` (fatals are caught at `runSync` in [[GatewayCommons]]),
  * `Expr.block` -> `blockExpr`.
  */
private[compiletime2] trait Gateway extends GatewayCommons {
  this: Derivation & hearth.MacroCommons & hearth.std.StdExtensions =>

  import ChimneyType.Implicits.*

  final def derivePatcherResult[
      A: Type,
      Patch: Type,
      Overrides <: runtime.PatcherOverrides: Type,
      Flags <: runtime.PatcherFlags: Type,
      ImplicitScopeFlags <: runtime.PatcherFlags: Type
  ](
      obj: Expr[A],
      patch: Expr[Patch],
      runtimeDataStore: Expr[PatcherDefinitionCommons.RuntimeDataStore]
  ): Expr[A] = {
    ensureStandardExtensionsLoaded()
    suppressWarnings {
      cacheDefinition(runtimeDataStore) { runtimeDataStore =>
        cacheDefinition(obj) { obj =>
          cacheDefinition(patch) { patch =>
            val context = PatcherContext
              .create[A, Patch](
                obj,
                patch,
                config =
                  PatcherConfigurations.readPatcherConfiguration[Overrides, Flags, ImplicitScopeFlags](runtimeDataStore)
              )
              .updateConfig(_.allowAPatchImplicitSearch)

            val result = enableLoggingIfFlagEnabled(derivePatcherResultExpr(context), context)

            blockExpr(
              List(Expr.suppressUnused(runtimeDataStore), Expr.suppressUnused(obj), Expr.suppressUnused(patch)),
              extractExprAndLog[A, Patch, A](result)
            )
          }
        }
      }
    }
  }

  final def derivePatcher[
      A: Type,
      Patch: Type,
      Overrides <: runtime.PatcherOverrides: Type,
      Flags <: runtime.PatcherFlags: Type,
      ImplicitScopeFlags <: runtime.PatcherFlags: Type
  ](
      runtimeDataStore: Expr[PatcherDefinitionCommons.RuntimeDataStore]
  ): Expr[Patcher[A, Patch]] = {
    ensureStandardExtensionsLoaded()
    suppressWarnings {
      cacheDefinition(runtimeDataStore) { runtimeDataStore =>
        // patcherInstanceCompat: on Scala 3 the old direct+await-inside-the-quote shape trips -Xcheck-macros
        // (see ChimneyExprs) - the compat runs the derivation BEFORE constructing the instance quote there.
        val result = patcherInstanceCompat[A, Patch] { (obj: Expr[A], patch: Expr[Patch]) =>
          val context = PatcherContext.create[A, Patch](
            obj,
            patch,
            config =
              PatcherConfigurations.readPatcherConfiguration[Overrides, Flags, ImplicitScopeFlags](runtimeDataStore)
          )

          enableLoggingIfFlagEnabled(derivePatcherResultExpr(context), context)
        }

        blockExpr(
          List(Expr.suppressUnused(runtimeDataStore)),
          extractExprAndLog[A, Patch, Patcher[A, Patch]](result)
        )
      }
    }
  }

  private def enableLoggingIfFlagEnabled[A](
      result: => DerivationResult[A],
      ctx: PatcherContext[?, ?]
  ): DerivationResult[A] =
    enableLoggingIfFlagEnabled[A](result, ctx.config.flags.displayMacrosLogging, ctx.derivationStartedAt)

  private def extractExprAndLog[A: Type, Patch: Type, Out: Type](result: DerivationResult[Expr[Out]]): Expr[Out] =
    extractExprAndLog[Out](
      result,
      s"""Chimney can't derive patching for ${Type.prettyPrint[A]} with patch type ${Type.prettyPrint[Patch]}"""
    )
}
