package io.scalaland.chimney.internal.compiletime.derivation.patcher

import hearth.fp.effect.MIO
import io.scalaland.chimney.dsl.PatcherDefinitionCommons
import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.compiletime.derivation.GatewayCommons
import io.scalaland.chimney.internal.runtime

private[compiletime] trait Gateway extends GatewayCommons {
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

            prependSuppressUnused(
              List(Expr.suppressUnused(runtimeDataStore), Expr.suppressUnused(obj), Expr.suppressUnused(patch))
            )(extractExprAndLog[A, Patch, A](result))
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
        // The body derivation runs as a lazy MIO into a generated def; the `patch` method calls it (see ChimneyExprs).
        val result = ChimneyExpr.Patcher.instance[A, Patch] { (obj: Expr[A], patch: Expr[Patch]) =>
          val context = PatcherContext.create[A, Patch](
            obj,
            patch,
            config =
              PatcherConfigurations.readPatcherConfiguration[Overrides, Flags, ImplicitScopeFlags](runtimeDataStore)
          )

          enableLoggingIfFlagEnabled(derivePatcherResultExpr(context), context)
        }

        prependSuppressUnused(List(Expr.suppressUnused(runtimeDataStore)))(
          extractExprAndLog[A, Patch, Patcher[A, Patch]](result)
        )
      }
    }
  }

  private def enableLoggingIfFlagEnabled[A](
      result: => MIO[A],
      ctx: PatcherContext[?, ?]
  ): MIO[A] =
    enableLoggingIfFlagEnabled[A](result, ctx.config.flags.displayMacrosLogging, ctx.derivationStartedAt)

  private def extractExprAndLog[A: Type, Patch: Type, Out: Type](result: MIO[Expr[Out]]): Expr[Out] =
    extractExprAndLog[Out](
      result,
      s"""Chimney can't derive patching for ${Type.prettyPrint[A]} with patch type ${Type.prettyPrint[Patch]}"""
    )
}
