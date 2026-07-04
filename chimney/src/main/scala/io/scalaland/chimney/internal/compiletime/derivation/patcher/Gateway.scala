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
            )(extractExprAndLog[A, Patch, A](result, context.config.flags.displayMacrosLogging))
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
        // Read the config once, outside the instance body, so the macro-logging flag is known before the derivation
        // runs (the runner needs it up front to decide whether to render the journal); it is pure compile-time code.
        val config =
          PatcherConfigurations.readPatcherConfiguration[Overrides, Flags, ImplicitScopeFlags](runtimeDataStore)
        // The body derivation runs as a lazy MIO into a generated def; the `patch` method calls it (see ChimneyExprs).
        val result = ChimneyExpr.Patcher.instance[A, Patch] { (obj: Expr[A], patch: Expr[Patch]) =>
          val context = PatcherContext.create[A, Patch](obj, patch, config = config)

          enableLoggingIfFlagEnabled(derivePatcherResultExpr(context), context)
        }

        prependSuppressUnused(List(Expr.suppressUnused(runtimeDataStore)))(
          extractExprAndLog[A, Patch, Patcher[A, Patch]](result, config.flags.displayMacrosLogging)
        )
      }
    }
  }

  private def enableLoggingIfFlagEnabled[Out](
      result: => MIO[Expr[Out]],
      ctx: PatcherContext[?, ?]
  ): MIO[Expr[Out]] =
    enableLoggingIfFlagEnabled[Out](result, ctx.config.flags.displayMacrosLogging, ctx.derivationStartedAt)

  private def extractExprAndLog[A: Type, Patch: Type, Out: Type](
      result: MIO[Expr[Out]],
      isMacroLoggingEnabled: Boolean
  ): Expr[Out] =
    extractExprAndLog[Out](
      result,
      s"""Chimney can't derive patching for ${Type.prettyPrint[A]} with patch type ${Type.prettyPrint[Patch]}""",
      isMacroLoggingEnabled
    )
}
