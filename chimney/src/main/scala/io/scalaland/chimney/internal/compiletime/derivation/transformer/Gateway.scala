package io.scalaland.chimney.internal.compiletime.derivation.transformer

import hearth.fp.effect.{Log, MIO}
import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.derivation.GatewayCommons
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

/** Every public entry point calls `ensureStandardExtensionsLoaded()` first (Hearth's `IsOption`/`IsEither`/...
  * providers return nothing until `Environment.loadStandardExtensions()` ran; the call is idempotent per bundle).
  *
  * `ChimneyExpr.*.instance` take the body derivation as a lazy `MIO` and turn it into a generated def that the instance
  * method calls (the cross-quotes usage-contract recipe - see `ChimneyExprs`). Fatal errors are caught at
  * `unsafe.runSync` in [[GatewayCommons]].
  */
private[compiletime] trait Gateway extends GatewayCommons {
  this: Derivation & hearth.MacroCommons & hearth.std.StdExtensions =>

  import ChimneyType.Implicits.*

  // Intended for: being called from platform-specific code which returns Expr directly to splicing site

  final def deriveTotalTransformationResult[
      From: Type,
      To: Type,
      Tail <: runtime.TransformerOverrides: Type,
      InstanceFlags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      src: Expr[From],
      runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
  ): Expr[To] = {
    ensureStandardExtensionsLoaded()
    suppressWarnings {
      cacheDefinition(runtimeDataStore) { runtimeDataStore =>
        cacheDefinition(src) { src =>
          val context = TransformationContext.ForTotal
            .create[From, To](
              src,
              TransformerConfigurations.readTransformerConfiguration[Tail, InstanceFlags, ImplicitScopeFlags](
                runtimeDataStore
              )
            )
            .updateConfig(_.allowFromToImplicitSummoning)

          val result = enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context)

          prependSuppressUnused(List(Expr.suppressUnused(runtimeDataStore), Expr.suppressUnused(src)))(
            extractExprAndLog[From, To, To](result, context.config.flags.displayMacrosLogging)
          )
        }
      }
    }
  }

  final def deriveTotalTransformer[
      From: Type,
      To: Type,
      Tail <: runtime.TransformerOverrides: Type,
      InstanceFlags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
  ): Expr[Transformer[From, To]] = {
    ensureStandardExtensionsLoaded()
    suppressWarnings {
      cacheDefinition(runtimeDataStore) { runtimeDataStore =>
        // Read the config once, outside the instance body, so the macro-logging flag is known before the derivation
        // runs (the runner needs it up front to decide whether to render the journal); it is pure compile-time code.
        val config =
          TransformerConfigurations.readTransformerConfiguration[Tail, InstanceFlags, ImplicitScopeFlags](
            runtimeDataStore
          )
        // The body derivation runs as a lazy MIO into a generated def; `transform` calls it (see ChimneyExprs).
        val result = ChimneyExpr.Transformer.instance[From, To] { (src: Expr[From]) =>
          val context = TransformationContext.ForTotal.create[From, To](src, config)

          enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context)
        }

        prependSuppressUnused(List(Expr.suppressUnused(runtimeDataStore)))(
          extractExprAndLog[From, To, Transformer[From, To]](result, config.flags.displayMacrosLogging)
        )
      }
    }
  }

  final def derivePartialTransformationResult[
      From: Type,
      To: Type,
      Tail <: runtime.TransformerOverrides: Type,
      InstanceFlags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      src: Expr[From],
      failFast: Expr[Boolean],
      runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
  ): Expr[partial.Result[To]] = {
    ensureStandardExtensionsLoaded()
    suppressWarnings {
      cacheDefinition(runtimeDataStore) { runtimeDataStore =>
        cacheDefinition(src) { src =>
          val context = TransformationContext.ForPartial
            .create[From, To](
              src,
              failFast,
              TransformerConfigurations.readTransformerConfiguration[Tail, InstanceFlags, ImplicitScopeFlags](
                runtimeDataStore
              )
            )
            .updateConfig(_.allowFromToImplicitSummoning)

          val result = enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context)

          prependSuppressUnused(List(Expr.suppressUnused(runtimeDataStore), Expr.suppressUnused(src)))(
            extractExprAndLog[From, To, partial.Result[To]](result, context.config.flags.displayMacrosLogging)
          )
        }
      }
    }
  }

  final def derivePartialTransformer[
      From: Type,
      To: Type,
      Tail <: runtime.TransformerOverrides: Type,
      InstanceFlags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
  ): Expr[PartialTransformer[From, To]] = {
    ensureStandardExtensionsLoaded()
    suppressWarnings {
      cacheDefinition(runtimeDataStore) { runtimeDataStore =>
        // Read the config once, outside the instance body, so the macro-logging flag is known before the derivation
        // runs (the runner needs it up front to decide whether to render the journal); it is pure compile-time code.
        val config =
          TransformerConfigurations.readTransformerConfiguration[Tail, InstanceFlags, ImplicitScopeFlags](
            runtimeDataStore
          )
        // The body derivation runs as a lazy MIO into a generated def; `transform` calls it (see ChimneyExprs).
        val result = ChimneyExpr.PartialTransformer.instance[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
          val context = TransformationContext.ForPartial.create[From, To](src, failFast, config)

          enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context)
        }

        prependSuppressUnused(List(Expr.suppressUnused(runtimeDataStore)))(
          extractExprAndLog[From, To, PartialTransformer[From, To]](result, config.flags.displayMacrosLogging)
        )
      }
    }
  }

  /** Adapts TransformationExpr[To] to expected type of transformation */
  def deriveFinalTransformationResultExpr[From, To](implicit
      ctx: TransformationContext[From, To]
  ): MIO[Expr[ctx.Target]] =
    Log.info(s"Start derivation with context: $ctx") >>
      deriveTransformationResultExpr[From, To]
        .map { transformationExpr =>
          ctx.fold(_ => transformationExpr.ensureTotal.asInstanceOf[Expr[ctx.Target]])(_ =>
            transformationExpr.ensurePartial.unsealErrorPath.asInstanceOf[Expr[ctx.Target]]
          )
        }

  private def enableLoggingIfFlagEnabled[Out](
      result: => MIO[Expr[Out]],
      ctx: TransformationContext[?, ?]
  ): MIO[Expr[Out]] =
    enableLoggingIfFlagEnabled[Out](result, ctx.config.flags.displayMacrosLogging, ctx.derivationStartedAt)

  private def extractExprAndLog[From: Type, To: Type, Out: Type](
      result: MIO[Expr[Out]],
      isMacroLoggingEnabled: Boolean
  ): Expr[Out] =
    extractExprAndLog[Out](
      result,
      s"""Chimney can't derive transformation from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}""",
      isMacroLoggingEnabled
    )
}
