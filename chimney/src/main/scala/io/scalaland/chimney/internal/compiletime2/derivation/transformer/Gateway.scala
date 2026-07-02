package io.scalaland.chimney.internal.compiletime2.derivation.transformer

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.GatewayCommons
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

/** Hearth-based port of `...compiletime.derivation.transformer.Gateway`.
  *
  * Differences vs the old version:
  *   - every public entry point calls `ensureStandardExtensionsLoaded()` first (Hearth's `IsOption`/`IsEither`/...
  *     providers return nothing until `Environment.loadStandardExtensions()` ran; the call is idempotent per bundle),
  *   - `DerivationResult.catchFatalErrors` disappears from the private `enableLoggingIfFlagEnabled` - the
  *     `DerivationResult` is a lazy MIO now, so nothing runs until `extractExprAndLog`'s `unsafe.runSync`, which is
  *     where [[GatewayCommons]] catches fatal errors instead,
  *   - `Expr.block` becomes the [[io.scalaland.chimney.internal.compiletime2.MacroCommonsCompat.blockExpr]] compat
  *     helper (pairwise-nested blocks, semantically identical),
  *   - `DerivationResult.direct`+`await` stays (now backed by MIO's `DirectStyle`) - `ChimneyExpr.*.instance` take pure
  *     `Expr => Expr` functions, so the effect must be unwrapped inside the generated-class body, exactly like before.
  */
private[compiletime2] trait Gateway extends GatewayCommons {
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

          blockExpr(
            List(Expr.suppressUnused(runtimeDataStore), Expr.suppressUnused(src)),
            extractExprAndLog[From, To, To](result)
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
        // transformerInstanceCompat: on Scala 3 the old direct+await-inside-the-quote shape trips -Xcheck-macros
        // (see ChimneyExprs) - the compat runs the derivation BEFORE constructing the instance quote there.
        val result = transformerInstanceCompat[From, To] { (src: Expr[From]) =>
          val context = TransformationContext.ForTotal
            .create[From, To](
              src,
              TransformerConfigurations.readTransformerConfiguration[Tail, InstanceFlags, ImplicitScopeFlags](
                runtimeDataStore
              )
            )

          enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context)
        }

        blockExpr(
          List(Expr.suppressUnused(runtimeDataStore)),
          extractExprAndLog[From, To, Transformer[From, To]](result)
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

          blockExpr(
            List(Expr.suppressUnused(runtimeDataStore), Expr.suppressUnused(src)),
            extractExprAndLog[From, To, partial.Result[To]](result)
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
        // partialTransformerInstanceCompat: on Scala 3 the old direct+await-inside-the-quote shape trips
        // -Xcheck-macros (see ChimneyExprs) - the compat runs the derivation BEFORE constructing the quote there.
        val result = partialTransformerInstanceCompat[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
          val context = TransformationContext.ForPartial
            .create[From, To](
              src,
              failFast,
              TransformerConfigurations.readTransformerConfiguration[Tail, InstanceFlags, ImplicitScopeFlags](
                runtimeDataStore
              )
            )

          enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context)
        }

        blockExpr(
          List(Expr.suppressUnused(runtimeDataStore)),
          extractExprAndLog[From, To, PartialTransformer[From, To]](result)
        )
      }
    }
  }

  /** Adapts TransformationExpr[To] to expected type of transformation */
  def deriveFinalTransformationResultExpr[From, To](implicit
      ctx: TransformationContext[From, To]
  ): DerivationResult[Expr[ctx.Target]] =
    DerivationResult.log(s"Start derivation with context: $ctx") >>
      deriveTransformationResultExpr[From, To]
        .map { transformationExpr =>
          ctx.fold(_ => transformationExpr.ensureTotal.asInstanceOf[Expr[ctx.Target]])(_ =>
            transformationExpr.ensurePartial.unsealErrorPath.asInstanceOf[Expr[ctx.Target]]
          )
        }

  private def enableLoggingIfFlagEnabled[A](
      result: => DerivationResult[A],
      ctx: TransformationContext[?, ?]
  ): DerivationResult[A] =
    enableLoggingIfFlagEnabled[A](result, ctx.config.flags.displayMacrosLogging, ctx.derivationStartedAt)

  private def extractExprAndLog[From: Type, To: Type, Out: Type](result: DerivationResult[Expr[Out]]): Expr[Out] =
    extractExprAndLog[Out](
      result,
      s"""Chimney can't derive transformation from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}"""
    )
}
