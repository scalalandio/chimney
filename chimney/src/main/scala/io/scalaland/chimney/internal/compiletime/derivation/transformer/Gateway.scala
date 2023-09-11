package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.GatewayCommons
import io.scalaland.chimney.internal.runtime
import io.scalaland.chimney.partial

private[compiletime] trait Gateway extends GatewayCommons { this: Derivation =>

  import ChimneyType.Implicits.*

  // Intended for: being called from platform-specific code which returns Expr directly to splicing site

  final def deriveTotalTransformationResult[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      InstanceFlags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      src: Expr[From],
      runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
  ): Expr[To] = cacheDefinition(runtimeDataStore) { runtimeDataStore =>
    cacheDefinition(src) { src =>
      val context = TransformationContext.ForTotal
        .create[From, To](
          src,
          TransformerConfigurations.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
          runtimeDataStore
        )
        .updateConfig(_.allowFromToImplicitSearch)

      val result = enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context)

      Expr.block(
        List(Expr.suppressUnused(runtimeDataStore), Expr.suppressUnused(src)),
        extractExprAndLog[From, To, To](result)
      )
    }
  }

  final def deriveTotalTransformer[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      InstanceFlags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
  ): Expr[Transformer[From, To]] = cacheDefinition(runtimeDataStore) { runtimeDataStore =>
    val result = DerivationResult.direct[Expr[To], Expr[Transformer[From, To]]] { await =>
      ChimneyExpr.Transformer.instance[From, To] { (src: Expr[From]) =>
        val context = TransformationContext.ForTotal
          .create[From, To](
            src,
            TransformerConfigurations.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
            runtimeDataStore
          )

        await(enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context))
      }
    }

    Expr.block(
      List(Expr.suppressUnused(runtimeDataStore)),
      extractExprAndLog[From, To, Transformer[From, To]](result)
    )
  }

  final def derivePartialTransformationResult[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      InstanceFlags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      src: Expr[From],
      failFast: Expr[Boolean],
      runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
  ): Expr[partial.Result[To]] = cacheDefinition(runtimeDataStore) { runtimeDataStore =>
    cacheDefinition(src) { src =>
      val context = TransformationContext.ForPartial
        .create[From, To](
          src,
          failFast,
          TransformerConfigurations.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
          runtimeDataStore
        )
        .updateConfig(_.allowFromToImplicitSearch)

      val result = enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context)

      Expr.block(
        List(Expr.suppressUnused(runtimeDataStore), Expr.suppressUnused(src)),
        extractExprAndLog[From, To, partial.Result[To]](result)
      )
    }
  }

  final def derivePartialTransformer[
      From: Type,
      To: Type,
      Cfg <: runtime.TransformerCfg: Type,
      InstanceFlags <: runtime.TransformerFlags: Type,
      ImplicitScopeFlags <: runtime.TransformerFlags: Type
  ](
      runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
  ): Expr[PartialTransformer[From, To]] = cacheDefinition(runtimeDataStore) { runtimeDataStore =>
    val result = DerivationResult.direct[Expr[partial.Result[To]], Expr[PartialTransformer[From, To]]] { await =>
      ChimneyExpr.PartialTransformer.instance[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
        val context = TransformationContext.ForPartial
          .create[From, To](
            src,
            failFast,
            TransformerConfigurations.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
            runtimeDataStore
          )

        await(enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context))
      }
    }

    Expr.block(
      List(Expr.suppressUnused(runtimeDataStore)),
      extractExprAndLog[From, To, PartialTransformer[From, To]](result)
    )
  }

  /** Adapts TransformationExpr[To] to expected type of transformation */
  def deriveFinalTransformationResultExpr[From, To](implicit
      ctx: TransformationContext[From, To]
  ): DerivationResult[Expr[ctx.Target]] =
    DerivationResult.log(s"Start derivation with context: $ctx") >>
      deriveTransformationResultExpr[From, To]
        .map { transformationExpr =>
          ctx.fold(_ => transformationExpr.ensureTotal.asInstanceOf[Expr[ctx.Target]])(_ =>
            transformationExpr.ensurePartial.asInstanceOf[Expr[ctx.Target]]
          )
        }

  private def enableLoggingIfFlagEnabled[A](
      result: => DerivationResult[A],
      ctx: TransformationContext[?, ?]
  ): DerivationResult[A] =
    enableLoggingIfFlagEnabled[A](
      DerivationResult.catchFatalErrors(result),
      ctx.config.flags.displayMacrosLogging,
      ctx.derivationStartedAt
    )

  private def extractExprAndLog[From: Type, To: Type, Out: Type](result: DerivationResult[Expr[Out]]): Expr[Out] =
    extractExprAndLog[Out](
      result,
      s"""Chimney can't derive transformation from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}"""
    )
}
