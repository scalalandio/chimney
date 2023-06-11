package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.{internal, partial, PartialTransformer, Transformer}

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Gateway { this: Derivation =>

  import ChimneyTypeImplicits.*

  // Intended for: being called from platform-specific code which returns Expr directly to splicing site

  final def deriveTotalTransformationResult[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](src: Expr[From], runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]): Expr[To] = {
    val context = TransformationContext.ForTotal.create[From, To](
      src,
      Configurations.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
      runtimeDataStore
    )

    val result = enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context)

    extractExprAndLog[From, To, To](result)
  }

  final def deriveTotalTransformer[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]): Expr[Transformer[From, To]] = {
    val result = DerivationResult.direct[Expr[To], Expr[Transformer[From, To]]] { await =>
      ChimneyExpr.Transformer.instance[From, To] { (src: Expr[From]) =>
        val context = TransformationContext.ForTotal.create[From, To](
          src,
          Configurations.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
          runtimeDataStore
        )

        await(enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context))
      }
    }

    extractExprAndLog[From, To, Transformer[From, To]](result)
  }

  final def derivePartialTransformationResult[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](
      src: Expr[From],
      failFast: Expr[Boolean],
      runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]
  ): Expr[partial.Result[To]] = {
    val context = TransformationContext.ForPartial.create[From, To](
      src,
      failFast,
      Configurations.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
      runtimeDataStore
    )

    val result = enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context)

    extractExprAndLog[From, To, partial.Result[To]](result)
  }

  final def derivePartialTransformer[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]): Expr[PartialTransformer[From, To]] = {
    val result = DerivationResult.direct[Expr[partial.Result[To]], Expr[PartialTransformer[From, To]]] { await =>
      ChimneyExpr.PartialTransformer.instance[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
        val context = TransformationContext.ForPartial.create[From, To](
          src,
          failFast,
          Configurations.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
          runtimeDataStore
        )

        await(enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context))
      }
    }

    extractExprAndLog[From, To, PartialTransformer[From, To]](result)
  }

  /** Adapts TransformationExpr[To] to expected type of transformation */
  private def deriveFinalTransformationResultExpr[From, To](implicit
      ctx: TransformationContext[From, To]
  ): DerivationResult[Expr[ctx.Target]] =
    DerivationResult.log(s"Start derivation with context: $ctx") >>
      // pattern match on TransformationExpr and convert to whatever is needed
      deriveTransformationResultExpr[From, To]
        .map { transformationExpr =>
          ctx.fold(_ => transformationExpr.ensureTotal.asInstanceOf[Expr[ctx.Target]])(_ =>
            transformationExpr.ensurePartial.asInstanceOf[Expr[ctx.Target]]
          )
        }

  private def enableLoggingIfFlagEnabled[A](
      result: DerivationResult[A],
      ctx: TransformationContext[?, ?]
  ): DerivationResult[A] =
    if (ctx.config.flags.displayMacrosLogging) DerivationResult.enableLogPrinting(ctx.derivationStartedAt) >> result
    else result

  private def extractExprAndLog[From: Type, To: Type, Out: Type](result: DerivationResult[Expr[Out]]): Expr[Out] = {
    result.state.macroLogging.foreach { case DerivationResult.State.MacroLogging(derivationStartedAt) =>
      val duration = java.time.Duration.between(derivationStartedAt, java.time.Instant.now())
      val info = result
        .logSuccess(expr => s"Derived final expression is:\n${expr.prettyPrint}")
        .log(f"Derivation took ${duration.getSeconds}%d.${duration.getNano}%09d s")
        .state
        .journal
        .print
      reportInfo("\n" + info)
    }

    result.toEither.fold(
      derivationErrors => {
        val lines = derivationErrors.prettyPrint

        val richLines =
          s"""Chimney can't derive transformation from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}
             |
             |$lines
             |Consult $chimneyDocUrl for usage examples.
             |
             |""".stripMargin

        reportError(richLines)
      },
      identity
    )
  }

  private val chimneyDocUrl = "https://scalalandio.github.io/chimney"
}
