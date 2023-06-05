package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.{internal, partial, PartialTransformer, Transformer}

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Gateway { this: Derivation =>

  // Intended for: being called from platform-specific code which returns Expr directly to splicing site

  final def deriveTotalTransformationResult[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](src: Expr[From], runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]): Expr[To] = {
    val context = TransformerContext.ForTotal.create[From, To](
      src,
      configurationsImpl.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
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
      ChimneyExpr.Transformer.lift[From, To] { (src: Expr[From]) =>
        val context = TransformerContext.ForTotal.create[From, To](
          src,
          configurationsImpl.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
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
    val context = TransformerContext.ForPartial.create[From, To](
      src,
      failFast,
      configurationsImpl.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
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
      ChimneyExpr.PartialTransformer.lift[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
        val context = TransformerContext.ForPartial.create[From, To](
          src,
          failFast,
          configurationsImpl.readTransformerConfig[Cfg, InstanceFlags, ImplicitScopeFlags],
          runtimeDataStore
        )

        await(enableLoggingIfFlagEnabled(deriveFinalTransformationResultExpr(context), context))
      }
    }

    extractExprAndLog[From, To, PartialTransformer[From, To]](result)
  }

  /** Adapts DerivedExpr[To] to expected type of transformation */
  private def deriveFinalTransformationResultExpr[From, To](implicit
      ctx: TransformerContext[From, To]
  ): DerivationResult[Expr[ctx.Target]] =
    DerivationResult.log(s"Start derivation with context: $ctx") >>
      // pattern match on DerivedExpr and convert to whatever is needed
      deriveTransformationResultExpr[From, To]
        .flatMap { derivedExpr =>
          ctx match {
            case _: TransformerContext.ForTotal[?, ?]   => DerivationResult(derivedExpr.ensureTotal)
            case _: TransformerContext.ForPartial[?, ?] => DerivationResult.pure(derivedExpr.ensurePartial)
          }
        }
        .asInstanceOf[DerivationResult[Expr[ctx.Target]]]

  private def enableLoggingIfFlagEnabled[A](
      result: DerivationResult[A],
      ctx: TransformerContext[?, ?]
  ): DerivationResult[A] =
    if (ctx.config.flags.displayMacrosLogging) DerivationResult.enableLogPrinting(ctx.derivationStartedAt) >> result
    else result

  private def extractExprAndLog[From: Type, To: Type, Out](result: DerivationResult[Expr[Out]]): Expr[Out] = {
    result.state.macroLogging.foreach { case DerivationResult.State.MacroLogging(derivationStartedAt) =>
      val duration = java.time.Duration.between(derivationStartedAt, java.time.Instant.now())
      val info = result
        .logSuccess(expr => s"Derived final expression is:\n${Expr.prettyPrint(expr)}")
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
