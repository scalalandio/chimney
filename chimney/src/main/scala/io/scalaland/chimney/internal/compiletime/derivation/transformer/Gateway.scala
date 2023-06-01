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

    val result = deriveTransformationResult(context)

    extractExprAndLog(context)(result)
  }

  final def deriveTotalTransformer[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]): Expr[Transformer[From, To]] =
    instantiateTotalTransformer[From, To] { (src: Expr[From]) =>
      deriveTotalTransformationResult[From, To, Cfg, InstanceFlags, ImplicitScopeFlags](src, runtimeDataStore)
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

    val result = deriveTransformationResult(context)

    extractExprAndLog(context)(result)
  }

  final def derivePartialTransformer[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      ImplicitScopeFlags <: internal.TransformerFlags: Type
  ](runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore]): Expr[PartialTransformer[From, To]] = {
    instantiatePartialTransformer[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
      derivePartialTransformationResult[From, To, Cfg, InstanceFlags, ImplicitScopeFlags](
        src,
        failFast,
        runtimeDataStore
      )
    }
  }

  /** Adapts DerivedExpr[To] to expected type of transformation */
  private def deriveTransformationResult[From, To](implicit
      ctx: TransformerContext[From, To]
  ): DerivationResult[Expr[ctx.Target]] =
    DerivationResult.log(s"Start derivation with context: $ctx") >>
      // pattern match on DerivedExpr and convert to whatever is needed
      deriveTransformationResultExpr[From, To]
        .flatMap {
          case DerivedExpr.TotalExpr(expr) =>
            ctx match {
              case _: TransformerContext.ForTotal[?, ?] =>
                DerivationResult.pure(expr)
              case _: TransformerContext.ForPartial[?, ?] =>
                DerivationResult
                  .pure(ChimneyExpr.PartialResult.Value(expr))
                  .log(
                    s"Derived expression is Total while Partial is expected - adapting by wrapping in partial.Result.Value"
                  )
            }
          case DerivedExpr.PartialExpr(expr) =>
            ctx match {
              case _: TransformerContext.ForTotal[?, ?] =>
                DerivationResult.fromException(
                  new AssertionError("Derived partial.Result expression where total Transformer excepts direct value")
                )
              case _: TransformerContext.ForPartial[?, ?] =>
                DerivationResult.pure(expr)
            }
        }
        .asInstanceOf[DerivationResult[Expr[ctx.Target]]]

  // TODO: rewrite in terms of ExprPromise

  protected def instantiateTotalTransformer[From: Type, To: Type](
      toExpr: Expr[From] => Expr[To]
  ): Expr[Transformer[From, To]]

  protected def instantiatePartialTransformer[From: Type, To: Type](
      toExpr: (Expr[From], Expr[Boolean]) => Expr[partial.Result[To]]
  ): Expr[PartialTransformer[From, To]]

  private def extractExprAndLog[From: Type, To: Type](
      ctx: TransformerContext[From, To]
  )(result: DerivationResult[Expr[ctx.Target]]): Expr[ctx.Target] = {
    if (ctx.config.flags.displayMacrosLogging) {
      val duration = java.time.Duration.between(ctx.derivationStartedAt, java.time.Instant.now())
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
          s"""Chimney can't derive transformation from ${Type[From]} to ${Type[To]}
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
