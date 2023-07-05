package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal
import io.scalaland.chimney.internal.compiletime.DerivationResult

trait Gateway { this: Derivation =>

  final def derivePatcherResult[A: Type, Patch: Type, Cfg <: internal.PatcherCfg: Type](
      obj: Expr[A],
      patch: Expr[Patch]
  ): Expr[A] = cacheDefinition(obj) { obj =>
    cacheDefinition(patch) { patch =>
      val context = PatcherContext.create[A, Patch](
        obj,
        patch,
        config = PatcherConfigurations.readPatcherConfig[Cfg]
      )

      val result = enableLoggingIfFlagEnabled(derivePatcherResultExpr(context), context)

      Expr.block(
        List(Expr.suppressUnused(obj), Expr.suppressUnused(patch)),
        extractExprAndLog[A, Patch](result)
      )
    }
  }

  // TODO: move to common utils, name it better
  protected def cacheDefinition[A: Type, Out: Type](expr: Expr[A])(usage: Expr[A] => Expr[Out]): Expr[Out] =
    ExprPromise.promise[A](ExprPromise.NameGenerationStrategy.FromType).fulfilAsVal(expr).use(usage)

  // TODO: move to common utils, name it better
  private def enableLoggingIfFlagEnabled[A](
      result: DerivationResult[A],
      ctx: PatcherContext[?, ?]
  ): DerivationResult[A] =
    if (ctx.config.displayMacrosLogging) DerivationResult.enableLogPrinting(ctx.derivationStartedAt) >> result
    else result

  // TODO: move to common utils, name it better
  private def extractExprAndLog[A: Type, Patch: Type](result: DerivationResult[Expr[A]]): Expr[A] = {
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
          s"""Chimney can't derive patcher for ${Type.prettyPrint[A]} with patch type ${Type.prettyPrint[Patch]}
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
