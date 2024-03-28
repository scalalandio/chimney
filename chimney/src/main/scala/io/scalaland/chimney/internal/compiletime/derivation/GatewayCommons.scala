package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}

private[compiletime] trait GatewayCommons { this: Definitions =>

  /** Assigns `expr` value to newly created val, and then uses reference to the reference to this val.
    *
    * It avoids recalculating the same expression in the runtime, "stabilizes" the val if it's needed, etc.
    */
  protected def cacheDefinition[A: Type, Out: Type](expr: Expr[A])(usage: Expr[A] => Expr[Out]): Expr[Out] =
    PrependDefinitionsTo.prependVal[A](expr, ExprPromise.NameGenerationStrategy.FromType).use(usage)

  /** Let us keep the information if logging is needed in code that never had access to Context. */
  protected def enableLoggingIfFlagEnabled[A](
      result: DerivationResult[A],
      isMacroLoggingEnabled: Boolean,
      derivationStartedAt: java.time.Instant
  ): DerivationResult[A] =
    if (isMacroLoggingEnabled) DerivationResult.enableLogPrinting(derivationStartedAt) >> result
    else result

  /** Unwraps the `result` and fails with error message or prints diagnostics (if needed) before returning expression */
  protected def extractExprAndLog[Out: Type](result: DerivationResult[Expr[Out]], errorHeader: => String): Expr[Out] = {
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
          s"""$errorHeader
             |
             |$lines
             |Consult ${Console.MAGENTA}$chimneyDocUrl${Console.RESET} for usage examples.
             |
             |""".stripMargin

        reportError(richLines)
      },
      identity
    )
  }

  private val chimneyDocUrl = "https://chimney.readthedocs.io"
}
