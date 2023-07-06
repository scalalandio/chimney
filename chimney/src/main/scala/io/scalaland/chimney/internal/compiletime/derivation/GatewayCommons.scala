package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}

private[compiletime] trait GatewayCommons { this: Definitions =>

  protected def cacheDefinition[A: Type, Out: Type](expr: Expr[A])(usage: Expr[A] => Expr[Out]): Expr[Out] =
    PrependDefinitionsTo.prependVal[A](expr, ExprPromise.NameGenerationStrategy.FromType).use(usage)

  protected def enableLoggingIfFlagEnabled[A](
      result: DerivationResult[A],
      isMacroLoggingEnabled: Boolean,
      derivationStartedAt: java.time.Instant
  ): DerivationResult[A] =
    if (isMacroLoggingEnabled) DerivationResult.enableLogPrinting(derivationStartedAt) >> result
    else result

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
