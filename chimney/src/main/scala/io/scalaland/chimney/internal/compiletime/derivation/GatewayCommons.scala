package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.internal.compiletime.{ChimneyDefinitions, DerivationResult}

private[compiletime] trait GatewayCommons { this: ChimneyDefinitions =>

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

  /** Adds @SuppressWarnings/@nowarn annotation the generated code - allows customizing it with a compiler flag. */
  protected def suppressWarnings[A: Type](expr: Expr[A]): Expr[A] = {
    // Add @SuppressWarnings(...) to the expr:
    // - by default use: "org.wartremover.warts.All" (WartRemover) and "all" (Scapegoat)
    // - overridden with "-Xmacro-settings:chimney.SuppressWarnings=value"
    //   - "-Xmacro-settings:chimney.SuppressWarnings=none" skips the annotation
    //   - "-Xmacro-settings:chimney.SuppressWarnings=a;b;c" would create @SuppressWarnings(Array("a", "b", "c"))
    val suppressWarningsCfg = XMacroSettings.foldLeft(Option(List("org.wartremover.warts.All", "all"))) {
      case (_, chimneyFlag"SuppressWarnings=$value") => if (value == "none") None else Option(value.split(";").toList)
      case (cfg, _)                                  => cfg
    }
    val suppressWarningsExpr = suppressWarningsCfg.fold(expr)(Expr.SuppressWarnings(_)(expr))

    // Add @nowarn(...) to the expr:
    // - by default annotation is not added
    // - overridden with "-Xmacro-settings:chimney.nowarn=value"
    //   - "-Xmacro-settings:chimney.nowarn=none" skips the annotation
    //   - "-Xmacro-settings:chimney.nowarn=true" would create @nowarn
    //   - "-Xmacro-settings:chimney.nowarn=msg" would create @nowarn("msg")
    val nowarnCfg = XMacroSettings.foldLeft(Option.empty[Option[String]]) {
      case (_, chimneyFlag"nowarn=$value") =>
        if (value == "none") None else if (value == "true") Option(None) else Option(Some(value))
      case (cfg, _) => cfg
    }
    val nowarnExpr = nowarnCfg.fold(suppressWarningsExpr)(Expr.nowarn(_)(expr))

    nowarnExpr
  }

  private val chimneyDocUrl = "https://chimney.readthedocs.io"
}
