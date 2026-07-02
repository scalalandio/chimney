package io.scalaland.chimney.internal.compiletime.derivation

import hearth.fp.effect.{Log, Logs, MResult}
import io.scalaland.chimney.internal.compiletime.{
  ChimneyDefinitions,
  DerivationError,
  DerivationErrors,
  DerivationResult
}

/** Hearth-based port of `...compiletime.derivation.GatewayCommons`.
  *
  * Differences vs the old version (full it.6 semantics-audit resolution):
  *   - [[cacheDefinition]]: `PrependDefinitionsTo.prependVal(_, NameGenerationStrategy.FromType).use` becomes
  *     `ValDefs.createVal(_, FreshName.FromType).use` (same generated `val` + scoping semantics),
  *   - [[extractExprAndLog]]: `DerivationResult` is now a lazy MIO, so this is where the program actually RUNS
  *     (`unsafe.runSync`); consequences:
  *     - the old `State#macroLogging` is an `MLocal` now and the final `MState`'s local-value accessor is
  *       `private[effect]` in Hearth, so it is read INSIDE the program (`result.attempt.tuple(macroLogging.get)`),
  *     - the old `FatalError`-smuggling + `DerivationResult.catchFatalErrors` (removed from the Gateways) is replaced
  *       by a `try`/`catch` around `runSync`: fatal errors (e.g. `StackOverflowError`) still render through
  *       [[DerivationError.printErrors]] with the old "increase -Xss64m" guidance and the same error header/footer;
  *       KNOWN DIVERGENCE: on the fatal path the logs (and the macro-logging flag) are lost with the unwound stack, so
  *       the `MacrosLogging` journal dump is skipped where the old code could still print the journal-so-far,
  *     - the journal dump keeps the OLD `Log.Journal#print` output shape byte-for-byte ([[renderOldJournalShape]]
  *       re-renders Hearth's flat `Logs` with the old `"$indent+ "`/`"$indent| "` scheme instead of Hearth's own
  *       `├`/`└`-style renderer); KNOWN DIVERGENCE: `Warn`/`Error`-level entries are skipped - Chimney logs everything
  *       at `Info`, but MIO itself appends `[Error] Caught exception ...` entries when catching `NonFatal` exceptions,
  *       which the old journal never contained,
  *     - the trailing "Derived final expression is:\n..." (success only) and "Derivation took ..." root-level entries
  *       are appended to the rendered journal exactly like the old `logSuccess`+`log` calls did,
  *   - [[suppressWarnings]]: same `-Xmacro-settings` parsing; `Expr.SuppressWarnings`/`Expr.nowarn` become
  *     [[MacroCommonsCompat.suppressWarningsExpr]]/[[MacroCommonsCompat.nowarnExpr]] (implemented per-platform in the
  *     `PlatformBridge`s - Hearth has no annotation-attaching API).
  */
private[compiletime] trait GatewayCommons {
  this: ChimneyDefinitions & hearth.MacroCommons & hearth.std.StdExtensions =>

  /** Assigns `expr` value to newly created val, and then uses reference to the reference to this val.
    *
    * It avoids recalculating the same expression in the runtime, "stabilizes" the val if it's needed, etc.
    */
  protected def cacheDefinition[A: Type, Out: Type](expr: Expr[A])(usage: Expr[A] => Expr[Out]): Expr[Out] =
    ValDefs.createVal[A](expr, FreshName.FromType).use(usage)

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
    // The macroLogging MLocal has to be read INSIDE the program - MState's local accessor is private[effect].
    val program: DerivationResult[(MResult[Expr[Out]], Option[DerivationResult.MacroLogging])] =
      result.attempt.tuple(DerivationResult.macroLogging.get)

    val (logs, errorsOrExpr, macroLogging) =
      try {
        val (state, outcome) = program.unsafe.runSync
        outcome match {
          case Right((errorsOrExpr, macroLogging)) => (state.logs, errorsOrExpr, macroLogging)
          // $COVERAGE-OFF$unreachable: after `.attempt` neither branch of `.tuple` can fail
          case Left(errors) => (state.logs, Left(errors), None)
          // $COVERAGE-ON$
        }
      } catch {
        // Old code smuggled fatal errors (e.g. StackOverflowError) through FatalError+catchFatalErrors so that they
        // render with the "-Xss64m" guidance; with MIO they fly out of runSync, so they are caught here instead
        // (the state - so the logs and the macro-logging flag - is lost with the unwound stack).
        case error: Throwable =>
          (Vector.empty[Log], Left(DerivationErrors(DerivationError.MacroException(error))), None)
      }

    macroLogging.foreach { case DerivationResult.MacroLogging(derivationStartedAt) =>
      val duration = java.time.Duration.between(derivationStartedAt, java.time.Instant.now())
      val info = renderOldJournalShape(
        logs ++
          errorsOrExpr.toOption.map { expr =>
            Log.Entry(Log.Level.Info, () => s"Derived final expression is:\n${expr.prettyPrint}", parentScopeId = 0)
          }.toList :+
          Log.Entry(
            Log.Level.Info,
            () => f"Derivation took ${duration.getSeconds}%d.${duration.getNano}%09d s",
            parentScopeId = 0
          )
      )
      Environment.reportInfo("\n" + info)
    }

    errorsOrExpr.fold(
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
    val suppressedWarningsExpr = suppressWarningsCfg.fold(expr)(suppressWarningsExpr(_)(expr))

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
    val nowarnedExpr = nowarnCfg.fold(suppressedWarningsExpr)(nowarnExpr(_)(expr))

    nowarnedExpr
  }

  /** Renders Hearth's flat [[Logs]] in the OLD `Log.Journal#print` shape (byte-for-byte for `Info` entries).
    *
    * Hearth's own renderer (`state.logs.render.fromInfo(...)`) uses a different scheme (`├`/`└` guides, `[Info]`
    * prefixes, root-scope header, scope durations), which would break every test asserting on the journal shape - so
    * the tree is reconstructed from the flat parent-scope-ID representation here (Hearth's `Log.toTree` is
    * `private[effect]`) and printed with the old `"$indent+ "`/`"$indent| "` scheme instead.
    *
    * `Warn`/`Error` entries (only ever emitted by MIO's own internals, e.g. "Caught exception ..." - the old journal
    * had no counterpart of those) are skipped; scope markers are always printed, like the old `Log.Scope`.
    */
  private def renderOldJournalShape(logs: Logs): String = {
    val byParent = scala.collection.mutable.Map.empty[Int, scala.collection.mutable.ArrayBuffer[Log]]
    logs.foreach { log =>
      byParent.getOrElseUpdate(log.parentScopeId, scala.collection.mutable.ArrayBuffer.empty[Log]) += log
    }

    val singleIndent = "  "
    def prependIndent(msg: String, indent: String): String = msg.replaceAll("\n", s"\n$indent| ")

    def print(parentScopeId: Int, indent: String): String =
      byParent.get(parentScopeId).fold("") {
        _.map {
          case Log.Entry(level, message, _) =>
            if (level == Log.Level.Info) s"$indent+ ${prependIndent(message(), indent)}\n" else ""
          case Log.Scope(scopeName, scopeId, _, _, _) =>
            s"$indent+ ${prependIndent(scopeName, indent)}\n${print(scopeId, indent + singleIndent)}"
        }.mkString
      }

    print(0, "")
  }

  private val chimneyDocUrl = "https://chimney.readthedocs.io"
}
