package io.scalaland.chimney.internal.compiletime.derivation

import hearth.fp.data.NonEmptyVector
import hearth.fp.effect.{Log, Logs, MIO, MLocal, MResult}
import io.scalaland.chimney.internal.compiletime.{ChimneyDefinitions, DerivationError}

/** Shared logic of the transformer/patcher Gateways.
  *
  * [[extractExprAndLog]] is where the (lazy, MIO-backed) derivation program actually runs (`unsafe.runSync`). KNOWN
  * DIVERGENCES vs the pre-Hearth journal: on the fatal-error path the logs are lost with the unwound stack, so the
  * `MacrosLogging` journal dump is skipped; `Warn`/`Error`-level entries (emitted only by MIO internals) are not
  * rendered (see [[renderOldJournalShape]]).
  */
private[compiletime] trait GatewayCommons {
  this: ChimneyDefinitions & hearth.MacroCommons & hearth.std.StdExtensions =>

  /** Assigns `expr` value to newly created val, and then uses reference to the reference to this val.
    *
    * It avoids recalculating the same expression in the runtime, "stabilizes" the val if it's needed, etc.
    */
  protected def cacheDefinition[A: Type, Out: Type](expr: Expr[A])(usage: Expr[A] => Expr[Out]): Expr[Out] =
    ValDefs.createVal[A](expr, FreshName.FromType).use(usage)

  /** "The macro-logging flag was enabled, dump the log journal at the end of the derivation". */
  final protected case class MacroLogging(derivationStartedAt: java.time.Instant)

  /** Macro-logging flag as an [[MLocal]] (set by [[enableLoggingIfFlagEnabled]], read - inside the program - by
    * [[extractExprAndLog]]).
    */
  private val macroLogging: MLocal[Option[MacroLogging]] =
    MLocal(Option.empty[MacroLogging])(identity)((a, b) => a.orElse(b))

  /** Let us keep the information if logging is needed in code that never had access to Context. */
  protected def enableLoggingIfFlagEnabled[A](
      result: MIO[A],
      isMacroLoggingEnabled: Boolean,
      derivationStartedAt: java.time.Instant
  ): MIO[A] =
    if (isMacroLoggingEnabled) macroLogging.set(Some(MacroLogging(derivationStartedAt))) >> result
    else result

  /** Unwraps the `result` and fails with error message or prints diagnostics (if needed) before returning expression */
  protected def extractExprAndLog[Out: Type](result: MIO[Expr[Out]], errorHeader: => String): Expr[Out] = {
    // The macroLogging MLocal has to be read INSIDE the program - MState's local accessor is private[effect].
    val program: MIO[(MResult[Expr[Out]], Option[MacroLogging])] =
      result.attempt.tuple(macroLogging.get)

    val (logs, errorsOrExpr, macroLoggingFlag) =
      try {
        val (state, outcome) = program.unsafe.runSync
        outcome match {
          case Right((errorsOrExpr, macroLoggingFlag)) => (state.logs, errorsOrExpr, macroLoggingFlag)
          // $COVERAGE-OFF$unreachable: after `.attempt` neither branch of `.tuple` can fail
          case Left(errors) => (state.logs, Left(errors), None)
          // $COVERAGE-ON$
        }
      } catch {
        // Fatal errors (e.g. StackOverflowError) fly out of runSync; catching them here keeps the "-Xss64m" guidance
        // in the rendered error (the state - so the logs and the macro-logging flag - is lost with the unwound stack).
        case error: Throwable =>
          (Vector.empty[Log], Left(NonEmptyVector.one[Throwable](DerivationError.MacroException(error))), None)
      }

    macroLoggingFlag.foreach { case MacroLogging(derivationStartedAt) =>
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
        val lines = DerivationError.printErrors(derivationErrors)

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

  /** `{ ${ statement1 }; ...; ${ expr } }` - prepends suppress-unused statements in front of the extracted expr.
    *
    * HEARTH 0.4.0 ISSUE WORKAROUND (hearth#317, Scala 3): keep this a def, do NOT inline the quote at call sites with
    * the derivation call inside `Expr.splice(...)` - the splice would then run the whole derivation under the nested
    * quote's `Quotes`, and everything it creates becomes splice-scoped (`-Xcheck-macros`: "Expression created in a
    * splice was used outside of that splice" once Iso/Codec derive their second instance). As an argument of this def
    * the derivation is evaluated eagerly, under the macro-entry context, before any quote is entered.
    */
  protected def prependSuppressUnused[Out: Type](statements: List[Expr[Unit]])(expr: Expr[Out]): Expr[Out] =
    statements.foldRight(expr) { (statement, acc) =>
      Expr.quote {
        Expr.splice(statement)
        Expr.splice(acc)
      }
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
