package io.scalaland.chimney.internal.compiletime.derivation

import hearth.fp.data.NonEmptyVector
import hearth.fp.effect.{DontRender, Log, MIO, RenderFrom}
import io.scalaland.chimney.internal.compiletime.{ChimneyDefinitions, DerivationError}

import scala.concurrent.duration.FiniteDuration

/** Shared logic of the transformer/patcher Gateways.
  *
  * [[extractExprAndLog]] runs the (lazy, MIO-backed) derivation program through Hearth's blessed
  * [[hearth.MIOIntegrations.MioExprOps.runToExprOrFail]] runner (rather than calling `unsafe.runSync` by hand): it sets
  * a generous timeout, wires up Hearth's benchmark/flame-graph facilities, renders the success-path journal (when
  * `MacrosLogging` is enabled) and, on failure, hands the accumulated errors to our [[renderFailure]]-style callback so
  * the user-facing message stays byte-identical.
  *
  * KNOWN DIVERGENCES vs the pre-Hearth (hand-rolled `unsafe.runSync`) journal:
  *   - The success-path `MacrosLogging` dump is now rendered by Hearth (`state.logs.render`) in Hearth's tree shape
  *     (`├`/`└` guides, `[Info]` prefixes, a root-scope header, scope durations) instead of the old `Log.Journal#print`
  *     shape (`+ `/`| `). Hearth's [[hearth.fp.effect.LogRendering]] is only a level-filter (`DontRender`/`RenderFrom`/
  *     `RenderOnly`) - the tree-rendering scheme itself is hardcoded and `private[effect]`, and `runToExprOrFail` does
  *     not expose `state.logs` to the caller, so the old byte-exact shape cannot be reproduced through it. No test
  *     asserts on the journal shape (the only `enableMacrosLogging` test usage is commented out), so this is a
  *     documented, untested divergence rather than a broken contract.
  *   - `Warn`/`Error`-level entries (emitted only by MIO internals) stay unrendered - we pass `DontRender` for both, to
  *     match the old behavior of never surfacing them.
  */
private[compiletime] trait GatewayCommons {
  this: ChimneyDefinitions & hearth.MacroCommons & hearth.std.StdExtensions =>

  /** Assigns `expr` value to newly created val, and then uses reference to the reference to this val.
    *
    * It avoids recalculating the same expression in the runtime, "stabilizes" the val if it's needed, etc.
    */
  protected def cacheDefinition[A: Type, Out: Type](expr: Expr[A])(usage: Expr[A] => Expr[Out]): Expr[Out] =
    ValDefs.createVal[A](expr, FreshName.FromType).use(usage)

  /** When macro-logging is enabled, append the two trailer entries the old journal dump used to add out-of-band
    * ("Derived final expression is: ..." and "Derivation took ...") as regular `Info` logs, so they show up in the
    * journal Hearth renders on the success path. Otherwise a no-op (no logs are rendered - see [[extractExprAndLog]]).
    */
  protected def enableLoggingIfFlagEnabled[Out](
      result: MIO[Expr[Out]],
      isMacroLoggingEnabled: Boolean,
      derivationStartedAt: java.time.Instant
  ): MIO[Expr[Out]] =
    if (isMacroLoggingEnabled)
      result.flatTap { expr =>
        val duration = java.time.Duration.between(derivationStartedAt, java.time.Instant.now())
        Log.info(s"Derived final expression is:\n${expr.prettyPrint}") >>
          Log.info(f"Derivation took ${duration.getSeconds}%d.${duration.getNano}%09d s")
      }
    else result

  /** Runs the derivation program and either returns its expression or aborts with the user-facing error message.
    *
    * Delegates to Hearth's [[hearth.MIOIntegrations.MioExprOps.runToExprOrFail]] (the blessed runner) instead of
    * calling `unsafe.runSync` directly: `runToExprOrFail` configures MIO benchmarking, writes flame graphs when the
    * `-Xmacro-settings:hearth.mioBenchmark*` options are set, renders the success-path journal and reports the failure
    * message we build in [[renderFailure]].
    */
  protected def extractExprAndLog[Out](
      result: MIO[Expr[Out]],
      errorHeader: => String,
      isMacroLoggingEnabled: Boolean
  ): Expr[Out] = {
    // Builds Chimney's exact user-facing error text (errorHeader + DerivationErrors prettyPrint + doc-URL footer). The
    // `renderedLogs` are the journal Hearth already rendered for us; they are empty unless MacrosLogging is enabled and
    // the derivation failed, so every error-message test (which does not enable logging) still sees byte-identical text.
    def renderFailure(renderedLogs: String, errors: NonEmptyVector[Throwable]): String = {
      val lines = DerivationError.printErrors(errors)

      val richLines =
        s"""$errorHeader
           |
           |$lines
           |Consult ${Console.MAGENTA}$chimneyDocUrl${Console.RESET} for usage examples.
           |
           |""".stripMargin

      if (renderedLogs.nonEmpty) s"$renderedLogs\n$richLines" else richLines
    }

    // Chimney's macro-dependent transformers derive nested instances by summoning implicits MID-derivation (e.g. an
    // implicit `Transformer[Option[List[A]], List[B]]` that needs `Transformer.AutoDerived[A, B]`). Summoning such an
    // implicit makes the COMPILER expand a separate transform macro - a genuinely distinct top-level `runToExprOrFail`
    // with its own entry point - while this outer one's `runSync` is still on the JVM stack. `runToExprOrFail` installs
    // a single global MIO deadline via `Environment.withMioTimeout`, which by design throws on re-entry (hearth#342,
    // closed as by-design: `runToExprOrFail` is meant to be called once, at the top level, and the deadline/aggregation
    // are single-top-level settings). Because the inner expansion is a SEPARATE macro run - not chimney re-entering its
    // own run - we neutralize the global deadline (a public `var`) around the summon and restore it afterwards, so the
    // inner run installs its own deadline and the outer resumes once it returns. (This is a deliberate accommodation of
    // compiler-driven nested expansion, not a `flatMap`/DirectStyle-composable nesting; restructuring so the compiler
    // never expands a macro-dependent implicit inside a run - deriving those nested instances within the same MIO
    // program instead - is a larger engine change tracked separately.)
    val savedTimeoutDeadline = MIO.timeoutDeadlineNanos
    MIO.timeoutDeadlineNanos = Long.MaxValue
    try
      result.runToExprOrFail(
        macroName = macroName,
        // On success `RenderFrom(Info)` makes Hearth dump the whole journal (only when the flag is on - otherwise
        // `DontRender` keeps non-logging derivations silent, as before). `Warn`/`Error` stay `DontRender` to match the
        // old behavior of never surfacing MIO-internal warnings/errors.
        infoRendering = if (isMacroLoggingEnabled) RenderFrom(Log.Level.Info) else DontRender,
        warnRendering = DontRender,
        errorRendering = DontRender,
        failOnErrorLog = false,
        timeout = macroExpansionTimeout
      )(renderFailure)
    catch {
      // A fatal StackOverflowError flies out of `runToExprOrFail`: Hearth's `handleMioTerminationException` only catches
      // MioTerminationException/timeout, and MIO's run loop only catches `NonFatal`. Catch it here to keep the "-Xss64m"
      // guidance (the state - so the logs - is lost with the unwound stack, hence no rendered logs are available).
      case error: StackOverflowError =>
        reportError(renderFailure("", NonEmptyVector.one[Throwable](DerivationError.MacroException(error))))
    } finally
      MIO.timeoutDeadlineNanos = savedTimeoutDeadline
  }

  /** `{ ${ statement1 }; ...; ${ expr } }` - prepends suppress-unused statements in front of the extracted expr. */
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

  /** Top scope of the journal Hearth renders and the name it uses in a timeout message. */
  private val macroName = "Chimney"

  /** Chimney had no explicit timeout (the old hand-rolled `unsafe.runSync` ran unbounded); Hearth's default is 2s,
    * which is far too low for large derivations. Pick a generous ceiling so ordinary compiles never time out spuriously
    * (manual termination via Ctrl+C still works through Hearth's `TerminationObserver` regardless of this value).
    */
  private val macroExpansionTimeout: FiniteDuration =
    FiniteDuration(10, java.util.concurrent.TimeUnit.MINUTES)

  private val chimneyDocUrl = "https://chimney.readthedocs.io"
}
