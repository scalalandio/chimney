# Hearth migration progress

## it.34 — Gateway: hand-rolled `unsafe.runSync` → Hearth's `runToExprOrFail`

Owner correction addressed: "you are not following best practices if you call runSync manually."
All 6 gateway entry points now run their derivation program through
`hearth.MIOIntegrations.MioExprOps.runToExprOrFail` instead of `program.unsafe.runSync`.

Files changed (only the Gateway/rendering path + docs, as scoped):
- `chimney/.../derivation/GatewayCommons.scala` — rewrote `extractExprAndLog`; dropped the MLocal
  smuggling + bespoke `renderOldJournalShape`; moved the macro-logging trailer lines into the program.
- `chimney/.../derivation/transformer/Gateway.scala` — thread `displayMacrosLogging` into
  `extractExprAndLog`; hoist config read out of the two `instance` closures.
- `chimney/.../derivation/patcher/Gateway.scala` — same threading/hoist for the 2 patcher entry points.
- `docs/docs/under-the-hood.md` — removed the "flame graphs don't affect Chimney yet" caveat.

LOC delta: `git diff --stat` = 4 files, +147 / -144 (net -3; GatewayCommons -~40 after dropping
`renderOldJournalShape`, offset by the nested-timeout workaround + comments).

### How the 5 required points resolved

1. **Failure output BYTE-IDENTICAL** — YES. `renderFailure(renderedLogs, errors)` rebuilds the exact
   `errorHeader + DerivationError.printErrors(errors) + doc-URL footer` text. `errors: NonEmptyVector[Throwable]`
   is `MErrors`, fed straight to `DerivationError.printErrors` (same partitioning as before). For every
   error-message test macro-logging is off ⇒ `renderedLogs == ""` ⇒ pure `richLines` ⇒ byte-identical.
   All 25 error-asserting specs pass unchanged.

2. **Macro-logging journal dump BYTE-IDENTICAL — NOT ACHIEVABLE via `runToExprOrFail`; documented gap.**
   Two independent reasons, both investigated:
   - `LogRendering` is a closed ADT (`DontRender`/`RenderFrom`/`RenderOnly`) — it only *filters by level*.
     The tree-rendering scheme (`├`/`└` guides, `[Info]` prefixes, root-scope header, scope durations) is
     hardcoded in `Log.render`/`renderTree` and `private[effect]`; it cannot be swapped for Chimney's old
     `+ `/`| ` shape.
   - `runToExprOrFail` does not expose `state.logs` to the caller, and MIO has no public combinator for a
     program to read its own logs, so Chimney can't render its own shape post-run either.
   Resolution: since the ONLY `enableMacrosLogging` test usage is commented out (no spec asserts the journal
   shape), we let Hearth render the journal in ITS shape via `infoRendering = RenderFrom(Info)` (gated on the
   flag; `DontRender` otherwise so non-logging derivations stay silent). The two trailer lines ("Derived final
   expression is: …" / "Derivation took …") are now emitted as `Info` logs inside the program (via
   `enableLoggingIfFlagEnabled`) so they still appear. Divergence documented in the GatewayCommons scaladoc.
   `Warn`/`Error` kept at `DontRender` to match the old behavior of never surfacing MIO-internal entries.

3. **Fatal StackOverflow "-Xss64m" guidance — PRESERVED.** Confirmed `runToExprOrFail`'s
   `handleMioTerminationException` only catches `MioTerminationException`/timeout, and MIO's run loop only
   catches `NonFatal` — a real `StackOverflowError` flies out uncaught. Kept a narrow
   `catch { case e: StackOverflowError => reportError(renderFailure("", one(MacroException(e)))) }` so the
   guidance text stays byte-identical. (Catching only SOE, not `Throwable`, avoids swallowing Hearth's
   internal abort control-flow from `reportErrorAndAbort`.)

4. **Flame-graph flags NOW WORK — PROBED OK.** `runToExprOrFail` calls `configureMioBenchmarking` +
   `writeFlameGraphIfConfigured`. Compiled a `transformInto` probe with
   `-Xmacro-settings:hearth.mioBenchmarkScopes=true` + `hearth.mioBenchmarkFlameGraphDir=…`; a valid
   `Probe.scala_5_69_Chimney.speedscope.json` (7390 bytes, schema OK, 24 frames, 1 profile) was written.
   Removed the "doesn't affect Chimney expansions yet" caveat in under-the-hood.md and stated it works.

5. **Timeout** — Chimney had none (unbounded). Set a generous `10 minutes` (`macroExpansionTimeout`) so
   ordinary compiles never time out spuriously; Ctrl+C termination still works via `TerminationObserver`.

### Hearth gap found (genuine) — filed

`runToExprOrFail` **cannot be nested**: it always calls `Environment.withMioTimeout`, which throws
`HearthAssertionError("MIO timeout is already set")` on re-entry. Chimney's macro-dependent transformers
(e.g. implicit `Transformer[Option[List[A]], List[B]]` needing `Transformer.AutoDerived[A, B]`) summon
implicits MID-derivation, triggering a nested macro expansion / nested `runToExprOrFail`. This regressed the
`TotalTransformerProductSpec` "Option[List[A]] -> List[B]" test (nested derivation failed → fell back to a
"can't transform coproduct instance None to List" error).
Filed: https://github.com/kubuszok/hearth/issues/342 (cited in GatewayCommons.scala).
Workaround in Chimney: `timeoutDeadlineNanos` is a public `var`; save it, set `Long.MaxValue` before the call,
restore in `finally` — the nested `withMioTimeout` then installs its own deadline and the outer resumes.

### Verification totals (all green)

- `chimney3/test`: 1118 passed (4 ignored); `chimney/test`: 980 passed. (clean before each — zinc quirk)
- `chimneyCats/test` 312, `chimneyCats3/test` 312, `chimneyProtobufs/test` 36, `chimneyProtobufs3/test` 36,
  `chimneySandwichTests/test` 6, `chimneySandwichTests3/test` 7, `chimneyJS3/test` 1083 — all passed.
- Flame-graph probe: `.speedscope.json` produced and validated.
- Docs snippets: `just test-snippets` → GLOBAL "All snippets run succesfully!"

Nothing committed. Branch: hearth-migration.
