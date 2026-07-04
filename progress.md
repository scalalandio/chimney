# Hearth migration progress

## it.35 — Chimney-specific, engine-aware macro-extension SPI (`ChimneyMacroExtension`)

Owner's headline feature: Chimney's OWN macro extensions where an integration is asked whether it special-cases a pair
of types, with the handlers getting access to the derivation engine (incl. recursive derivation), so they support both
top-level types AND types whose outer layer the extension builds while deferring inner values to Chimney recursively.

### The SPI as implemented (owner reviews this)

Reuses Hearth's generic `MacroExtension[Macro]`/`ServiceLoader` machinery with a Chimney-specific `Macro` type (NOT a
parallel loader) - exactly mirroring `hearth.std.StandardMacroExtension = MacroExtension[MacroCommons & StdExtensions]`.

- `io.scalaland.chimney.integrations.ChimneyMacroExtension` (public-facing base, `private[chimney]` for now):
  `abstract class ChimneyMacroExtension extends hearth.MacroExtension[hearth.MacroCommons & ChimneyEngineExtensionApi]`.
  Integration authors extend it and implement `def extend(ctx: MacroCommons & ChimneyEngineExtensionApi): Unit`,
  registered via `META-INF/services/io.scalaland.chimney.integrations.ChimneyMacroExtension`.
- `…derivation.transformer.ChimneyEngineExtensionApi` (`private[chimney]`, mixed into the transformer `Derivation`
  cake; `extends Contexts with rules.TransformationRules` so the re-exposed engine types resolve on `ctx` cross-file):
  - `type SpecialCaseContext[From, To] = TransformationContext[From, To]`,
    `type DerivedExpr[A] = TransformationExpr[A]` (re-exposed engine types),
  - `trait SpecialCaseHandler { def apply[From, To](implicit Type[From], Type[To]): Option[SpecialCasedTransformation[From, To]] }`,
  - `trait SpecialCasedTransformation[From, To] { def specialCase(implicit ctx: SpecialCaseContext[From, To]): MIO[Option[DerivedExpr[To]]] }`
    (`Some(expr)` = produced; `None` = decline-after-matching → derivation continues),
  - `object IsChimneySpecialCased { def unapply[From, To]((Type[From], Type[To])): Option[SpecialCasedTransformation[From, To]] }` (owner's extractor),
  - `registerSpecialCase(handler)`, result builders `specialCasedTotal/Partial/Expr`, `specialCaseYield`,
  - recursion: `deriveInner[InnerFrom: Type, InnerTo: Type](Expr[InnerFrom])(implicit SpecialCaseContext[?, ?]): MIO[DerivedExpr[InnerTo]]`
    (delegates to the engine's `deriveRecursiveTransformationExpr`; N inners = N calls, composed via `DerivedExpr.flatMap/map`),
  - context accessors `sourceOf`/`isPartialContext`/`prefersPartialTransformer` (the `TransformationContext` members
    stay `protected`; reached only via these facade helpers),
  - `ensureChimneyMacroExtensionsLoaded()` load-once guard (mirrors `ensureStandardExtensionsLoaded`), calling
    `Environment.loadMacroExtensions[ChimneyMacroExtension]`.
- New rule `…rules.TransformSpecialCasedRuleModule` (`TransformSpecialCasedRule`): loads handlers once, then
  `(Type[From], Type[To]) match { case IsChimneySpecialCased(handler) => handler.specialCase.map{Some→Expanded, None→AttemptNextRule}; case _ => attemptNextRule }`.

Matching the owner's sketch literally:
```
(Type[From], Type[To]) match {
  case IsChimneySpecialCased(handler) => handler.specialCase(using ctx) // rule matched
  case _                              => // rule yielded
}
```

### Slot / precedence

`rulesAvailableForPlatform`: inserted AFTER the 4 implicit rules
(`TransformImplicit`, `…PartialFallbackToTotal`, `…ImplicitOuterTransformer`, `…ImplicitConversion`) and BEFORE
`TransformSubtypesRule`. So user/`integrations` implicits keep priority; a registered handler beats the built-in
structural rules. Precedence test (engine-test module) confirms a user `implicit Transformer[Int, TestSpecialLeaf]`
BEATS the handler (`+1000` marker observed).

### How recursive inner derivation is threaded

The handler builds the OUTER `Expr` and calls `deriveInner[InnerFrom, InnerTo](innerExpr)` per inner value; each returns
`MIO[DerivedExpr[InnerTo]]` (the engine's `TransformationExpr`, which is `MIO`-deferred and only run inside the rule's
expansion, i.e. inside the splicing context - CROSS-QUOTES contract respected, same as `TransformImplicitOuterTransformerRule`).
The handler composes N of them via `DerivedExpr.flatMap`/`map` (which thread total↔partial automatically), so a partial
inner makes the whole outer partial. Proven with `TestBox2[A, B] → TestSpecialBox2[C, D]` (N = 2), where the inner
`Int → TestSpecialLeaf` re-hits the very same rule recursively.

### Protobuf migration (the referee)

Reimplemented as `ProtobufsChimneyMacroExtension` handlers (registered via
`META-INF/services/io.scalaland.chimney.integrations.ChimneyMacroExtension`), DELETED the corresponding implicits:

Became handlers (leaf, no inner derivation - pure outer transforms):
- proto `Duration` ↔ `java.time.Duration` (total both ways),
- proto `Duration` ↔ `FiniteDuration` (total both ways),
- proto `Duration` → `scala.concurrent.duration.Duration` (total, upcast) and `Duration` → proto `Duration`
  (PARTIAL, rejects `Duration.Infinite`) — the total/partial asymmetry `IsValueType` (one inner type) can't express,
- `Empty` → `Unit` (total; PARTIAL `fromEmpty` when `enableImplicitConflictResolution(PreferPartialTransformer)` — the
  handler reproduces the old total/partial conflict-resolution over the single pair),
- any `A` → `Empty` (total; incl. case objects), `Empty` → any `A` (partial `fromEmpty`; yields in total context).

Deleted implicits: `totalTransformerFromEmptyToUnitInstance`, `totalTransformerToEmptyInstance`,
`partialTransformerFromEmptyInstance`, and all 5 total + 1 partial `Duration` instances.
`ProtobufsTransformerImplicits` is now EMPTY.

STAYED as implicits (documented): the empty-oneof/`sealedoneof`/`UnrecognizedEnum` partial instances - they match a
BOUNDED `From` type FAMILY for ANY `To` (whole-family Implicit-rule hook, not a concrete pair a handler matches), and
`partialTransformerFromEmptySealedOneOfInstance` is summoned directly by a spec; plus `DefaultValue[UnknownFieldSet]`.

Semantic refinement (documented): for the `Empty ↔ Unit` pair, an UNRESOLVED partial context (no
`enableImplicitConflictResolution`) previously reported implicit ambiguity (total vs partial); the single handler now
resolves it deterministically toward the total path. No spec asserted that ambiguity; both resolution tests
(`PreferTotal → Some(())`, `PreferPartial → None`) pass.

### Tests

- `ProtobufExtensionProvidersSpec` "kept-implicit boundary" group (which asserted Duration/Empty STILL need the import)
  REWRITTEN to prove they now derive import-free through the handler (all Duration partners incl. the partial
  Infinite-rejection, and Empty↔Unit / any↔Empty / partial Empty→A). All protobuf specs now **39** (was 36), all green ×2.
- New module `chimney-chimney-extension-test` (self-contained, `dependsOn(chimney)`; the chimney-protobufs pattern):
  `TestChimneyMacroExtension` (Compile) + `ChimneyMacroExtensionSpec` (Test, 5 tests) prove
  (a) special-cased pair derives total+partial (`Int`/`String → TestSpecialLeaf`),
  (b) recursive N=2 inner derivation (`TestBox2 → TestSpecialBox2`, incl. partiality propagation, inner re-hits the rule),
  (c) precedence (user implicit BEATS the handler),
  (d) it works from a SEPARATELY-COMPILED artifact via `ServiceLoader` (no import brings the conversions into scope).
  Green ×2.

### ServiceLoader wiring / build

- Reused Hearth's `Environment.loadMacroExtensions[ChimneyMacroExtension]` (same as `loadStandardExtensions`).
- `chimney-protobufs` and the new `chimney-chimney-extension-test` each ship a
  `META-INF/services/io.scalaland.chimney.integrations.ChimneyMacroExtension`.
- Could NOT put the proof handler in `chimney-engine-test-extension`: implementing a Chimney SPI needs `chimney` on the
  Compile classpath, but `chimney` depends on that module `% Test` → adding the back-dep makes sbt's build DSL a
  recursive `lazy val` (and a project-level cycle). The self-contained new module (chimney does NOT depend on it) is the
  cycle-free pattern, identical to `chimney-protobufs`. Registered in the root `.aggregate`.

### Cross-quotes gotchas found (Scala 2)

- Do NOT name the handler's implicit `SpecialCaseContext` param `ctx`: it shadows the extension `ctx` that
  `import ctx.*` relies on, so the Scala-2 reifier prefixes imported facade methods (`sourceOf`, `Type`) with the wrong
  receiver → "value sourceOf is not a member of …SpecialCaseContext". Renamed to `context`.
- Do NOT reference package-object / `enum` CONSTANTS inside `Expr.quote` on Scala 2 (`scala.concurrent.duration.SECONDS`,
  `java.util.concurrent.TimeUnit.SECONDS`): reification emits them by SIMPLE name → "not found: value SECONDS" at the
  splice site. Used the companion METHOD `scala.concurrent.duration.Duration.fromNanos(sec*1e9 + nanos)` instead.
- `Expr`/`Type`/`Option` must not appear in TYPE-annotation position under `import ctx.*` (the term shadows the type):
  either infer, or fully-qualify (`scala.Option`).

### Hearth gap / friction found

- A `private[chimney]` **type** member does not resolve cross-file through a path on Scala 3 ("classfile … missing"):
  had to widen `TransformationContext`/`TransformationExpr` from `protected` to fully public (they live in the
  mima-excluded `internal.compiletime` package; their members are still reached only via facade helpers). A
  `private[chimney]` **def** member resolves fine cross-file - only the type-member case is affected. (Not filed as a
  hearth bug - it is a Chimney visibility choice; noting the asymmetry.)

### Verification totals

- clean-then-test per module (zinc quirk): `chimney3` 1118 (4 ignored), `chimney` 980, `chimneyProtobufs3`/`chimneyProtobufs`
  **39/39** (the migration referee), `chimney-chimney-extension-test` ×2 **5/5**.
- `chimneyCats3`/`chimneyCats`, `chimneyJavaCollections3`/`…`, `chimneySandwichTests3`/`…`, `chimneyJS3`,
  `chimneyEngineTestExtension3`/`…`: see run log (all green).
- Docs snippets: `cd docs && just test-snippets` → GLOBAL "All snippets run succesfully!".

Nothing committed. Branch: hearth-migration.

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
