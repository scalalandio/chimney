package io.scalaland.chimney.internal.compiletime.derivation.transformer

import hearth.fp.effect.MIO
import hearth.fp.syntax.*
import io.scalaland.chimney.dsl.PreferPartialTransformer
import io.scalaland.chimney.partial

/** Engine surface handed to a [[io.scalaland.chimney.integrations.ChimneyMacroExtension]] when it is loaded (see
  * [[TransformSpecialCasedRuleModule]]).
  *
  * This is Chimney's OWN, engine-aware macro-extension SPI - distinct from Hearth's `StandardMacroExtension` (which can
  * only register `IsCollection`/`IsValueType`/... providers and has no access to the derivation engine). A
  * `ChimneyMacroExtension` registers PAIR-SPECIFIC transformation handlers (`(Type[From], Type[To]) => ...`) that:
  *
  *   - decide, by inspecting `Type[From]`/`Type[To]`, whether they special-case a given pair (the
  *     [[IsChimneySpecialCased]] extractor asks each registered handler in turn),
  *   - build the resulting `Expr` for the OUTER layer while DEFERRING inner values back to the engine via
  *     [[deriveInner]] (which re-enters the full rule pipeline recursively - supporting N inner derivations, not just
  *     the single-inner shape `TotalOuterTransformer`/`PartialOuterTransformer` allow),
  *   - have full access to the [[SpecialCaseContext]] (total vs partial, config/flags, `src` expr, fallbacks, ...), so
  *     they can reproduce total/partial asymmetry and honour `enableImplicitConflictResolution`.
  *
  * The re-exposed `SpecialCase*`/`DerivedExpr` aliases exist because the engine types they point at
  * (`TransformationContext`, `Rule.ExpansionResult`, `TransformationExpr`) are `protected`; the aliases are
  * `private[chimney]` so that separately-compiled IN-TREE integration artifacts (`chimney-protobufs`,
  * `chimney-engine-test-extension`) can name them. Promoting the SPI to fully public API is a follow-up once the shape
  * is signed off.
  *
  * CROSS-QUOTES CONTRACT: a handler's produced `Expr` and any `Expr`s recursively derived through [[deriveInner]] are
  * built lazily, INSIDE the running derivation program (the `MIO` returned by `specialCase` is only run while the
  * special-cased rule expands, i.e. inside the splicing context) - exactly like the built-in
  * `TransformImplicitOuterTransformerRule`, so the pieces compose without owner/scope violations.
  */
private[chimney] trait ChimneyEngineExtensionApi extends Contexts with rules.TransformationRules {
  this: Derivation & hearth.MacroCommons =>

  /** The derivation context for a special-cased pair (total or partial, with config/flags/src/fallbacks). Its members
    * are reached via the facade helpers [[sourceOf]]/[[isPartialContext]]/[[prefersPartialTransformer]].
    */
  final type SpecialCaseContext[From, To] = TransformationContext[From, To]

  /** A derived (possibly partial) inner expression returned by [[deriveInner]] (compose via its `fold`/`map`). */
  final type DerivedExpr[A] = TransformationExpr[A]

  /** A handler registered by an integration. Asked - via [[IsChimneySpecialCased]] - whether it special-cases a pair.
    */
  private[chimney] trait SpecialCaseHandler {

    /** Return `Some(handler)` iff this extension special-cases the `(From, To)` pair, else `None`. */
    def apply[From, To](implicit From: Type[From], To: Type[To]): Option[SpecialCasedTransformation[From, To]]
  }

  /** The pair-specific transformation matched for a concrete `(From, To)`. `specialCase` yields `Some(derivedExpr)`
    * when it produces a transformation, or `None` to DECLINE after matching by type (e.g. no total path in a total
    * context) so derivation continues with the next rule.
    */
  private[chimney] trait SpecialCasedTransformation[From, To] {

    /** Build the transformation for the (already matched) `(From, To)` pair, deferring inner values via
      * [[deriveInner]].
      */
    def specialCase(implicit ctx: SpecialCaseContext[From, To]): MIO[Option[DerivedExpr[To]]]
  }

  private val specialCaseHandlers = scala.collection.mutable.ListBuffer.empty[SpecialCaseHandler]

  /** Register a handler (call from `ChimneyMacroExtension.extend`). First registered, first asked. */
  private[chimney] def registerSpecialCase(handler: SpecialCaseHandler): Unit = {
    specialCaseHandlers += handler
    ()
  }

  /** Owner's sketch:
    * {{{
    * (Type[From], Type[To]) match {
    *   case IsChimneySpecialCased(handler) => handler.specialCase(using ctx) // rule matched
    *   case _                              => // rule yielded
    * }
    * }}}
    */
  private[chimney] object IsChimneySpecialCased {

    def unapply[From, To](types: (Type[From], Type[To])): Option[SpecialCasedTransformation[From, To]] = {
      implicit val from: Type[From] = types._1
      implicit val to: Type[To] = types._2
      specialCaseHandlers.iterator.flatMap(_.apply[From, To]).nextOption()
    }
  }

  // Result builders re-exposed for handler authors (they never touch `Rule.ExpansionResult`/`TransformationExpr.*`).

  /** Yield a total transformation `Expr[To]`. */
  private[chimney] def specialCasedTotal[To](expr: Expr[To]): MIO[Option[DerivedExpr[To]]] =
    MIO.pure(Some(TransformationExpr.fromTotal(expr)))

  /** Yield a partial transformation `Expr[partial.Result[To]]`. */
  private[chimney] def specialCasedPartial[To](expr: Expr[partial.Result[To]]): MIO[Option[DerivedExpr[To]]] =
    MIO.pure(Some(TransformationExpr.fromPartial(expr)))

  /** Yield an already-built (total or partial) [[DerivedExpr]] (e.g. one composed out of [[deriveInner]] results). */
  private[chimney] def specialCasedExpr[To](expr: DerivedExpr[To]): MIO[Option[DerivedExpr[To]]] = MIO.pure(Some(expr))

  /** Decline this pair AFTER matching by type (e.g. no total path in a total context) - derivation continues. */
  private[chimney] def specialCaseYield[To]: MIO[Option[DerivedExpr[To]]] = MIO.pure(None)

  /** Defer an inner value to the full rule pipeline recursively (the mechanism the OUTER handler builds around). */
  private[chimney] def deriveInner[InnerFrom: Type, InnerTo: Type](
      inner: Expr[InnerFrom]
  )(implicit ctx: SpecialCaseContext[?, ?]): MIO[DerivedExpr[InnerTo]] =
    deriveRecursiveTransformationExpr[InnerFrom, InnerTo](inner)

  /** Build a special-cased OUTER transformation that maps every inner element THROUGH the engine - the extension analog
    * of a `TotalOuterTransformer`, for when the outer container carries a homogeneous `InnerFrom` to map into `InnerTo`
    * (e.g. `F[A] -> F[B]` / `F[A] -> G[B]`).
    *
    * It recursively derives `InnerFrom => InnerTo` for the elements (re-entering the full rule pipeline under the
    * `everyItem` path, so nested transformers/overrides apply), folds whether that inner derivation came back total or
    * partial, and lets the handler wrap the built inner FUNCTION with its own outer construction:
    *   - [[onTotalInner]] receives `Expr[InnerFrom => InnerTo]` and returns the total `Expr[To]` (e.g.
    *     `Traverse[F].map(src)(fn)`),
    *   - [[onPartialInner]] receives `Expr[InnerFrom => partial.Result[InnerTo]]` and the fail-fast flag and returns
    *     `Expr[partial.Result[To]]` (used when an element's derivation is partial; requires a partial context - a
    *     partial inner in a total context is a derivation bug, surfaced as an assertion).
    *
    * Mirrors the built-in `TransformImplicitOuterTransformerRule`, so the produced pieces compose under the
    * cross-quotes contract exactly like a summoned `TotalOuterTransformer` would.
    */
  private[chimney] def specialCasedOuterMap[From, To, InnerFrom: Type, InnerTo: Type](
      onTotalInner: Expr[InnerFrom => InnerTo] => Expr[To]
  )(
      onPartialInner: (Expr[InnerFrom => partial.Result[InnerTo]], Expr[Boolean]) => Expr[partial.Result[To]]
  )(implicit ctx: SpecialCaseContext[From, To]): MIO[Option[DerivedExpr[To]]] = {
    import ChimneyType.Implicits.* // provides the `Type[partial.Result[InnerTo]]` needed by `build` below
    LambdaBuilder
      .of1[InnerFrom]()
      .traverse { (innerFromExpr: Expr[InnerFrom]) =>
        deriveRecursiveTransformationExpr[InnerFrom, InnerTo](innerFromExpr, Path(_.everyItem), Path(_.everyItem))
      }
      .flatMap { (builder: LambdaBuilder[InnerFrom => *, TransformationExpr[InnerTo]]) =>
        builder.foldTransformationExpr { (onTotal: LambdaBuilder[InnerFrom => *, Expr[InnerTo]]) =>
          specialCasedTotal(onTotalInner(onTotal.build[InnerTo]))
        } { (onPartial: LambdaBuilder[InnerFrom => *, Expr[partial.Result[InnerTo]]]) =>
          ctx.fold { _ =>
            MIO.fail[Option[DerivedExpr[To]]](
              new AssertionError("Special-cased outer transformer derived a partial inner in a total context")
            )
          } { partialCtx =>
            specialCasedPartial(onPartialInner(onPartial.build[partial.Result[InnerTo]], partialCtx.failFast))
          }
        }
      }
  }

  // Context accessors (the context's own members stay `protected`; these facade helpers reach them from inside the cake).

  /** The source expression being transformed. */
  private[chimney] def sourceOf[From, To](implicit ctx: SpecialCaseContext[From, To]): Expr[From] = ctx.src

  /** `true` in a partial (`PartialTransformer`/`transformIntoPartial`) context, `false` in a total one. */
  private[chimney] def isPartialContext(implicit ctx: SpecialCaseContext[?, ?]): Boolean =
    ctx.fold(_ => false)(_ => true)

  /** `true` iff the user set `enableImplicitConflictResolution(PreferPartialTransformer)`. */
  private[chimney] def prefersPartialTransformer(implicit ctx: SpecialCaseContext[?, ?]): Boolean =
    ctx.config.flags.implicitConflictResolution.contains(PreferPartialTransformer)

  // Load handlers once per macro-bundle instance (mirrors `ensureStandardExtensionsLoaded`).
  private var chimneyMacroExtensionsLoaded = false

  /** Call before consulting [[IsChimneySpecialCased]]. Idempotent. Loads `ChimneyMacroExtension`s via `ServiceLoader`.
    */
  protected def ensureChimneyMacroExtensionsLoaded(): Unit =
    if (!chimneyMacroExtensionsLoaded) {
      Environment.loadMacroExtensions[io.scalaland.chimney.integrations.ChimneyMacroExtension].toEither match {
        case Right(_)     => chimneyMacroExtensionsLoaded = true
        case Left(errors) =>
          assertionFailed(
            ("Failed to load Chimney macro extensions:" +: errors.toVector.map(error => s"  ${error.getMessage}"))
              .mkString("\n")
          )
      }
    }
}
