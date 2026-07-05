package io.scalaland.chimney.internal.compiletime

/** Hearth workarounds and small helpers used by the derivation engine; each workaround member cites its upstream issue
  * (https://github.com/kubuszok/hearth/issues). After the 0.4.0-20-gfd010da-SNAPSHOT sweep both the #334
  * annotation-attaching gap and the #307/#344 upper-bounded `Type.CtorN` gap are GONE:
  * `nowarnExpr`/`suppressWarningsExpr` attach annotations cross-platform via the type-based `Expr.annotated[A, Ann]`
  * (no per-platform Java-annotation INSTANCE needed), and `ChimneyTypes`/`DslMacros` call Hearth's direct
  * `Type.CtorN.UpperBounded.of` in shared code (Scala 3's cross-quotes plugin now rewrites the three-select form).
  * Earlier the 0.4.0-16-gd4adc1c-SNAPSHOT sweep (it.30) and the cross-quotes usage-contract refactor (it.31) removed
  * the #317/#318 shims (`prependFreshValCompat`, `withMacroEntryCtxCompat`, the Scala 3 derive-first `*InstanceCompat`
  * overrides): chimney honors the cross-quotes usage contract ("an expr that is spliced has to be created inside the
  * expr that is splicing it") - derivations run inside the splice that consumes them (see `ChimneyExprs`) and caches
  * never hand out `Expr`s across splices (see [[TypeCache]]).
  */
private[compiletime] trait MacroCommonsCompat { this: hearth.MacroCommons =>

  /** Alias for Hearth's `??`. */
  final type ExistentialType = ??

  /** Alias for Hearth's `Expr_??`. */
  final type ExistentialExpr = Expr_??

  /** Emits an actual `.asInstanceOf[B]` cast in the generated code (unlike Hearth's `Expr.upcast[A, B]`, which is a
    * compile-time-verified widening with no runtime cast).
    *
    * Kept as a proper method with its own type parameters (helper-method pattern) so that the Scala 2 cross-quotes
    * expansion resolves `A`/`B` through `WeakTypeTag`s instead of path-dependent types.
    */
  protected def castToExpr[A: Type, B: Type](expr: Expr[A]): Expr[B] =
    Expr.quote {
      Expr.splice(expr).asInstanceOf[B]
    }

  /** Scala 2 workaround for Java Enums in DSL-encoded `runtime.Path` types.
    *
    * On Scala 2 the whitebox DSL macros cannot embed the Java-enum-value singleton type (e.g. `Color.Black.type`) into
    * the refined `TransformerOverrides` type, so they encode it as
    * `io.scalaland.chimney.internal.runtime.RefinedJavaEnum[Color, "Black"]`. When `Configurations.extractPath` parses
    * a `Path.Matching`/`Path.SourceMatching` element it must decode that marker back into the enum instance's real
    * type, otherwise subtype-override matching silently fails.
    *
    * Default is identity (Scala 3 DSL embeds real singleton types); the Scala 2 `PlatformBridge` overrides it.
    */
  protected def fixJavaEnumCompat(inst: ??): ?? = inst

  /** `.asInstanceOfExpr[B]` syntax over [[castToExpr]]. */
  implicit final protected class CompatExprOps[A](private val expr: Expr[A]) {

    def asInstanceOfExpr[B](implicit A: Type[A], B: Type[B]): Expr[B] = castToExpr[A, B](expr)
  }

  /** `.asInstanceOfExpr[B]`/`.upcastToExprOf[B]` syntax on `Expr_??`. */
  implicit final protected class CompatExistentialExprOps(private val expr: Expr_??) {

    def asInstanceOfExpr[B: Type]: Expr[B] = {
      import expr.{Underlying as A, value as valueExpr}
      castToExpr[A, B](valueExpr)
    }

    def upcastToExprOf[B: Type]: Expr[B] = {
      import expr.{Underlying as A, value as valueExpr}
      valueExpr.upcast[B]
    }
  }

  /** Alias for `Environment.reportErrorAndAbort`. */
  protected def reportError(errors: String): Nothing = Environment.reportErrorAndAbort(errors)

  /** Alias for `Environment.XMacroSettings`. */
  protected def XMacroSettings: List[String] = Environment.XMacroSettings

  /** Extracts the value of a literal `String` singleton type (asserts on non-literal types). */
  implicit final protected class CompatTypeStringOps[S <: String](private val S: Type[S]) {

    def extractStringSingleton: String =
      Type.StringCodec.fromType(S).map(_.value).getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        assertionFailed(s"Invalid string literal type: ${Type.prettyPrint(using S)}")
        // $COVERAGE-ON$
      }
  }

  /** Extracts an `object`'s singleton instance from its type.
    *
    * Hearth's `Type.ModuleCodec` type parameter is bounded by `Singleton`, which chimney's call sites (`M <:
    * TransformedNamesComparison`) do not satisfy - hence the cast through `Nothing` (the codec's implementation is a
    * single erased object).
    */
  protected def extractObjectSingletonOf[M: Type]: Option[M] =
    Type.ModuleCodec[Nothing].asInstanceOf[TypeCodec[M]].fromType(Type[M]).map(_.value)

  /** `Option`-returning implicit summoning.
    *
    * TODO(hearth-migration): consider switching call sites to Hearth's `Expr.summonImplicitIgnoring(...)` to replace
    * Chimney's manual self-recursion exclusion logic.
    */
  protected def summonImplicitOptionOf[A: Type]: Option[Expr[A]] = Expr.summonImplicit[A].toOption

  /** Implicit summoning that fails the expansion when nothing is found. */
  protected def summonImplicitUnsafeOf[A: Type]: Expr[A] = Expr.summonImplicit[A].get

  /** Attaches `@nowarn`/`@nowarn(msg)` to the generated expr (the `-Xmacro-settings:chimney.nowarn=...` user feature,
    * used by `GatewayCommons.suppressWarnings`).
    *
    * Hearth's built-in unused-suppression (`Expr.suppressUnused`) covers unused-value warnings; these user-configurable
    * `@nowarn`/`@SuppressWarnings` wrappers are attached cross-platform via `Expr.annotated` (hearth#334), which binds
    * the expr to a fresh `@annotation val`.
    */
  protected def nowarnExpr[A: Type](warnings: Option[String])(expr: Expr[A]): Expr[A] = {
    implicit val nowarnType: Type[scala.annotation.nowarn] = Type.of[scala.annotation.nowarn]
    warnings match {
      case Some(msg) => expr.annotated[scala.annotation.nowarn](Expr(msg).asUntyped)
      case None      => expr.annotated[scala.annotation.nowarn]()
    }
  }

  /** Attaches `@SuppressWarnings(Array(...))` to the generated expr (on by default for linters like WartRemover,
    * configurable with `-Xmacro-settings:chimney.SuppressWarnings=...`) - see [[nowarnExpr]].
    */
  protected def suppressWarningsExpr[A: Type](warnings: List[String])(expr: Expr[A]): Expr[A] = {
    implicit val suppressWarningsType: Type[java.lang.SuppressWarnings] = Type.of[java.lang.SuppressWarnings]
    implicit val stringType: Type[String] = Type.of[String] // for ExprCodec[Array[String]]
    expr.annotated[java.lang.SuppressWarnings](Expr(warnings.toArray).asUntyped)
  }

  /** Cross-quotes-limitation workaround: on Scala 2 cross-quotes `Type.of[F[A, ?]]` fails to compile whenever the
    * enclosing method has type parameters ("not found: type ?$1" - the generated workaround method loses the wildcard;
    * a documented best-effort-WeakTypeTag limitation, not a bug). `Type.of[F[Any, ?]]` in a member without type
    * parameters expands fine, so `ChimneyType.*.inferred` captures such an example once and then replaces the leading
    * type arguments with the actual ones.
    *
    * Since hearth#312 (fixed in 0.4.1) `UntypedType.applyTypeArgs` applies to the type CONSTRUCTOR on BOTH platforms,
    * so the shared implementation is a plain re-application. On Scala 2 the result would still leave the wildcard
    * example's existentially-quantified symbols unbound, so [[PlatformBridge]] (Scala 2) overrides it with a version
    * that re-quantifies via `internal.existentialAbstraction` (a property of Chimney's wildcard-example mechanism, not
    * of Hearth's `applyTypeArgs`).
    */
  protected def reapplyLeadingTypeArgsCompat(wildcardExample: UntypedType, leading: List[UntypedType]): UntypedType = {
    val dealiased = UntypedType.dealias(wildcardExample)
    val existingArgs = UntypedType.typeArguments(dealiased)
    UntypedType.applyTypeArgs(dealiased, leading ++ existingArgs.drop(leading.size))
  }

  // NOTE: there are deliberately NO ambient implicit `Type`s (Int/String/Option/...) - inside `Expr.quote` the
  // cross-quotes plugin summons `Type`s automatically; plain (non-quoted) shared code creates local `implicit val`s
  // with inline `Type.of[...]` (or a helper def with its own `[X: Type]` parameters when existential-imported types
  // are involved).

  /** Identity of the Cross-Quotes scope the calling code is currently evaluated under.
    *
    * Scala 3 `PlatformBridge` overrides this with the ACTIVE `Quotes` (each `Expr.splice` evaluates its thunks under a
    * fresh nested `Quotes`); on Scala 2 there is no expr scoping, so the default is a single constant token per cake
    * instance. Used by [[TypeCache]] to keep cached values scope-local.
    */
  protected def cacheScopeToken: AnyRef = this

  /** Caches a computed `F[A]` per `Type[A]` (keys compared with `=:=`) WITHIN a single Cross-Quotes scope.
    *
    * CROSS-QUOTES USAGE CONTRACT: an expr that is spliced has to be created inside the expr that is splicing it - so a
    * cache accessed from inside an `Expr.splice` must NOT hand out values materialized during a DIFFERENT splice
    * evaluation (deriving a second instance in one expansion - Iso/Codec - would then use the first splice's `Expr`s
    * and `-Xcheck-macros` aborts with a ScopeException). Cached values here routinely embed materialized `Expr`s
    * (summoned integration implicits, provider views, default-value exprs), so entries are partitioned by
    * [[cacheScopeToken]]: within one scope the memoization is as effective as before, a new scope recomputes (fresh
    * summons/exprs) instead of leaking foreign-scope trees. On Scala 2 the token is constant and this behaves like a
    * plain per-expansion cache.
    */
  final protected class TypeCache[F[_]] {
    sealed private trait Entry {
      type Underlying
      val key: Type[Underlying]
      val value: F[Underlying]
    }
    private object Entry {
      def apply[A](key: Type[A], value: F[A]): Entry { type Underlying = A } = new Impl(key, value)
      final class Impl[A](val key: Type[A], val value: F[A]) extends Entry { type Underlying = A }
    }
    private val storage =
      scala.collection.mutable.Map.empty[AnyRef, scala.collection.mutable.ListBuffer[Entry]]

    def apply[A](key: Type[A])(newValue: => F[A]): F[A] = {
      val entries = storage.getOrElseUpdate(cacheScopeToken, scala.collection.mutable.ListBuffer.empty[Entry])
      entries.find(_.key =:= key) match {
        case Some(found) => found.value.asInstanceOf[F[A]]
        case None        =>
          val value = newValue
          entries += Entry(key, value)
          value
      }
    }
  }
}
