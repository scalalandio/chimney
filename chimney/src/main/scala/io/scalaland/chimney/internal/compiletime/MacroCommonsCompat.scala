package io.scalaland.chimney.internal.compiletime

/** Hearth workarounds and small helpers used by the derivation engine; each workaround member cites its upstream issue
  * (https://github.com/kubuszok/hearth/issues). After the 0.4.0-16-gd4adc1c-SNAPSHOT sweep (it.30) and the cross-quotes
  * usage-contract refactor (it.31) the surviving workarounds are: #307 (ctorN factories - the fixed codegen still fails
  * to MATCH existentially-quantified type projections on Scala 2) and the #334 annotation-attaching gap
  * (nowarn/suppressWarnings). The former #317/#318 shims (`prependFreshValCompat`, `withMacroEntryCtxCompat`, the Scala
  * 3 derive-first `*InstanceCompat` overrides) are GONE: chimney now honors the cross-quotes usage contract ("an expr
  * that is spliced has to be created inside the expr that is splicing it") - derivations run inside the splice that
  * consumes them (see `ChimneyExprs`) and caches never hand out `Expr`s across splices (see [[TypeCache]]).
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
    * Hearth's built-in unused-suppression (`Expr.suppressUnused`) covers unused-value warnings, but it has no
    * annotation-attaching API for these user-configurable `@nowarn`/`@SuppressWarnings` wrappers - they are implemented
    * per-platform in the `PlatformBridge`s (Scala 2: quasiquote; Scala 3: `AnnotatedType` `ValDef`).
    */
  protected def nowarnExpr[A: Type](warnings: Option[String])(expr: Expr[A]): Expr[A]

  /** Attaches `@SuppressWarnings(Array(...))` to the generated expr (on by default for linters like WartRemover,
    * configurable with `-Xmacro-settings:chimney.SuppressWarnings=...`) - see [[nowarnExpr]].
    */
  protected def suppressWarningsExpr[A: Type](warnings: List[String])(expr: Expr[A]): Expr[A]

  /** Workaround for a Hearth bug (hearth#307 leftover): on Scala 2 the 0.4.0 `Type.CtorN.UpperBounded.of[...]` (and
    * `Bounded.of`) with a non-`Any` upper bound expanded to code that did not typecheck; 0.4.1 fixed THAT, and
    * 0.4.0-16-gd4adc1c also fixed the follow-up literal-args decay (`.dealias.widen` in `matchResult`) - but the
    * generated `unapply` still fails to MATCH existentially-quantified type projections, e.g.
    * `TransformerFlags#OptionFallbackMerge[?$3]` coming from DSL members declared with wildcard args
    * (`Disable[OptionFallbackMerge[?], Flags]` in `TransformerTargetFlagsDsl`): Scala 2 configuration parsing then
    * aborts with "Invalid internal TransformerFlag type: ...OptionFallbackMerge[...`?$3`]!" (verified against
    * 0.4.0-16-gd4adc1c-SNAPSHOT; see the follow-up comment on hearth#307).
    *
    * These factories hand-build the same `Type.CtorN.UpperBounded` instances on top of Hearth's untyped API instead
    * (extracted args passed through UNWIDENED). `applied` is the type constructor applied to its upper bounds - it only
    * serves as a way to obtain the untyped type constructor in shared code.
    *
    * Semantics difference vs the cross-quotes-generated instances: `unapply` matches on the exact (dealiased) type
    * constructor, without `baseType` subtype-awareness - which is enough for Chimney's phantom-type configs.
    */
  protected def ctor1UpperBoundedCompat[U1, HKT[_ <: U1]](applied: Type[HKT[U1]]): Type.Ctor1.UpperBounded[U1, HKT] =
    new Type.Ctor1.Bounded[Nothing, U1, HKT] {
      private val untypedCtor: UntypedType = UntypedType.typeConstructor(applied.asUntyped)

      def apply[A <: U1: Type]: Type[HKT[A]] =
        UntypedType.applyTypeArgs(untypedCtor, List(Type[A].asUntyped)).asTyped[HKT[A]]

      def unapply[In](In: Type[In]): Option[Nothing <:??<: U1] = {
        val dealiased = UntypedType.dealias(In.asUntyped)
        if (UntypedType.sameTypeConstructorAs(untypedCtor, dealiased))
          UntypedType.typeArguments(dealiased) match {
            case a1 :: Nil => Some(a1.asTyped[U1].as_??<:[U1])
            case _         => None
          }
        else None
      }

      override def asUntyped: UntypedType = untypedCtor
    }

  /** See [[ctor1UpperBoundedCompat]]. */
  protected def ctor2UpperBoundedCompat[U1, U2, HKT[_ <: U1, _ <: U2]](
      applied: Type[HKT[U1, U2]]
  ): Type.Ctor2.UpperBounded[U1, U2, HKT] =
    new Type.Ctor2.Bounded[Nothing, U1, Nothing, U2, HKT] {
      private val untypedCtor: UntypedType = UntypedType.typeConstructor(applied.asUntyped)

      def apply[A <: U1: Type, B <: U2: Type]: Type[HKT[A, B]] =
        UntypedType.applyTypeArgs(untypedCtor, List(Type[A].asUntyped, Type[B].asUntyped)).asTyped[HKT[A, B]]

      def unapply[In](In: Type[In]): Option[(Nothing <:??<: U1, Nothing <:??<: U2)] = {
        val dealiased = UntypedType.dealias(In.asUntyped)
        if (UntypedType.sameTypeConstructorAs(untypedCtor, dealiased))
          UntypedType.typeArguments(dealiased) match {
            case a1 :: a2 :: Nil => Some((a1.asTyped[U1].as_??<:[U1], a2.asTyped[U2].as_??<:[U2]))
            case _               => None
          }
        else None
      }

      override def asUntyped: UntypedType = untypedCtor
    }

  /** See [[ctor1UpperBoundedCompat]]. */
  protected def ctor3UpperBoundedCompat[U1, U2, U3, HKT[_ <: U1, _ <: U2, _ <: U3]](
      applied: Type[HKT[U1, U2, U3]]
  ): Type.Ctor3.UpperBounded[U1, U2, U3, HKT] =
    new Type.Ctor3.Bounded[Nothing, U1, Nothing, U2, Nothing, U3, HKT] {
      private val untypedCtor: UntypedType = UntypedType.typeConstructor(applied.asUntyped)

      def apply[A <: U1: Type, B <: U2: Type, C <: U3: Type]: Type[HKT[A, B, C]] =
        UntypedType
          .applyTypeArgs(untypedCtor, List(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped))
          .asTyped[HKT[A, B, C]]

      def unapply[In](In: Type[In]): Option[(Nothing <:??<: U1, Nothing <:??<: U2, Nothing <:??<: U3)] = {
        val dealiased = UntypedType.dealias(In.asUntyped)
        if (UntypedType.sameTypeConstructorAs(untypedCtor, dealiased))
          UntypedType.typeArguments(dealiased) match {
            case a1 :: a2 :: a3 :: Nil =>
              Some((a1.asTyped[U1].as_??<:[U1], a2.asTyped[U2].as_??<:[U2], a3.asTyped[U3].as_??<:[U3]))
            case _ => None
          }
        else None
      }

      override def asUntyped: UntypedType = untypedCtor
    }

  /** See [[ctor1UpperBoundedCompat]]. */
  protected def ctor4UpperBoundedCompat[U1, U2, U3, U4, HKT[_ <: U1, _ <: U2, _ <: U3, _ <: U4]](
      applied: Type[HKT[U1, U2, U3, U4]]
  ): Type.Ctor4.UpperBounded[U1, U2, U3, U4, HKT] =
    new Type.Ctor4.Bounded[Nothing, U1, Nothing, U2, Nothing, U3, Nothing, U4, HKT] {
      private val untypedCtor: UntypedType = UntypedType.typeConstructor(applied.asUntyped)

      def apply[A <: U1: Type, B <: U2: Type, C <: U3: Type, D <: U4: Type]: Type[HKT[A, B, C, D]] =
        UntypedType
          .applyTypeArgs(untypedCtor, List(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped))
          .asTyped[HKT[A, B, C, D]]

      def unapply[In](
          In: Type[In]
      ): Option[(Nothing <:??<: U1, Nothing <:??<: U2, Nothing <:??<: U3, Nothing <:??<: U4)] = {
        val dealiased = UntypedType.dealias(In.asUntyped)
        if (UntypedType.sameTypeConstructorAs(untypedCtor, dealiased))
          UntypedType.typeArguments(dealiased) match {
            case a1 :: a2 :: a3 :: a4 :: Nil =>
              Some(
                (
                  a1.asTyped[U1].as_??<:[U1],
                  a2.asTyped[U2].as_??<:[U2],
                  a3.asTyped[U3].as_??<:[U3],
                  a4.asTyped[U4].as_??<:[U4]
                )
              )
            case _ => None
          }
        else None
      }

      override def asUntyped: UntypedType = untypedCtor
    }

  /** See [[ctor1UpperBoundedCompat]]. */
  protected def ctor5UpperBoundedCompat[U1, U2, U3, U4, U5, HKT[_ <: U1, _ <: U2, _ <: U3, _ <: U4, _ <: U5]](
      applied: Type[HKT[U1, U2, U3, U4, U5]]
  ): Type.Ctor5.UpperBounded[U1, U2, U3, U4, U5, HKT] =
    new Type.Ctor5.Bounded[Nothing, U1, Nothing, U2, Nothing, U3, Nothing, U4, Nothing, U5, HKT] {
      private val untypedCtor: UntypedType = UntypedType.typeConstructor(applied.asUntyped)

      def apply[A <: U1: Type, B <: U2: Type, C <: U3: Type, D <: U4: Type, E <: U5: Type]: Type[HKT[A, B, C, D, E]] =
        UntypedType
          .applyTypeArgs(
            untypedCtor,
            List(Type[A].asUntyped, Type[B].asUntyped, Type[C].asUntyped, Type[D].asUntyped, Type[E].asUntyped)
          )
          .asTyped[HKT[A, B, C, D, E]]

      def unapply[In](
          In: Type[In]
      ): Option[(Nothing <:??<: U1, Nothing <:??<: U2, Nothing <:??<: U3, Nothing <:??<: U4, Nothing <:??<: U5)] = {
        val dealiased = UntypedType.dealias(In.asUntyped)
        if (UntypedType.sameTypeConstructorAs(untypedCtor, dealiased))
          UntypedType.typeArguments(dealiased) match {
            case a1 :: a2 :: a3 :: a4 :: a5 :: Nil =>
              Some(
                (
                  a1.asTyped[U1].as_??<:[U1],
                  a2.asTyped[U2].as_??<:[U2],
                  a3.asTyped[U3].as_??<:[U3],
                  a4.asTyped[U4].as_??<:[U4],
                  a5.asTyped[U5].as_??<:[U5]
                )
              )
            case _ => None
          }
        else None
      }

      override def asUntyped: UntypedType = untypedCtor
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
