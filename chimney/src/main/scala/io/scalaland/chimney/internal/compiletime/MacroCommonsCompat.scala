package io.scalaland.chimney.internal.compiletime

/** Hearth 0.4.0 workarounds and small helpers used by the derivation engine; each workaround member cites its upstream
  * issue (https://github.com/kubuszok/hearth/issues).
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

  /** `true` for accessor methods that scalac marks STABLE (`MethodSymbol.isStable`) even though Hearth's `Method.isVal`
    * is `false` - most notably `val` members of structural REFINEMENT types (e.g. a type param bounded with
    * `A <: { val value: String }`), whose member symbol is a deferred stable method with no accessed field.
    *
    * HEARTH 0.4.0 ISSUE WORKAROUND (hearth#326): such getters must classify as `ConstructorBodyVal` (an
    * always-available accessor, no `.enableMethodAccessors` needed), but Hearth's `isVal` (`symbol.isVal ||
    * accessed-field-isVal || isLazy`) misses them, silently demoting them to flag-gated `AccessorMethod`s (regression
    * vs 1.x, caught by the docs "Parametric types" snippet). The Scala 2 `PlatformBridge` overrides this with the
    * `isStable` formula; the Scala 3 default `false` is 1.x parity (on Scala 3 refinement members were never listed as
    * body vals - verified against chimney 1.8.2: the same shape fails with "no accessor named value" there too).
    */
  protected def isStableAccessorCompat(method: Method): Boolean = false

  /** Runs the thunk with Cross-Quotes' active context restored to the MACRO-ENTRY one (`Quotes` on Scala 3).
    *
    * HEARTH 0.4.0 ISSUE WORKAROUND (hearth#317, Scala 3): whole derivations that run INSIDE an `Expr.splice` (the
    * `Transformer`/`PartialTransformer`/`Patcher` `instance` builders) execute under Cross-Quotes' `nestedCtx`.
    * Everything they create through cross-quoted helpers (types, exprs, cached `SealedEnum`/`Product` views,
    * trait-level lazy vals initialized on first touch) then BELONGS to that one splice evaluation - deriving a SECOND
    * instance in the same expansion (Iso/Codec do exactly that) re-evaluates the same splice and `-Xcheck-macros`
    * aborts with "Type created in a splice, extruded from that splice and then used in a subsequent evaluation of that
    * same splice" / "Expression created in a splice was used outside of that splice". Restoring the entry `Quotes` for
    * the derivation makes all definitions/types/caches entry-scoped and legal in every splice.
    *
    * IMPORTANT: quote any splice-scoped parameters (e.g. `Expr.quote(src)`) BEFORE entering this wrapper. Identity on
    * Scala 2 (no `Quotes` scoping).
    */
  protected def withMacroEntryCtxCompat[T](thunk: => T): T = thunk

  /** Prepends a `FreshName.FromType`-named val in front of `use`'s result - a `ValDefs.createVal(...).use(...)` that is
    * SAFE to call inside an `Expr.splice` on Scala 3.
    *
    * HEARTH 0.4.0 ISSUE WORKAROUND (hearth#317, Scala 3): Hearth's `ValDefs` is bound to the macro-entry `Quotes`, so
    * inside an `Expr.splice` (where Cross-Quotes' `nestedCtx` switched the active `Quotes`) the created `ValDef` is
    * owned by the ENTRY splice owner, while trees that pass through cross-quoted helpers get re-owned to the nested
    * quote's owner (e.g. `method transform`); `ValDefs.closeScope`'s `Block.apply` then trips `-Xcheck-macros` with
    * "Block contains definition with different owners". The Scala 3 `PlatformBridge` override builds the val under
    * `CrossQuotes.ctx` (correct owner) and heals the body with `changeOwner`. The shared default (fine on Scala 2,
    * where there is no owner tracking) delegates to `ValDefs`.
    */
  protected def prependFreshValCompat[A: Type, B: Type](value: Expr[A])(use: Expr[A] => Expr[B]): Expr[B] =
    ValDefs.createVal[A](value, FreshName.FromType).use(use)

  /** Re-attaches a precise `Type[A]` to an expression whose statically-carried type information is unreliable.
    *
    * HEARTH 0.4.0 ISSUE WORKAROUND (hearth#308, Scala 2): `ValDefs.closeScope[A]` (which backs `.use`/`.close`) has no
    * `Type`/`WeakTypeTag` bound, so the returned `c.Expr[A](block)` gets a compiler-materialized, UNRESOLVED
    * `WeakTypeTag[A]` (its `tpe` is literally the abstract type param `A`). The block tree is untyped, so `Expr.typeOf`
    * falls back to that junk tag, and everything derived from it (e.g. `TransformationExpr`'s implicit `Type[A]`,
    * `partial.Result.Value.apply[A]` type arguments) renders an unresolvable `A` in trees - "type mismatch; found:
    * x$macro$N.type; required: A" or `scala.MatchError: WeakTypeTag[A]`.
    *
    * Call this on every `.use` result that can later flow into `Expr.typeOf` (via `TransformationExpr`). The shared
    * default is identity (on Scala 3 quoted exprs always carry their real type); the Scala 2 `PlatformBridge` overrides
    * it to `c.Expr[A](expr.tree)(Type[A])`.
    */
  protected def retagExprCompat[A: Type](expr: Expr[A]): Expr[A] = expr

  /** Builds a `classOf[A]` class-literal expression for a CONCRETE class type `A` (hearth#321).
    *
    * Both `PlatformBridge`s override this with the platform's class-literal tree (`Literal(Constant(tpe))` on Scala 2,
    * `Literal(ClassOfConstant(tpe))` on Scala 3). It exists because Hearth 0.4.0's `Expr.ClassExprCodec` on Scala 2
    * lifts the token through a quasiquote whose type splice does not survive Chimney's re-typecheck (the
    * `java.util.EnumSet`/`EnumMap` provider bug - see [[JavaCollectionsPlatformCompat]]), while a class LITERAL of a
    * concrete type prints as plain `classOf[fqcn.Type]` and re-typechecks fine.
    *
    * The shared default should never be called - every concrete cake goes through a `PlatformBridge`.
    */
  protected def classOfExprCompat[A: Type]: Expr[java.lang.Class[A]] =
    // $COVERAGE-OFF$should never happen unless a partial cake without PlatformBridge calls it
    assertionFailed(s"classOfExprCompat is platform-specific, got no override for ${Type.prettyPrint[A]}")
  // $COVERAGE-ON$

  /** Safe replacement for `list.traverse[ValDefs, B](f)`.
    *
    * HEARTH 0.4.0 BUG WORKAROUND: `Applicative[ValDefs].map2(fa, fb)(f)` reads its BY-NAME `fb` twice (`fb.definitions`
    * and `fb.value`), so `List.traverse[ValDefs, _]` re-runs every per-element computation:
    * `ValDefs.createVal/createLazy/...` mint a fresh name on each evaluation, and the tree ends up with the DEFINITION
    * carrying name `x$macro$N` (first evaluation) while the REFERENCE carries `x$macro$N+1` (second evaluation) -
    * generated code then fails to typecheck with "not found: value x$macro$N+1".
    *
    * This helper evaluates each per-element `ValDefs` exactly once (strict `map`), then folds with `map2` over
    * already-constructed values, for which the double read of the by-name argument is harmless.
    *
    * Fixed on hearth master - TODO(hearth-migration): remove once the fix ships.
    */
  protected def traverseValDefsCompat[A, B](list: List[A])(f: A => ValDefs[B]): ValDefs[List[B]] = {
    val cells: List[ValDefs[B]] = list.map(f)
    cells.foldRight(ValDefsTraverse.pure(List.empty[B])) { (cell, acc) =>
      ValDefsTraverse.map2(cell, acc)(_ :: _)
    }
  }

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

  /** Workaround for a Hearth 0.4.0 bug (hearth#307): on Scala 2 `Type.CtorN.UpperBounded.of[...]` (and `Bounded.of`)
    * with a non-`Any` upper bound expands to code that does not typecheck (the generated `matchResult` casts the
    * extracted argument to `Type[scala.Any]` and then calls `.as_<:??<:[L, U]`, which requires `U >: Any`).
    *
    * These factories hand-build the same `Type.CtorN.UpperBounded` instances on top of Hearth's untyped API instead.
    * `applied` is the type constructor applied to its upper bounds - it only serves as a way to obtain the untyped type
    * constructor in shared code.
    *
    * Semantics difference vs the cross-quotes-generated instances: `unapply` matches on the exact (dealiased) type
    * constructor, without `baseType` subtype-awareness - which is enough for Chimney's phantom-type configs.
    *
    * TODO(hearth-migration): remove once fixed upstream.
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

  /** Workaround for a Hearth 0.4.0 bug (hearth#312): on Scala 2 cross-quotes `Type.of[F[A, ?]]` fails to compile
    * whenever the enclosing method has type parameters ("not found: type ?$1" - the generated workaround method loses
    * the wildcard). `Type.of[F[Any, ?]]` in a member without type parameters expands fine, so `ChimneyType.*.inferred`
    * captures such an example once and then replaces the leading type arguments with the actual ones.
    *
    * The shared implementation applies the args to the TYPE CONSTRUCTOR (`UntypedType.typeConstructor`) - Hearth
    * 0.4.0's `applyTypeArgs` takes the constructor itself on Scala 2 (`appliedType(_.typeConstructor, args)`) but
    * `appliedTo`s the given type AS-IS on Scala 3, so applying to the already-applied wildcard example would silently
    * produce a malformed type there (and every implicit search against it - e.g. the
    * `TotalOuterTransformer`/`PartialOuterTransformer` integration summons - would just fail). On Scala 2 the result
    * would additionally leave the existential's quantified symbols unbound, so [[PlatformBridge]] (Scala 2) overrides
    * it with a version that re-quantifies via `internal.existentialAbstraction`.
    *
    * TODO(hearth-migration): remove once fixed upstream.
    */
  protected def reapplyLeadingTypeArgsCompat(wildcardExample: UntypedType, leading: List[UntypedType]): UntypedType = {
    val dealiased = UntypedType.dealias(wildcardExample)
    val existingArgs = UntypedType.typeArguments(dealiased)
    UntypedType.applyTypeArgs(
      UntypedType.typeConstructor(dealiased),
      leading ++ existingArgs.drop(leading.size)
    )
  }

  // NOTE: there are deliberately NO ambient implicit `Type`s (Int/String/Option/...) - inside `Expr.quote` the
  // cross-quotes plugin summons `Type`s automatically; plain (non-quoted) shared code creates local `implicit val`s
  // with inline `Type.of[...]` (or a helper def with its own `[X: Type]` parameters when existential-imported types
  // are involved).

  /** Caches a computed `F[A]` per `Type[A]` (keys compared with `=:=`). */
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
    private val storage = scala.collection.mutable.ListBuffer.empty[Entry]

    def apply[A](key: Type[A])(newValue: => F[A]): F[A] =
      storage.find(_.key =:= key) match {
        case Some(found) => found.value.asInstanceOf[F[A]]
        case None        =>
          val value = newValue
          storage += Entry(key, value)
          value
      }
  }
}
