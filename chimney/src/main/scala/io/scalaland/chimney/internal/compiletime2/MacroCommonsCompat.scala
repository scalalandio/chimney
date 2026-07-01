package io.scalaland.chimney.internal.compiletime2

/** Compatibility shims between chimney-macro-commons API shapes and Hearth 0.4.0.
  *
  * Only contains what the Hearth-based `compiletime2` foundation (and, transitively, the rules that will be ported on
  * top of it) actually needs and what has no direct Hearth counterpart. Everything that maps 1:1 (see `api-mapping.md`)
  * should be used directly from Hearth instead of being aliased here.
  *
  * NOT ported on purpose (REWRITE-rated in the API mapping, call sites will be migrated to Hearth natives):
  *   - `ExprPromise`/`PrependDefinitionsTo`/`PatternMatchCase` - use `LambdaBuilder`/`ValDefs`/`MatchCase`
  *   - `Expr.Option`/`Expr.Either`/`Expr.Iterable`/... modules - use `IsOption`/`IsEither`/`IsCollection`/`IsMap`
  */
private[compiletime2] trait MacroCommonsCompat { this: hearth.MacroCommons =>

  /** macro-commons `ExistentialType` == Hearth's `??` (kept as alias for mechanical porting). */
  final type ExistentialType = ??

  /** macro-commons `ExistentialExpr` == Hearth's `Expr_??` (kept as alias for mechanical porting). */
  final type ExistentialExpr = Expr_??

  /** macro-commons `Expr.asInstanceOf[A, B](expr)` - emits an actual `.asInstanceOf[B]` cast in the generated code
    * (unlike Hearth's `Expr.upcast[A, B]` which is a compile-time-verified widening with no runtime cast).
    *
    * Kept as a proper method with its own type parameters (helper-method pattern) so that the Scala 2 cross-quotes
    * expansion resolves `A`/`B` through `WeakTypeTag`s instead of path-dependent types.
    */
  protected def castToExpr[A: Type, B: Type](expr: Expr[A]): Expr[B] =
    Expr.quote {
      Expr.splice(expr).asInstanceOf[B]
    }

  /** macro-commons `ExprOps.asInstanceOfExpr[B]` counterpart. */
  implicit final protected class CompatExprOps[A](private val expr: Expr[A]) {

    def asInstanceOfExpr[B](implicit A: Type[A], B: Type[B]): Expr[B] = castToExpr[A, B](expr)
  }

  /** macro-commons `ExistentialExprOps.asInstanceOfExpr[B]`/`.upcastToExprOf[B]` counterpart (ops on `Expr_??`). */
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

  /** macro-commons `Results#reportError(errors: String): Nothing` counterpart (Hearth spells it
    * `Environment.reportErrorAndAbort`).
    */
  protected def reportError(errors: String): Nothing = Environment.reportErrorAndAbort(errors)

  /** macro-commons `Definitions#XMacroSettings` counterpart (Hearth exposes it on `Environment`). */
  protected def XMacroSettings: List[String] = Environment.XMacroSettings

  /** macro-commons `TypeStringOps#extractStringSingleton` counterpart (same assertion on non-literal types). */
  implicit final protected class CompatTypeStringOps[S <: String](private val S: Type[S]) {

    def extractStringSingleton: String =
      Type.StringCodec.fromType(S).map(_.value).getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        assertionFailed(s"Invalid string literal type: ${Type.prettyPrint(using S)}")
        // $COVERAGE-ON$
      }
  }

  /** macro-commons `Type.extractObjectSingleton[M]: Option[M]` counterpart.
    *
    * Hearth's `Type.ModuleCodec` is the same classloader-probing implementation (both descend from the same code), but
    * its type parameter is bounded by `Singleton`, which chimney's call sites (`M <: TransformedNamesComparison`) do
    * not satisfy - hence the cast through `Nothing` (the codec's implementation is a single erased object).
    */
  protected def extractObjectSingletonOf[M: Type]: Option[M] =
    Type.ModuleCodec[Nothing].asInstanceOf[TypeCodec[M]].fromType(Type[M]).map(_.value)

  /** macro-commons `Expr.summonImplicit[A]: Option[Expr[A]]` counterpart.
    *
    * TODO(hearth-migration): during the rules port consider switching call sites to Hearth's
    * `Expr.summonImplicitIgnoring(...)` to replace Chimney's manual self-recursion exclusion logic.
    */
  protected def summonImplicitOptionOf[A: Type]: Option[Expr[A]] = Expr.summonImplicit[A].toOption

  /** macro-commons `Expr.summonImplicitUnsafe[A]` counterpart. */
  protected def summonImplicitUnsafeOf[A: Type]: Expr[A] = Expr.summonImplicit[A].get

  /** macro-commons `Expr.nowarn[A](warnings)(expr)` counterpart.
    *
    * TODO(hearth-migration): Hearth has no annotation-attaching API in the typed layer - implement
    * `@scala.annotation.nowarn` attachment (with a dynamic filter string) on untyped trees, or redesign GatewayCommons
    * so the generated code does not need the suppression. Until then this is an identity, which only affects the opt-in
    * (via `-Xmacro-settings`) warning suppression of generated code, not its correctness.
    */
  protected def nowarnExpr[A: Type](warnings: Option[String])(expr: Expr[A]): Expr[A] = {
    hearth.fp.ignore(warnings)
    expr
  }

  /** macro-commons `Expr.SuppressWarnings[A](warnings)(expr)` counterpart.
    *
    * TODO(hearth-migration): same as [[nowarnExpr]] - needs `@SuppressWarnings(Array(...))` attachment on untyped
    * trees; identity until the Gateway port decides the final approach.
    */
  protected def suppressWarningsExpr[A: Type](warnings: List[String])(expr: Expr[A]): Expr[A] = {
    hearth.fp.ignore(warnings)
    expr
  }

  /** Workaround for a Hearth 0.4.0 bug: on Scala 2 `Type.CtorN.UpperBounded.of[...]` (and `Bounded.of`) with a
    * non-`Any` upper bound expands to code that does not typecheck (the generated `matchResult` casts the extracted
    * argument to `Type[scala.Any]` and then calls `.as_<:??<:[L, U]`, which requires `U >: Any`).
    *
    * These factories hand-build the same `Type.CtorN.UpperBounded` instances on top of Hearth's untyped API instead.
    * `applied` is the type constructor applied to its upper bounds - it only serves as a way to obtain the untyped type
    * constructor in shared code.
    *
    * Semantics difference vs the cross-quotes-generated instances: `unapply` matches on the exact (dealiased) type
    * constructor, without `baseType` subtype-awareness - which is enough for Chimney's phantom-type configs.
    *
    * TODO(hearth-migration): remove once fixed upstream (report to https://github.com/kubuszok/hearth/issues).
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

  /** Workaround for a Hearth 0.4.0 bug: on Scala 2 cross-quotes `Type.of[F[A, ?]]` fails to compile whenever the
    * enclosing method has type parameters ("not found: type ?$1" - the generated workaround method loses the wildcard).
    * `Type.of[F[Any, ?]]` in a member without type parameters expands fine, so `ChimneyType.*.inferred` captures such
    * an example once and then replaces the leading type arguments with the actual ones.
    *
    * The shared implementation is correct on Scala 3 (wildcard arguments survive `applyTypeArgs`); on Scala 2 it would
    * leave the existential's quantified symbols unbound, so [[PlatformBridge]] (Scala 2) overrides it with a version
    * that re-quantifies via `internal.existentialAbstraction`.
    *
    * TODO(hearth-migration): remove once fixed upstream (report to https://github.com/kubuszok/hearth/issues).
    */
  protected def reapplyLeadingTypeArgsCompat(wildcardExample: UntypedType, leading: List[UntypedType]): UntypedType = {
    val dealiased = UntypedType.dealias(wildcardExample)
    val existingArgs = UntypedType.typeArguments(dealiased)
    UntypedType.applyTypeArgs(dealiased, leading ++ existingArgs.drop(leading.size))
  }

  // TODO(hearth-migration): macro-commons `Type.Implicits` (ambient implicit `Type`s for Int/String/Option/...) is not
  // needed by the compiletime2 foundation itself - inside `Expr.quote` the cross-quotes plugin summons `Type`s
  // automatically. If the ported rules turn out to rely on `import Type.Implicits.*` for plain (non-quoted) shared
  // code, recreate the object here with `implicit val IntType: Type[Int] = Type.of[Int]` etc.

  /** macro-commons `Type.Cache[F[_]]` counterpart (verbatim copy - it is pure shared code over `Type` + `=:=`).
    *
    * We cannot add members to Hearth's `Type` module, so call sites change `new Type.Cache[F]` -> `new TypeCache[F]`.
    * Used by the `datatypes` adapters and (later) by TotallyBuildIterables/OuterTransformers.
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
