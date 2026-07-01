package io.scalaland.chimney.internal.compiletime2.datatypes

import io.scalaland.chimney.internal.compiletime2.ChimneyDefinitions

import scala.collection.compat.Factory

/** Hearth-based port of `io.scalaland.chimney.internal.compiletime.datatypes.IterableOrArrays`.
  *
  * DESIGN CHOICE: implemented on Hearth's `Type.CtorN.fromUntyped` extractors (Map/Iterable/Iterator/Array) plus
  * `Type.isIArray`, NOT on Hearth's `IsCollection`/`IsMap`, because macro-commons semantics differ:
  *   - Hearth's `IsCollection` also matches `String`, `Option` (and anything provided by loaded extensions) - types
  *     that macro-commons' `IterableOrArray` did NOT treat as collections (matching them would change rule dispatch,
  *     e.g. String-to-String transformations would suddenly become iterable transformations),
  *   - Hearth's providers refuse to match when `Factory`/`ClassTag` cannot be summoned at parse time, while
  *     macro-commons matched on the type shape alone and only summoned (unsafely) when `factory` was actually used -
  *     e.g. transforming FROM `Array[T]` with an abstract `T` (no `ClassTag`) works in macro-commons because only
  *     `iterator` is needed,
  *   - this also keeps the plain-value datatypes layer independent from standard-extension loading.
  * TODO(hearth-migration): when the rules are ported, consider ALSO consulting `IsCollection`/`IsMap` (after these
  * built-in shapes) so that third-party Hearth collection extensions are picked up - as a new, opt-in feature.
  *
  * Other judgment calls:
  *   - `map` for `Array`/`IArray` returns an `Iterator[B]` expression where macro-commons returned `Array[B]`/
  *     `IArray[B]` - building an Array requires summoning `ClassTag[B]` inside the emitted code; `map` is not called
  *     by any Chimney rule (verified by grep - only `factory`/`iterator`/`to` are used via
  *     `TotallyOrPartiallyBuildIterable`), the member exists purely for API-shape parity,
  *   - `IArray` support is written in shared code (Scala 2 sees `Type.isIArray == false`): the generated code casts
  *     to `Array[Inner]` (valid: `IArray` is `Array` at runtime) and the factory adapts a summoned
  *     `Factory[Inner, Array[Inner]]` instead of using chimney's Scala 3-only `FactoryCompat.iarrayFactory`.
  */
private[compiletime2] trait IterableOrArrays { this: ChimneyDefinitions & hearth.MacroCommons =>

  /** Something allowing us to dispatch same-looking-source-code-but-different ASTs for Iterables and Arrays.
    *
    * Exists because `Array` is NOT `Iterable`, and all operations like `.map`, `.to`, etc are done through extension
    * methods. Meanwhile, we would like to be able to convert to and from Array easily.
    */
  abstract protected class IterableOrArray[M, A] {
    def factory: Expr[Factory[A, M]]

    def iterator(m: Expr[M]): Expr[Iterator[A]]

    def map[B: Type](m: Expr[M])(f: Expr[A => B]): ExistentialExpr

    def to[C: Type](m: Expr[M])(factory: Expr[Factory[A, C]]): Expr[C]
  }
  protected object IterableOrArray {

    // `fromUntyped` wrappers use `=:=` + `baseType` for matching, which is reliable across compilation-unit
    // boundaries (see the hearth-value-types skill notes on `Type.CtorN.fromUntyped`).
    private lazy val MapCtor: Type.Ctor2[scala.collection.Map] =
      Type.Ctor2.fromUntyped[scala.collection.Map](Type.Ctor2.of[scala.collection.Map].asUntyped)
    private lazy val IterableCtor: Type.Ctor1[Iterable] =
      Type.Ctor1.fromUntyped[Iterable](Type.Ctor1.of[Iterable].asUntyped)
    private lazy val IteratorCtor: Type.Ctor1[Iterator] =
      Type.Ctor1.fromUntyped[Iterator](Type.Ctor1.of[Iterator].asUntyped)
    private lazy val ArrayCtor: Type.Ctor1[Array] =
      Type.Ctor1.fromUntyped[Array](Type.Ctor1.of[Array].asUntyped)

    private type Cached[M] = Option[Existential[IterableOrArray[M, *]]]
    private val iterableOrArrayCache = new TypeCache[Cached]
    final def parse[M](implicit M: Type[M]): Option[Existential[IterableOrArray[M, *]]] =
      iterableOrArrayCache(M)(M match {
        case MapCtor(k, v) =>
          import k.Underlying as K, v.Underlying as V
          val a = tuple2TypeOf[K, V]
          import a.Underlying as Inner
          Some(buildInIterableSupport[M, Inner](s"support build-in for Map-type ${Type.prettyPrint[M]}"))
        case IterableCtor(a) =>
          import a.Underlying as Inner
          Some(buildInIterableSupport[M, Inner](s"support build-in for Iterable-type ${Type.prettyPrint[M]}"))
        case IteratorCtor(a) =>
          import a.Underlying as Inner
          Some(buildInIteratorSupport[M, Inner])
        case ArrayCtor(a) =>
          import a.Underlying as Inner
          Some(buildInArraySupport[M, Inner])
        case _ if Type.isIArray[M] =>
          UntypedType.typeArguments(M.asUntyped) match {
            case elem :: Nil =>
              val a = UntypedType.as_??(elem)
              import a.Underlying as Inner
              Some(buildInIArraySupport[M, Inner])
            case _ => None
          }
        case _ => None
      })
    final def unapply[M](M: Type[M]): Option[Existential[IterableOrArray[M, *]]] = parse(using M)

    // Kept in a separate method (regular type parameters) - cross-quotes `Type.of` referencing existential-imported
    // types directly inside `parse` trips the plugin's workaround-method generation.
    private def tuple2TypeOf[K: Type, V: Type]: ?? = Type.of[(K, V)].as_??

    @scala.annotation.nowarn("msg=is never used")
    private def buildInIterableSupport[M: Type, Inner: Type](hint: String): Existential[IterableOrArray[M, *]] =
      Existential[IterableOrArray[M, *], Inner](
        new IterableOrArray[M, Inner] {
          implicit private val IterableInner: Type[Iterable[Inner]] = Type.of[Iterable[Inner]]

          def factory: Expr[Factory[Inner, M]] = {
            implicit val FactoryInnerM: Type[Factory[Inner, M]] = Type.of[Factory[Inner, M]]
            summonImplicitUnsafeOf[Factory[Inner, M]]
          }

          def iterator(m: Expr[M]): Expr[Iterator[Inner]] =
            Expr.quote(Expr.splice(m.upcast[Iterable[Inner]]).iterator)

          def map[B: Type](m: Expr[M])(f: Expr[Inner => B]): ExistentialExpr = {
            implicit val IterableB: Type[Iterable[B]] = Type.of[Iterable[B]]
            Expr.quote(Expr.splice(m.upcast[Iterable[Inner]]).map(Expr.splice(f))).as_??
          }

          def to[C: Type](m: Expr[M])(factory: Expr[Factory[Inner, C]]): Expr[C] =
            Expr.quote(Expr.splice(m.upcast[Iterable[Inner]]).to(Expr.splice(factory)))

          override def toString: String = hint
        }
      )

    @scala.annotation.nowarn("msg=is never used")
    private def buildInIteratorSupport[M: Type, Inner: Type]: Existential[IterableOrArray[M, *]] =
      Existential[IterableOrArray[M, *], Inner](
        new IterableOrArray[M, Inner] {
          implicit private val IteratorInner: Type[Iterator[Inner]] = Type.of[Iterator[Inner]]

          def factory: Expr[Factory[Inner, M]] = {
            implicit val FactoryInnerM: Type[Factory[Inner, M]] = Type.of[Factory[Inner, M]]
            summonImplicitUnsafeOf[Factory[Inner, M]]
          }

          def iterator(m: Expr[M]): Expr[Iterator[Inner]] =
            m.upcast[Iterator[Inner]]

          def map[B: Type](m: Expr[M])(f: Expr[Inner => B]): ExistentialExpr = {
            implicit val IteratorB: Type[Iterator[B]] = Type.of[Iterator[B]]
            Expr.quote(Expr.splice(m.upcast[Iterator[Inner]]).map(Expr.splice(f))).as_??
          }

          def to[C: Type](m: Expr[M])(factory: Expr[Factory[Inner, C]]): Expr[C] =
            Expr.quote(Expr.splice(m.upcast[Iterator[Inner]]).to(Expr.splice(factory)))

          override def toString: String = s"support build-in for Iterator-type ${Type.prettyPrint[M]}"
        }
      )

    @scala.annotation.nowarn("msg=is never used")
    private def buildInArraySupport[M: Type, Inner: Type]: Existential[IterableOrArray[M, *]] =
      Existential[IterableOrArray[M, *], Inner](
        new IterableOrArray[M, Inner] {
          implicit private val ArrayInner: Type[Array[Inner]] = Type.of[Array[Inner]]

          def factory: Expr[Factory[Inner, M]] = {
            implicit val FactoryInnerM: Type[Factory[Inner, M]] = Type.of[Factory[Inner, M]]
            summonImplicitUnsafeOf[Factory[Inner, M]]
          }

          def iterator(m: Expr[M]): Expr[Iterator[Inner]] =
            Expr.quote(Expr.splice(m.upcast[Array[Inner]]).iterator)

          // Returns Iterator[B] instead of macro-commons' Array[B] - see the trait's ScalaDoc (member is unused).
          def map[B: Type](m: Expr[M])(f: Expr[Inner => B]): ExistentialExpr = {
            implicit val IteratorB: Type[Iterator[B]] = Type.of[Iterator[B]]
            Expr.quote(Expr.splice(m.upcast[Array[Inner]]).iterator.map(Expr.splice(f))).as_??
          }

          def to[C: Type](m: Expr[M])(factory: Expr[Factory[Inner, C]]): Expr[C] =
            Expr.quote(Expr.splice(m.upcast[Array[Inner]]).to(Expr.splice(factory)))

          override def toString: String = s"support build-in for Array-type ${Type.prettyPrint[M]}"
        }
      )

    @scala.annotation.nowarn("msg=is never used")
    private def buildInIArraySupport[M: Type, Inner: Type]: Existential[IterableOrArray[M, *]] =
      Existential[IterableOrArray[M, *], Inner](
        new IterableOrArray[M, Inner] {
          implicit private val ArrayInner: Type[Array[Inner]] = Type.of[Array[Inner]]

          // At runtime IArray[Inner] IS Array[Inner], so the generated `.asInstanceOf[Array[Inner]]` is a no-op.
          private def asArray(m: Expr[M]): Expr[Array[Inner]] = castToExpr[M, Array[Inner]](m)

          def factory: Expr[Factory[Inner, M]] = {
            implicit val FactoryInnerArray: Type[Factory[Inner, Array[Inner]]] = Type.of[Factory[Inner, Array[Inner]]]
            val arrayFactory = summonImplicitUnsafeOf[Factory[Inner, Array[Inner]]]
            Expr.quote(Expr.splice(arrayFactory).asInstanceOf[Factory[Inner, M]])
          }

          def iterator(m: Expr[M]): Expr[Iterator[Inner]] =
            Expr.quote(Expr.splice(asArray(m)).iterator)

          // Returns Iterator[B] instead of macro-commons' IArray[B] - see the trait's ScalaDoc (member is unused).
          def map[B: Type](m: Expr[M])(f: Expr[Inner => B]): ExistentialExpr = {
            implicit val IteratorB: Type[Iterator[B]] = Type.of[Iterator[B]]
            Expr.quote(Expr.splice(asArray(m)).iterator.map(Expr.splice(f))).as_??
          }

          def to[C: Type](m: Expr[M])(factory: Expr[Factory[Inner, C]]): Expr[C] =
            Expr.quote(Expr.splice(asArray(m)).to(Expr.splice(factory)))

          override def toString: String = s"support build-in for IArray-type ${Type.prettyPrint[M]}"
        }
      )
  }
}
