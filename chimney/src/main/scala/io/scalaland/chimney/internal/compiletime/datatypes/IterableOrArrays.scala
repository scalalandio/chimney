package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitions

import scala.collection.Factory

/** DESIGN CHOICE: implemented on Hearth's `Type.CtorN.fromUntyped` extractors (Map/Iterable/Iterator/Array) plus
  * `Type.isIArray`, NOT on Hearth's `IsCollection`/`IsMap`, because the semantics differ:
  *   - Hearth's `IsCollection` also matches `String`, `Option` (and anything provided by loaded extensions) - matching
  *     them would change rule dispatch, e.g. String-to-String transformations would suddenly become iterable
  *     transformations,
  *   - Hearth's providers refuse to match when `Factory`/`ClassTag` cannot be summoned at parse time, while this trait
  *     matches on the type shape alone and only summons when `factory` is actually used - e.g. transforming FROM
  *     `Array[T]` with an abstract `T` (no `ClassTag`) works because only `iterator` is needed,
  *   - this also keeps the plain-value datatypes layer independent from standard-extension loading.
  * NOTE: `IsCollection`/`IsMap` ARE consulted - but one layer up, in
  * [[io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations.TotallyBuildIterables]], as a
  * fallback that runs only AFTER this hardcoded matching failed (see its ScalaDoc for the guards). This trait stays
  * extension-free on purpose.
  *
  * `map` for `Array`/`IArray` returns an `Iterator[B]` expression, NOT `Array[B]`/`IArray[B]` - building an Array would
  * require summoning `ClassTag[B]` inside the emitted code; no Chimney rule calls `map`. `IArray` support lives in
  * shared code (Scala 2 sees `Type.isIArray == false`): the generated code casts to `Array[Inner]` (valid: `IArray` is
  * `Array` at runtime) and adapts a summoned `Factory[Inner, Array[Inner]]` rather than requiring a dedicated
  * `Factory[Inner, IArray[Inner]]` instance.
  */
private[compiletime] trait IterableOrArrays { this: ChimneyDefinitions & hearth.MacroCommons =>

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

    private lazy val MapCtor: Type.Ctor2[scala.collection.Map] = Type.Ctor2.of[scala.collection.Map]
    private lazy val IterableCtor: Type.Ctor1[Iterable] = Type.Ctor1.of[Iterable]
    private lazy val IteratorCtor: Type.Ctor1[Iterator] = Type.Ctor1.of[Iterator]
    private lazy val ArrayCtor: Type.Ctor1[Array] = Type.Ctor1.of[Array]

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

    // Kept in a separate method (regular type parameters): cross-quotes `Type.of` resolves implicit `Type`s only
    // best-effort, and for existential-imported types the reliable form is "a def with type bounds" - a documented,
    // non-fixable Cross-Quotes limitation (https://scala-hearth.readthedocs.io/en/stable/cross-quotes/#limitations,
    // points 1 and 4), NOT a Hearth bug.
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

          // Returns Iterator[B], not Array[B] - see the trait's ScalaDoc (member is unused).
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

          // Returns Iterator[B], not IArray[B] - see the trait's ScalaDoc (member is unused).
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
