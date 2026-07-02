package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.Factory

/** Hearth-based port of `...compiletime.derivation.transformer.integrations.TotallyBuildIterables`.
  *
  * Differences vs the old version:
  *   - `new Type.Cache[Cached]` becomes `new TypeCache[Cached]`,
  *   - `Type.Map.unapply` (macro-commons `Ctor2`) becomes a `Type.Ctor2.fromUntyped[scala.collection.Map]` extractor
  *     (same `baseType`-aware matching, mirrors [[datatypes.IterableOrArrays]]).
  *
  * EXTENSION FALLBACK (Phase 5 prereq): [[TotallyBuildIterable.parse]] gained a THIRD alternative consulting Hearth's
  * `IsCollection`/`IsMap` providers (built-ins AND ServiceLoader-registered `StandardMacroExtension`s). This is the
  * mechanism that will let chimney-java-collections (and Kindlings-style collection extensions) plug into the
  * MapToMap/IterableToIterable rules without implicits. Precedence and guards (order matters):
  *   - it ranks BELOW `providedSupport` (the [[io.scalaland.chimney.integrations.TotallyBuildIterable]] implicit) and
  *     below `buildInSupport` ([[IterableOrArray]]'s hardcoded Map/Iterable/Iterator/Array/IArray shapes) - anything
  *     the old engine matched keeps its exact old expansion; the fallback is only consulted for types the existing
  *     engine REJECTED,
  *   - `String` is filtered out: Hearth's built-in `IsCollectionProviderForString` would otherwise turn every String
  *     into a `Char` collection and change rule dispatch for String-typed fields,
  *   - `scala.Option`/`scala.Either` shapes are filtered out: Hearth's `IsCollectionProviderForScalaOption` models
  *     Option as an at-most-1-element collection - chimney handles optionals via [[OptionalValues]] and eithers via the
  *     EitherToEither rule and must keep doing so (Option-to-List etc. staying unsupported is covered behavior). Hearth
  *     0.4.0 has no Either-as-collection built-in, but the filter also pins the semantics against third-party
  *     providers,
  *   - any type that Hearth's `IsOption` matches is filtered out (e.g. `java.util.Optional`, which Hearth's built-ins
  *     model BOTH as an option and as a collection): optional semantics win, mirroring the OptionToOption-before-
  *     IterableToIterable rule order; such types go through [[OptionalValues]]' own fallback instead,
  *   - only "total-shaped" providers are accepted: `build` must be a `CtorLikeOf.PlainValue` with `CtorResult =:= M`
  *     (all Hearth built-in scala/java collection providers are of this shape). Smart-constructor providers (e.g.
  *     Kindlings' NonEmptyList with `EitherStringOrValue`) cannot be a TOTAL factory; TODO(hearth-extensions): map
  *     those onto [[PartiallyBuildIterables]] in Phase 5 proper,
  *   - it is SKIPPED when a `PartiallyBuildIterable`/`OptionalValue` implicit exists for the type: integrations
  *     implicits must beat extension providers ([[TotallyOrPartiallyBuildIterable]] tries Totally BEFORE Partially, and
  *     MapToMap/IterableToIterable run before ToOption, so without these guards an extension match would shadow the
  *     implicit),
  *   - map-ness is detected via `IsCollectionOf.asMap` (Hearth's `IsMap` is exactly `IsCollection` + `asMap`, so
  *     "consult IsMap first, then IsCollection" collapses into one parse + a shape check). Hearth map providers use
  *     provider-specific `Pair` types (e.g. `java.util.Map.Entry`), while chimney's Map support requires
  *     `Item =:= (K, V)` at codegen level - the fallback adapts by mapping the pair iterator to tuples and wrapping the
  *     provider's `Factory[Pair, M]` in a generated `Factory[(K, V), M]` (`pair(k, v)` inserted in `addOne`).
  * KNOWN PITFALL (accepted & documented): Hearth providers gate on `Factory`/`ClassTag` summonability at PARSE time
  * (e.g. `IsCollectionProviderForScalaCollection` skips when `Factory[Item, M]` cannot be summoned), while chimney's
  * hardcoded shapes only summon when the factory is actually USED. That difference is invisible here because the
  * hardcoded shapes win for every type they match; for extension-provided types parse-time gating is the provider
  * author's contract. NEW CAPABILITY (not a covered-behavior change): on the JVM, Hearth 0.4.0's built-in `java.util.*`
  * collection providers make Java collections derivable WITHOUT the chimney-java-collections import (that module's
  * planned migration path). No core spec covered `java.util` collections before; with the import, the module's
  * implicits keep winning via `providedSupport`. (On JS/Native Hearth ships no Java providers - the fallback never
  * matches there.) The fallback calls `ensureStandardExtensionsLoaded()` (idempotent; the Gateways already load at
  * entry).
  */
trait TotallyBuildIterables { this: Derivation & hearth.MacroCommons & hearth.std.StdExtensions =>

  // Cross-quotes helpers for the Hearth-provider fallback - hoisted to the (unshadowed) trait level and kept in
  // methods with regular type parameters (the cross-quotes helper-def pattern; see ScalaStdCompat's GOTCHA).

  private lazy val hearthFallbackStringType: Type[String] = Type.of[String]

  @scala.annotation.nowarn("msg=is never used")
  private def iterableIteratorCompat[A: Type](iterable: Expr[Iterable[A]]): Expr[Iterator[A]] = {
    implicit val IterableA: Type[Iterable[A]] = Type.of[Iterable[A]]
    implicit val IteratorA: Type[Iterator[A]] = ScalaType.Iterator[A]
    Expr.quote(Expr.splice(iterable).iterator)
  }

  @scala.annotation.nowarn("msg=is never used")
  private def pairIteratorToTupleIteratorCompat[Pair: Type, K: Type, V: Type](
      iterator: Expr[Iterator[Pair]],
      toTuple: Expr[Pair] => Expr[(K, V)]
  ): Expr[Iterator[(K, V)]] = {
    implicit val IteratorPair: Type[Iterator[Pair]] = ScalaType.Iterator[Pair]
    implicit val TupleKV: Type[(K, V)] = ScalaType.Tuple2[K, V]
    implicit val IteratorKV: Type[Iterator[(K, V)]] = ScalaType.Iterator[(K, V)]
    Expr.quote {
      Expr.splice(iterator).map { (pair: Pair) =>
        Expr.splice(toTuple(Expr.quote(pair)))
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  private def tupleFactoryFromPairFactoryCompat[Pair: Type, K: Type, V: Type, M: Type](
      pairFactory: Expr[Factory[Pair, M]],
      fromTuple: Expr[(K, V)] => Expr[Pair]
  ): Expr[Factory[(K, V), M]] = {
    implicit val TupleKV: Type[(K, V)] = ScalaType.Tuple2[K, V]
    implicit val FactoryPairM: Type[Factory[Pair, M]] = ScalaType.Factory[Pair, M]
    implicit val FactoryKVM: Type[Factory[(K, V), M]] = ScalaType.Factory[(K, V), M]
    Expr.quote {
      new scala.collection.Factory[(K, V), M] {
        private val underlying = Expr.splice(pairFactory)
        override def fromSpecific(it: IterableOnce[(K, V)]): M =
          underlying.fromSpecific(it.iterator.map { (tuple: (K, V)) =>
            Expr.splice(fromTuple(Expr.quote(tuple)))
          })
        override def newBuilder: scala.collection.mutable.Builder[(K, V), M] = {
          val impl = underlying.newBuilder
          new scala.collection.mutable.Builder[(K, V), M] {
            override def clear(): Unit = impl.clear()
            override def result(): M = impl.result()
            override def addOne(elem: (K, V)): this.type = {
              impl.addOne(Expr.splice(fromTuple(Expr.quote(elem))))
              this
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  private def tupleFirstCompat[A: Type, B: Type](tuple: Expr[(A, B)]): Expr[A] = {
    implicit val TupleAB: Type[(A, B)] = ScalaType.Tuple2[A, B]
    Expr.quote(Expr.splice(tuple)._1)
  }

  @scala.annotation.nowarn("msg=is never used")
  private def tupleSecondCompat[A: Type, B: Type](tuple: Expr[(A, B)]): Expr[B] = {
    implicit val TupleAB: Type[(A, B)] = ScalaType.Tuple2[A, B]
    Expr.quote(Expr.splice(tuple)._2)
  }

  /** Something allowing us to share the logic which handles [[scala.collection.Iterable]], [[scala.Array]],
    * [[java.util.Collection]], ... and whatever we want to support.
    *
    * Tries to use [[io.scalaland.chimney.integrations.TotallyBuildIterable]] and then falls back on [[IterableOrArray]]
    * hardcoded support, if type is eligible.
    */
  abstract protected class TotallyBuildIterable[Collection, Item]
      extends TotallyOrPartiallyBuildIterable[Collection, Item] {

    def factory: Either[Expr[Factory[Item, Collection]], Expr[Factory[Item, partial.Result[Collection]]]] = Left(
      totalFactory
    )

    def totalFactory: Expr[Factory[Item, Collection]]

    def iterator(collection: Expr[Collection]): Expr[Iterator[Item]]

    def to[Collection2: Type](
        collection: Expr[Collection],
        factory: Expr[Factory[Item, Collection2]]
    ): Expr[Collection2]

    val asMap: Option[(ExistentialType, ExistentialType)]
  }
  protected object TotallyBuildIterable {

    private lazy val MapCtor: Type.Ctor2[scala.collection.Map] =
      Type.Ctor2.fromUntyped[scala.collection.Map](Type.Ctor2.of[scala.collection.Map].asUntyped)

    private type Cached[M] = Option[Existential[TotallyBuildIterable[M, *]]]
    private val totallyBulidIterableCache = new TypeCache[Cached]
    def parse[M](implicit M: Type[M]): Option[Existential[TotallyBuildIterable[M, *]]] =
      totallyBulidIterableCache(M)(
        providedSupport[M].orElse(buildInSupport[M]).orElse(hearthProviderSupport[M])
      )
    def unapply[M](M: Type[M]): Option[Existential[TotallyBuildIterable[M, *]]] = parse(using M)

    private def providedSupport[Collection: Type]: Option[Existential[TotallyBuildIterable[Collection, *]]] =
      summonTotallyBuildIterable[Collection].map { totallyBuildIterable =>
        import totallyBuildIterable.{Underlying as Item, value as totallyBuildIterableExpr}
        Existential[TotallyBuildIterable[Collection, *], Item](
          new TotallyBuildIterable[Collection, Item] {

            def totalFactory: Expr[Factory[Item, Collection]] =
              totallyBuildIterableExpr.totalFactory

            def iterator(collection: Expr[Collection]): Expr[Iterator[Item]] =
              totallyBuildIterableExpr.iterator(collection)

            def to[Collection2: Type](
                collection: Expr[Collection],
                factory: Expr[Factory[Item, Collection2]]
            ): Expr[Collection2] = totallyBuildIterableExpr.to(collection, factory)

            val asMap: Option[(ExistentialType, ExistentialType)] = totallyBuildIterableExpr.tpe match {
              case ChimneyType.TotallyBuildMap(_, key, value) => Some(key -> value)
              case _                                          => None
            }

            override def toString: String = s"support provided by ${Expr.prettyPrint(totallyBuildIterableExpr)}"
          }
        )
      }

    private def buildInSupport[M: Type]: Option[Existential[TotallyBuildIterable[M, *]]] =
      IterableOrArray.parse[M].map { found =>
        import found.{Underlying as Item, value as iora}
        Existential[TotallyBuildIterable[M, *], Item](
          new TotallyBuildIterable[M, Item] {

            def totalFactory: Expr[Factory[Item, M]] =
              iora.factory

            def iterator(collection: Expr[M]): Expr[Iterator[Item]] =
              iora.iterator(collection)

            def to[Collection2: Type](
                collection: Expr[M],
                factory: Expr[Factory[Item, Collection2]]
            ): Expr[Collection2] = iora.to(collection)(factory)

            val asMap: Option[(ExistentialType, ExistentialType)] = Type[M] match {
              case MapCtor(key, value) => Some(key -> value)
              case _                   => None
            }

            override def toString: String = iora.toString
          }
        )
      }

    /** Fallback consulting Hearth `IsCollection`/`IsMap` providers registered by `StandardMacroExtension`s - see the
      * trait's ScalaDoc for the full list of guards and their rationale.
      */
    private def hearthProviderSupport[M: Type]: Option[Existential[TotallyBuildIterable[M, *]]] = {
      ensureStandardExtensionsLoaded()
      // HEARTH GOTCHA (report upstream): bottom types conform to everything (`Null <:< java.util.Optional[?]` etc.),
      // so `<:<`-matching built-in providers match `Null`/`Nothing` and then CRASH eagerly while building their exprs
      // (upcast assertion at parse time). Never consult providers for bottom types.
      if (Type[M] <:< ScalaType.Implicits.NullType) None
      else if (Type[M] =:= hearthFallbackStringType) None // String-as-collection excluded
      else if (Type[M].isOption || Type[M].isEither) None // Option/Either-as-collection excluded
      else if (IsOption.unapply(Type[M]).isDefined) None // optional semantics win (handled by OptionalValues)
      else
        IsCollection.unapply(Type[M]).flatMap { isCollection =>
          import isCollection.{Underlying as Item, value as isCollectionOf}
          val isTotalShaped = (isCollectionOf.build match {
            case _: CtorLikeOf.PlainValue[?, ?] => true
            case _                              => false // smart-constructor providers are not TOTAL - see ScalaDoc
          }) && (isCollectionOf.CtorResult =:= Type[M])
          if (!isTotalShaped) None
          // Integrations implicits beat extension providers - only summoned when a provider actually matched.
          else if (summonPartiallyBuildIterable[M].isDefined || summonOptionalValue[M].isDefined) None
          else
            isCollectionOf.asMap match {
              case Some(isMapOf) =>
                val key = isMapOf.Key.as_??
                val value = isMapOf.Value.as_??
                import key.Underlying as K, value.Underlying as V
                Some(mkHearthMapSupport[M, Item, K, V](isMapOf))
              case None =>
                Some(mkHearthIterableSupport[M, Item](isCollectionOf))
            }
        }
    }

    // Kept in separate methods (regular type parameters) - the cross-quotes helper-def pattern.

    private def mkHearthIterableSupport[M: Type, Item: Type](
        isCollectionOf: IsCollectionOf[M, Item]
    ): Existential[TotallyBuildIterable[M, *]] =
      Existential[TotallyBuildIterable[M, *], Item](
        new TotallyBuildIterable[M, Item] {

          def totalFactory: Expr[Factory[Item, M]] =
            // CtorResult =:= M was checked by the caller - same runtime value, equivalent tree type.
            isCollectionOf.factory.asInstanceOf[Expr[Factory[Item, M]]]

          def iterator(collection: Expr[M]): Expr[Iterator[Item]] =
            iterableIteratorCompat(isCollectionOf.asIterable(collection))

          def to[Collection2: Type](
              collection: Expr[M],
              factory: Expr[Factory[Item, Collection2]]
          ): Expr[Collection2] = iterator(collection).to(factory)

          val asMap: Option[(ExistentialType, ExistentialType)] = None

          override def toString: String =
            s"support provided by Hearth extension IsCollection for ${Type.prettyPrint[M]}"
        }
      )

    private def mkHearthMapSupport[M: Type, Pair: Type, K: Type, V: Type](
        isMapOf: IsMapOf[M, Pair]
    ): Existential[TotallyBuildIterable[M, *]] = {
      implicit val TupleKV: Type[(K, V)] = ScalaType.Tuple2[K, V]
      // K/V are exactly isMapOf.Key/isMapOf.Value (extracted by the caller) - the casts below are identities that
      // only bridge the path-dependent types to the regular type parameters (cross-quotes helper-def pattern).
      def toTuple(pair: Expr[Pair]): Expr[(K, V)] =
        ScalaExpr.Tuple2(isMapOf.key(pair).asInstanceOf[Expr[K]], isMapOf.value(pair).asInstanceOf[Expr[V]])
      def fromTuple(tuple: Expr[(K, V)]): Expr[Pair] =
        isMapOf.pair(
          tupleFirstCompat(tuple).asInstanceOf[Expr[isMapOf.Key]],
          tupleSecondCompat(tuple).asInstanceOf[Expr[isMapOf.Value]]
        )
      Existential[TotallyBuildIterable[M, *], (K, V)](
        new TotallyBuildIterable[M, (K, V)] {

          def totalFactory: Expr[Factory[(K, V), M]] =
            tupleFactoryFromPairFactoryCompat[Pair, K, V, M](
              // CtorResult =:= M was checked by the caller - same runtime value, equivalent tree type.
              isMapOf.factory.asInstanceOf[Expr[Factory[Pair, M]]],
              fromTuple
            )

          def iterator(collection: Expr[M]): Expr[Iterator[(K, V)]] =
            pairIteratorToTupleIteratorCompat[Pair, K, V](
              iterableIteratorCompat(isMapOf.asIterable(collection)),
              toTuple
            )

          def to[Collection2: Type](
              collection: Expr[M],
              factory: Expr[Factory[(K, V), Collection2]]
          ): Expr[Collection2] = iterator(collection).to(factory)

          val asMap: Option[(ExistentialType, ExistentialType)] = Some(Type[K].as_?? -> Type[V].as_??)

          override def toString: String =
            s"support provided by Hearth extension IsMap for ${Type.prettyPrint[M]}"
        }
      )
    }
  }
}
