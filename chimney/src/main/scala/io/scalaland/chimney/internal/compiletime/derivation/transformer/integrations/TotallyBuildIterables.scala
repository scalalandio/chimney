package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.Factory

/** Collection support built DIRECTLY on Hearth's `IsCollection`/`IsMap` providers: the built-in providers (Scala
  * collections/Iterators/Arrays/IArrays, and `java.util.*` on the JVM) and ServiceLoader-registered
  * `StandardMacroExtension`s (e.g. Kindlings' cats collections) are ONE path - there is no separate hardcoded
  * shape-matching layer anymore.
  *
  * Precedence contract: user implicits (rules #1/#3) > integrations implicits (`providedSupport` + the
  * `PartiallyBuildIterable`/`OptionalValue` implicit guard below) > `IsCollection`-based support.
  *
  * Guards on the `IsCollection` path (rule-dispatch decisions, not mechanics):
  *   - `String` is filtered out: Hearth's `IsCollectionProviderForString` would otherwise turn every String into a
  *     `Char` collection and change rule dispatch for String-typed fields,
  *   - `scala.Option`/`scala.Either` shapes are filtered out: chimney handles optionals via [[OptionalValues]] and
  *     eithers via the EitherToEither rule,
  *   - any type that Hearth's `IsOption` matches is filtered out (e.g. `java.util.Optional`, which Hearth's built-ins
  *     model BOTH as an option and as a collection): optional semantics win, mirroring the OptionToOption-before-
  *     IterableToIterable rule order,
  *   - a `PartiallyBuildIterable`/`OptionalValue` IMPLICIT for the type wins over the `IsCollection` match
  *     (integrations implicits beat provider-based support; [[TotallyOrPartiallyBuildIterable]] tries Totally BEFORE
  *     Partially, and MapToMap/IterableToIterable run before ToOption, so without this guard a provider match would
  *     shadow the implicit).
  *
  * Provider mechanics:
  *   - only "total-shaped" providers are accepted: `build` must be a `CtorLikeOf.PlainValue`. Smart-constructor
  *     providers (e.g. Kindlings' NonEmptyList) surface through [[PartiallyBuildIterables]]' twin instead. When
  *     `CtorResult =:= M` the provider's factory is used AS-IS; when `CtorResult != M` (e.g. Kindlings' `Chain`, which
  *     accumulates into a `List[E]`) the intermediate `Factory[Item, CtorResult]` is wrapped into a generated
  *     `Factory[Item, M]` whose `result()` applies the provider's total constructor,
  *   - `foreach` splices the per-item body straight into the provider's loop (index-based for arrays) - the leaner
  *     replacement for the old `iterator.map(...)` mechanics,
  *   - map-ness is detected via `IsCollectionOf.asMap`. When the provider's `Pair` already IS `(K, V)` (all Scala map
  *     providers) no adaptation is emitted; provider-specific pair types (e.g. `java.util.Map.Entry`) are adapted to
  *     chimney's `Item =:= (K, V)` contract by mapping the pair iterator to tuples and wrapping the provider's
  *     `Factory[Pair, M]` in a generated `Factory[(K, V), M]`,
  *
  * KNOWN SEMANTICS (Hearth's, embraced): providers gate on `Factory`/`ClassTag` summonability at PARSE time - e.g.
  * transforming FROM `Array[T]` with an abstract `T` requires a `ClassTag[T]` even though only iteration is needed, and
  * a type matches only when its factory prerequisites (`Ordering` for sorted collections etc.) are summonable.
  */
trait TotallyBuildIterables { this: Derivation & hearth.MacroCommons & hearth.std.StdExtensions =>

  // Cross-quotes helpers - hoisted to the trait level and kept in methods with regular type parameters (the
  // cross-quotes helper-def pattern). `protected` (not `private`) - PartiallyBuildIterables' twin reuses them.

  private lazy val hearthFallbackStringType: Type[String] = Type.of[String]
  // Deliberately NOT implicit (they are only pattern-matching keys; hearth#316 - the sibling-implicit-lazy-Type
  // deadlock - is fixed since 0.4.1, so this is a style choice now, not a workaround).
  protected lazy val hearthFallbackOptionOfAnyType: Type[Option[Any]] = Type.of[Option[Any]]
  protected lazy val hearthFallbackEitherOfAnyType: Type[Either[Any, Any]] = Type.of[Either[Any, Any]]

  @scala.annotation.nowarn("msg=is never used")
  protected def iteratorToCompat[A: Type, C: Type](
      it: Expr[Iterator[A]],
      factory: Expr[Factory[A, C]]
  ): Expr[C] = {
    implicit val IteratorA: Type[Iterator[A]] = Type.of[Iterator[A]]
    implicit val FactoryAC: Type[Factory[A, C]] = Type.of[Factory[A, C]]
    Expr.quote(Expr.splice(it).to(Expr.splice(factory)))
  }

  @scala.annotation.nowarn("msg=is never used")
  protected def iterableToCompat[A: Type, C: Type](
      iterable: Expr[Iterable[A]],
      factory: Expr[Factory[A, C]]
  ): Expr[C] = {
    implicit val IterableA: Type[Iterable[A]] = Type.of[Iterable[A]]
    implicit val FactoryAC: Type[Factory[A, C]] = Type.of[Factory[A, C]]
    Expr.quote(Expr.splice(iterable).to(Expr.splice(factory)))
  }

  protected def tuple2ExprCompat[A: Type, B: Type](a: Expr[A], b: Expr[B]): Expr[(A, B)] = {
    implicit val TupleAB: Type[(A, B)] = Type.of[(A, B)]
    Expr.quote((Expr.splice(a), Expr.splice(b)))
  }

  @scala.annotation.nowarn("msg=is never used")
  protected def iterableIteratorCompat[A: Type](iterable: Expr[Iterable[A]]): Expr[Iterator[A]] = {
    implicit val IterableA: Type[Iterable[A]] = Type.of[Iterable[A]]
    implicit val IteratorA: Type[Iterator[A]] = Type.of[Iterator[A]]
    Expr.quote(Expr.splice(iterable).iterator)
  }

  @scala.annotation.nowarn("msg=is never used")
  protected def pairIteratorToTupleIteratorCompat[Pair: Type, K: Type, V: Type](
      iterator: Expr[Iterator[Pair]],
      toTuple: Expr[Pair] => Expr[(K, V)]
  ): Expr[Iterator[(K, V)]] = {
    implicit val IteratorPair: Type[Iterator[Pair]] = Type.of[Iterator[Pair]]
    implicit val TupleKV: Type[(K, V)] = Type.of[(K, V)]
    implicit val IteratorKV: Type[Iterator[(K, V)]] = Type.of[Iterator[(K, V)]]
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
    implicit val TupleKV: Type[(K, V)] = Type.of[(K, V)]
    implicit val FactoryPairM: Type[Factory[Pair, M]] = Type.of[Factory[Pair, M]]
    implicit val FactoryKVM: Type[Factory[(K, V), M]] = Type.of[Factory[(K, V), M]]
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

  // Twins of PartiallyBuildIterables' partialFactoryFromBuilderCompat/partialTupleFactoryFromPairBuilderCompat for
  // TOTAL PlainValue providers whose CtorResult != M (e.g. Kindlings' Chain accumulating into a List[E]): the
  // provider's intermediate factory accumulates, result() applies the provider's total constructor.

  @scala.annotation.nowarn("msg=is never used")
  private def totalFactoryFromBuilderCompat[Item: Type, CtorResult0: Type, M: Type](
      underlyingFactory: Expr[Factory[Item, CtorResult0]],
      buildToValue: Expr[scala.collection.mutable.Builder[Item, CtorResult0]] => Expr[M]
  ): Expr[Factory[Item, M]] = {
    implicit val FactoryItemCtorResult: Type[Factory[Item, CtorResult0]] = Type.of[Factory[Item, CtorResult0]]
    implicit val FactoryItemM: Type[Factory[Item, M]] = Type.of[Factory[Item, M]]
    implicit val BuilderItemCtorResult: Type[scala.collection.mutable.Builder[Item, CtorResult0]] =
      Type.of[scala.collection.mutable.Builder[Item, CtorResult0]]
    Expr.quote {
      new scala.collection.Factory[Item, M] {
        override def fromSpecific(it: IterableOnce[Item]): M = newBuilder.addAll(it).result()
        override def newBuilder: scala.collection.mutable.Builder[Item, M] = {
          val impl = Expr.splice(underlyingFactory).newBuilder
          new scala.collection.mutable.Builder[Item, M] {
            override def clear(): Unit = impl.clear()
            override def result(): M = Expr.splice(buildToValue(Expr.quote(impl)))
            override def addOne(elem: Item): this.type = {
              val _ = impl.addOne(elem)
              this
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  private def totalTupleFactoryFromPairBuilderCompat[Pair: Type, K: Type, V: Type, CtorResult0: Type, M: Type](
      underlyingFactory: Expr[Factory[Pair, CtorResult0]],
      fromTuple: Expr[(K, V)] => Expr[Pair],
      buildToValue: Expr[scala.collection.mutable.Builder[Pair, CtorResult0]] => Expr[M]
  ): Expr[Factory[(K, V), M]] = {
    implicit val TupleKV: Type[(K, V)] = Type.of[(K, V)]
    implicit val FactoryPairCtorResult: Type[Factory[Pair, CtorResult0]] = Type.of[Factory[Pair, CtorResult0]]
    implicit val FactoryKVM: Type[Factory[(K, V), M]] = Type.of[Factory[(K, V), M]]
    implicit val BuilderPairCtorResult: Type[scala.collection.mutable.Builder[Pair, CtorResult0]] =
      Type.of[scala.collection.mutable.Builder[Pair, CtorResult0]]
    Expr.quote {
      new scala.collection.Factory[(K, V), M] {
        override def fromSpecific(it: IterableOnce[(K, V)]): M = newBuilder.addAll(it).result()
        override def newBuilder: scala.collection.mutable.Builder[(K, V), M] = {
          val impl = Expr.splice(underlyingFactory).newBuilder
          new scala.collection.mutable.Builder[(K, V), M] {
            override def clear(): Unit = impl.clear()
            override def result(): M = Expr.splice(buildToValue(Expr.quote(impl)))
            override def addOne(elem: (K, V)): this.type = {
              val _ = impl.addOne(Expr.splice(fromTuple(Expr.quote(elem))))
              this
            }
          }
        }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  protected def tupleFirstCompat[A: Type, B: Type](tuple: Expr[(A, B)]): Expr[A] = {
    implicit val TupleAB: Type[(A, B)] = Type.of[(A, B)]
    Expr.quote(Expr.splice(tuple)._1)
  }

  @scala.annotation.nowarn("msg=is never used")
  protected def tupleSecondCompat[A: Type, B: Type](tuple: Expr[(A, B)]): Expr[B] = {
    implicit val TupleAB: Type[(A, B)] = Type.of[(A, B)]
    Expr.quote(Expr.splice(tuple)._2)
  }

  /** Something allowing us to share the logic which handles [[scala.collection.Iterable]], [[scala.Array]],
    * [[java.util.Collection]], ... and whatever we want to support.
    *
    * Tries to use [[io.scalaland.chimney.integrations.TotallyBuildIterable]] and then falls back on Hearth's
    * `IsCollection`/`IsMap` providers, if type is eligible.
    */
  abstract protected class TotallyBuildIterable[Collection, Item]
      extends TotallyOrPartiallyBuildIterable[Collection, Item] {

    def factory: Either[Expr[Factory[Item, Collection]], Expr[Factory[Item, partial.Result[Collection]]]] = Left(
      totalFactory
    )

    def totalFactory: Expr[Factory[Item, Collection]]
  }
  protected object TotallyBuildIterable {

    private type Cached[M] = Option[Existential[TotallyBuildIterable[M, *]]]
    private val totallyBulidIterableCache = new Type.Cache[Cached]
    def parse[M](implicit M: Type[M]): Option[Existential[TotallyBuildIterable[M, *]]] =
      totallyBulidIterableCache.getOrPut(M)(providedSupport[M].orElse(hearthSupport[M]))
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

            def foreach(collection: Expr[Collection])(f: Expr[Item] => Expr[Unit]): Expr[Unit] =
              iteratorForeachCompat(iterator(collection))(f)

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

    /** Hearth `IsCollection`/`IsMap` providers (built-ins AND `StandardMacroExtension`s) - see the trait's ScalaDoc for
      * the guards and their rationale.
      */
    private def hearthSupport[M: Type]: Option[Existential[TotallyBuildIterable[M, *]]] = {
      ensureStandardExtensionsLoaded()
      if (Type[M] =:= hearthFallbackStringType) None // String-as-collection excluded
      else if (Type[M] <:< hearthFallbackOptionOfAnyType || Type[M] <:< hearthFallbackEitherOfAnyType)
        None // Option/Either-as-collection excluded
      else if (IsOption.unapply(Type[M]).isDefined) None // optional semantics win (handled by OptionalValues)
      else
        IsCollection.unapply(Type[M]).flatMap { isCollection =>
          import isCollection.{Underlying as Item, value as isCollectionOf}
          (isCollectionOf.build match {
            case pv: CtorLikeOf.PlainValue[?, ?] =>
              // build is CtorLikeOf[Builder[Item, CtorResult], M] - re-establishing the erased type params.
              Some(
                pv.asInstanceOf[
                  CtorLikeOf.PlainValue[scala.collection.mutable.Builder[Item, isCollectionOf.CtorResult], M]
                ]
              )
            case _ => None // smart-constructor providers are not TOTAL - see ScalaDoc
          }) match {
            case None             => None
            case Some(plainValue) =>
              // Integrations implicits beat provider-based support - only summoned when a provider actually matched.
              if (summonPartiallyBuildIterable[M].isDefined || summonOptionalValue[M].isDefined) None
              else {
                implicit val CtorResult0: Type[isCollectionOf.CtorResult] = isCollectionOf.CtorResult
                // None = provider builds M directly, use its factory AS-IS (all Hearth built-ins);
                // Some = CtorResult != M, wrap the intermediate factory so that
                // result() applies the provider's total constructor (e.g. Kindlings' Chain builds a List[E] first).
                val buildToValue
                    : Option[Expr[scala.collection.mutable.Builder[Item, isCollectionOf.CtorResult]] => Expr[M]] =
                  if (isCollectionOf.CtorResult =:= Type[M]) None
                  else
                    Some(
                      // Identity cast - Scala 2 refuses to unify the dependent prefixes (isCollection.value vs the
                      // isCollectionOf import alias) even though the types are the same.
                      plainValue.ctor.asInstanceOf[
                        Expr[scala.collection.mutable.Builder[Item, isCollectionOf.CtorResult]] => Expr[M]
                      ]
                    )
                isCollectionOf.asMap match {
                  case Some(isMapOf) =>
                    val key = isMapOf.Key.as_??
                    val value = isMapOf.Value.as_??
                    import key.Underlying as K, value.Underlying as V
                    Some(mkHearthMapSupport[M, Item, K, V, isCollectionOf.CtorResult](isMapOf, buildToValue))
                  case None =>
                    Some(mkHearthIterableSupport[M, Item, isCollectionOf.CtorResult](isCollectionOf, buildToValue))
                }
              }
          }
        }
    }

    // Kept in separate methods (regular type parameters) - the cross-quotes helper-def pattern.

    private def mkHearthIterableSupport[M: Type, Item: Type, CtorResult0: Type](
        isCollectionOf: IsCollectionOf[M, Item],
        buildToValue: Option[Expr[scala.collection.mutable.Builder[Item, CtorResult0]] => Expr[M]]
    ): Existential[TotallyBuildIterable[M, *]] =
      Existential[TotallyBuildIterable[M, *], Item](
        new TotallyBuildIterable[M, Item] {

          def totalFactory: Expr[Factory[Item, M]] =
            buildToValue match {
              case None =>
                // CtorResult =:= M was checked by the caller - same runtime value, equivalent tree type.
                isCollectionOf.factory.asInstanceOf[Expr[Factory[Item, M]]]
              case Some(build) =>
                totalFactoryFromBuilderCompat[Item, CtorResult0, M](
                  // CtorResult0 IS isCollectionOf.CtorResult (passed by the caller) - identity cast bridging the
                  // path-dependent type to the regular type parameter.
                  isCollectionOf.factory.asInstanceOf[Expr[Factory[Item, CtorResult0]]],
                  build
                )
            }

          def iterator(collection: Expr[M]): Expr[Iterator[Item]] =
            iterableIteratorCompat(isCollectionOf.asIterable(collection))

          def foreach(collection: Expr[M])(f: Expr[Item] => Expr[Unit]): Expr[Unit] =
            isCollectionOf.foreach(collection)(f)

          def to[Collection2: Type](
              collection: Expr[M],
              factory: Expr[Factory[Item, Collection2]]
          ): Expr[Collection2] =
            iterableToCompat(isCollectionOf.asIterable(collection), factory)

          val asMap: Option[(ExistentialType, ExistentialType)] = None

          override def toString: String =
            s"support provided by Hearth IsCollection for ${Type.prettyPrint[M]}"
        }
      )

    private def mkHearthMapSupport[M: Type, Pair: Type, K: Type, V: Type, CtorResult0: Type](
        isMapOf: IsMapOf[M, Pair],
        buildToValue: Option[Expr[scala.collection.mutable.Builder[Pair, CtorResult0]] => Expr[M]]
    ): Existential[TotallyBuildIterable[M, *]] = {
      implicit val TupleKV: Type[(K, V)] = Type.of[(K, V)]
      // When the provider's Pair already IS (K, V) (all Scala map providers) no pair-to-tuple adaptation is emitted -
      // the casts below are compile-time identities.
      val pairIsTuple = Type[Pair] =:= TupleKV
      // K/V are exactly isMapOf.Key/isMapOf.Value (extracted by the caller) - the casts below are identities that
      // only bridge the path-dependent types to the regular type parameters (cross-quotes helper-def pattern).
      def toTuple(pair: Expr[Pair]): Expr[(K, V)] =
        tuple2ExprCompat(isMapOf.key(pair).asInstanceOf[Expr[K]], isMapOf.value(pair).asInstanceOf[Expr[V]])
      def fromTuple(tuple: Expr[(K, V)]): Expr[Pair] =
        isMapOf.pair(
          tupleFirstCompat(tuple).asInstanceOf[Expr[isMapOf.Key]],
          tupleSecondCompat(tuple).asInstanceOf[Expr[isMapOf.Value]]
        )
      Existential[TotallyBuildIterable[M, *], (K, V)](
        new TotallyBuildIterable[M, (K, V)] {

          def totalFactory: Expr[Factory[(K, V), M]] =
            buildToValue match {
              case None if pairIsTuple =>
                // CtorResult =:= M and Pair =:= (K, V) were checked - same runtime value, equivalent tree type.
                isMapOf.factory.asInstanceOf[Expr[Factory[(K, V), M]]]
              case None =>
                tupleFactoryFromPairFactoryCompat[Pair, K, V, M](
                  isMapOf.factory.asInstanceOf[Expr[Factory[Pair, M]]],
                  fromTuple
                )
              case Some(build) =>
                totalTupleFactoryFromPairBuilderCompat[Pair, K, V, CtorResult0, M](
                  isMapOf.factory.asInstanceOf[Expr[Factory[Pair, CtorResult0]]],
                  fromTuple,
                  build
                )
            }

          def iterator(collection: Expr[M]): Expr[Iterator[(K, V)]] =
            if (pairIsTuple)
              iterableIteratorCompat(isMapOf.asIterable(collection)).asInstanceOf[Expr[Iterator[(K, V)]]]
            else
              pairIteratorToTupleIteratorCompat[Pair, K, V](
                iterableIteratorCompat(isMapOf.asIterable(collection)),
                toTuple
              )

          def foreach(collection: Expr[M])(f: Expr[(K, V)] => Expr[Unit]): Expr[Unit] =
            if (pairIsTuple) isMapOf.foreach(collection)(f.asInstanceOf[Expr[Pair] => Expr[Unit]])
            else isMapOf.foreach(collection)(pair => f(toTuple(pair)))

          def to[Collection2: Type](
              collection: Expr[M],
              factory: Expr[Factory[(K, V), Collection2]]
          ): Expr[Collection2] =
            if (pairIsTuple)
              iterableToCompat(
                isMapOf.asIterable(collection).asInstanceOf[Expr[Iterable[(K, V)]]],
                factory
              )
            else iteratorToCompat(iterator(collection), factory)

          val asMap: Option[(ExistentialType, ExistentialType)] = Some(Type[K].as_?? -> Type[V].as_??)

          override def toString: String =
            s"support provided by Hearth IsMap for ${Type.prettyPrint[M]}"
        }
      )
    }
  }
}
