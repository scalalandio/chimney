package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.Factory

/** EXTENSION FALLBACK: [[PartiallyBuildIterable.parse]] has a SECOND alternative consulting Hearth's
  * `IsCollection`/`IsMap` providers whose `build` is a SMART CONSTRUCTOR (one of the four `CtorLikeOf.Either*OrValue`
  * shapes - e.g. Kindlings cats-integration's `NonEmptyList` provider builds a `List[E]` and then returns
  * `Left("Cannot create NonEmptyList from empty collection")` for an empty one). It is the twin of
  * [[TotallyBuildIterables]]' fallback, which accepts only `CtorLikeOf.PlainValue` providers - the two are disjoint by
  * construction, and rules try Totally BEFORE Partially ([[TotallyOrPartiallyBuildIterable]]). Precedence and guards
  * mirror the total fallback (see its ScalaDoc for the full rationale): ranks below `providedSupport` (the
  * [[io.scalaland.chimney.integrations.PartiallyBuildIterable]] implicit); `String` / `Option`/`Either` shapes /
  * `IsOption` matches are filtered out; it is SKIPPED when a `TotallyBuildIterable`/`OptionalValue` implicit exists for
  * the type (integrations implicits beat extension providers); map-ness detected via `asMap` with the same
  * pair-to-tuple adaptation.
  *
  * The generated `partialFactory` delegates to the provider's `factory` (an intermediate `Factory[Item, CtorResult]`,
  * e.g. `Factory[E, List[E]]` for NonEmptyList) for accumulation and applies the provider's smart constructor in
  * `result()`, adapting its `Either` error channel onto `partial.Result` (see
  * [[io.scalaland.chimney.internal.compiletime.CtorLikeExprs]] for the exact mapping). From the rules' perspective the
  * result behaves EXACTLY like an `io.scalaland.chimney.integrations.PartiallyBuildIterable` implicit:
  * PartialTransformer works (construction failure surfaces as `partial.Result.Errors` at the collection's path), Total
  * derivation fails with the usual "Only PartiallyBuildIterable available ... in total context" -> "Chimney can't
  * derive ..." error.
  */
trait PartiallyBuildIterables { this: Derivation & hearth.MacroCommons & hearth.std.StdExtensions =>

  private lazy val hearthPartialFallbackStringType: Type[String] = Type.of[String]

  // Cross-quotes helpers for the Hearth-provider fallback - hoisted to the trait level and kept in methods with
  // regular type parameters (the cross-quotes helper-def pattern).

  @scala.annotation.nowarn("msg=is never used")
  private def partialFactoryFromBuilderCompat[Item: Type, CtorResult0: Type, M: Type](
      underlyingFactory: Expr[Factory[Item, CtorResult0]],
      buildToPartial: Expr[scala.collection.mutable.Builder[Item, CtorResult0]] => Expr[partial.Result[M]]
  ): Expr[Factory[Item, partial.Result[M]]] = {
    implicit val PartialResultM: Type[partial.Result[M]] = ChimneyType.PartialResult[M]
    implicit val FactoryItemCtorResult: Type[Factory[Item, CtorResult0]] = Type.of[Factory[Item, CtorResult0]]
    implicit val FactoryItemPartialM: Type[Factory[Item, partial.Result[M]]] =
      Type.of[Factory[Item, partial.Result[M]]]
    implicit val BuilderItemCtorResult: Type[scala.collection.mutable.Builder[Item, CtorResult0]] =
      Type.of[scala.collection.mutable.Builder[Item, CtorResult0]]
    Expr.quote {
      new scala.collection.Factory[Item, partial.Result[M]] {
        override def fromSpecific(it: IterableOnce[Item]): partial.Result[M] = newBuilder.addAll(it).result()
        override def newBuilder: scala.collection.mutable.Builder[Item, partial.Result[M]] = {
          val impl = Expr.splice(underlyingFactory).newBuilder
          new scala.collection.mutable.Builder[Item, partial.Result[M]] {
            override def clear(): Unit = impl.clear()
            override def result(): partial.Result[M] = Expr.splice(buildToPartial(Expr.quote(impl)))
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
  private def partialTupleFactoryFromPairBuilderCompat[Pair: Type, K: Type, V: Type, CtorResult0: Type, M: Type](
      underlyingFactory: Expr[Factory[Pair, CtorResult0]],
      fromTuple: Expr[(K, V)] => Expr[Pair],
      buildToPartial: Expr[scala.collection.mutable.Builder[Pair, CtorResult0]] => Expr[partial.Result[M]]
  ): Expr[Factory[(K, V), partial.Result[M]]] = {
    implicit val TupleKV: Type[(K, V)] = Type.of[(K, V)]
    implicit val PartialResultM: Type[partial.Result[M]] = ChimneyType.PartialResult[M]
    implicit val FactoryPairCtorResult: Type[Factory[Pair, CtorResult0]] = Type.of[Factory[Pair, CtorResult0]]
    implicit val FactoryKVPartialM: Type[Factory[(K, V), partial.Result[M]]] =
      Type.of[Factory[(K, V), partial.Result[M]]]
    implicit val BuilderPairCtorResult: Type[scala.collection.mutable.Builder[Pair, CtorResult0]] =
      Type.of[scala.collection.mutable.Builder[Pair, CtorResult0]]
    Expr.quote {
      new scala.collection.Factory[(K, V), partial.Result[M]] {
        override def fromSpecific(it: IterableOnce[(K, V)]): partial.Result[M] = newBuilder.addAll(it).result()
        override def newBuilder: scala.collection.mutable.Builder[(K, V), partial.Result[M]] = {
          val impl = Expr.splice(underlyingFactory).newBuilder
          new scala.collection.mutable.Builder[(K, V), partial.Result[M]] {
            override def clear(): Unit = impl.clear()
            override def result(): partial.Result[M] = Expr.splice(buildToPartial(Expr.quote(impl)))
            override def addOne(elem: (K, V)): this.type = {
              val _ = impl.addOne(Expr.splice(fromTuple(Expr.quote(elem))))
              this
            }
          }
        }
      }
    }
  }

  /** Something allowing us to share the logic which handles NonEmptyList, NonEmptySet, ... and whatever we want to
    * support.
    *
    * Tries to use [[io.scalaland.chimney.integrations.PartiallyBuildIterable]], if type is eligible.
    */
  abstract protected class PartiallyBuildIterable[Collection, Item]
      extends TotallyOrPartiallyBuildIterable[Collection, Item] {

    def factory: Either[Expr[Factory[Item, Collection]], Expr[Factory[Item, partial.Result[Collection]]]] = Right(
      partialFactory
    )

    def partialFactory: Expr[Factory[Item, partial.Result[Collection]]]

    def iterator(collection: Expr[Collection]): Expr[Iterator[Item]]

    def to[Collection2: Type](
        collection: Expr[Collection],
        factory: Expr[Factory[Item, Collection2]]
    ): Expr[Collection2]

    val asMap: Option[(ExistentialType, ExistentialType)]
  }
  protected object PartiallyBuildIterable {

    private type Cached[M] = Option[Existential[PartiallyBuildIterable[M, *]]]
    private val partiallyBulidIterableCache = new Type.Cache[Cached]
    def parse[M](implicit M: Type[M]): Option[Existential[PartiallyBuildIterable[M, *]]] =
      partiallyBulidIterableCache.getOrPut(M)(providedSupport[M].orElse(hearthProviderSupport[M]))
    def unapply[M](M: Type[M]): Option[Existential[PartiallyBuildIterable[M, *]]] = parse(using M)

    private def providedSupport[Collection: Type]: Option[Existential[PartiallyBuildIterable[Collection, *]]] =
      summonPartiallyBuildIterable[Collection].map { partiallyBuildIterable =>
        import partiallyBuildIterable.{Underlying as Item, value as partiallyBuildIterableExpr}
        Existential[PartiallyBuildIterable[Collection, *], Item](
          new PartiallyBuildIterable[Collection, Item] {

            def partialFactory: Expr[Factory[Item, partial.Result[Collection]]] =
              partiallyBuildIterableExpr.partialFactory

            def iterator(collection: Expr[Collection]): Expr[Iterator[Item]] =
              partiallyBuildIterableExpr.iterator(collection)

            def foreach(collection: Expr[Collection])(f: Expr[Item] => Expr[Unit]): Expr[Unit] =
              iteratorForeachCompat(iterator(collection))(f)

            def to[Collection2: Type](
                collection: Expr[Collection],
                factory: Expr[Factory[Item, Collection2]]
            ): Expr[Collection2] = partiallyBuildIterableExpr.to(collection, factory)

            val asMap: Option[(ExistentialType, ExistentialType)] = partiallyBuildIterableExpr.tpe match {
              case ChimneyType.PartiallyBuildMap(_, key, value) => Some(key -> value)
              case _                                            => None
            }

            override def toString: String = s"support provided by ${Expr.prettyPrint(partiallyBuildIterableExpr)}"
          }
        )
      }

    /** Fallback consulting Hearth `IsCollection`/`IsMap` providers with SMART-CONSTRUCTOR `build` shapes - see the
      * trait's ScalaDoc for the full list of guards and their rationale.
      */
    private def hearthProviderSupport[M: Type]: Option[Existential[PartiallyBuildIterable[M, *]]] = {
      ensureStandardExtensionsLoaded()
      // Guards mirror TotallyBuildIterables.hearthSupport.
      if (Type[M] =:= hearthPartialFallbackStringType) None // String-as-collection excluded
      else if (Type[M] <:< hearthFallbackOptionOfAnyType || Type[M] <:< hearthFallbackEitherOfAnyType)
        None // Option/Either-as-collection excluded
      else if (IsOption.unapply(Type[M]).isDefined) None // optional semantics win (handled by OptionalValues)
      else
        IsCollection.unapply(Type[M]).flatMap { isCollection =>
          import isCollection.{Underlying as Item, value as isCollectionOf}
          // The CtorLikeOf shape is inspected once, at parse level: None for PlainValue (the TOTAL fallback's
          // territory), Some(exprFn) for the four smart-constructor shapes.
          ctorLikeToPartialResultExpr[scala.collection.mutable.Builder[Item, isCollectionOf.CtorResult], M](
            isCollectionOf.build
          ) match {
            case None                 => None
            case Some(buildToPartial) =>
              // Integrations implicits beat extension providers - only summoned when a provider actually matched.
              if (summonTotallyBuildIterable[M].isDefined || summonOptionalValue[M].isDefined) None
              else {
                implicit val CtorResult0: Type[isCollectionOf.CtorResult] = isCollectionOf.CtorResult
                isCollectionOf.asMap match {
                  case Some(isMapOf) =>
                    val key = isMapOf.Key.as_??
                    val value = isMapOf.Value.as_??
                    import key.Underlying as K, value.Underlying as V
                    Some(mkHearthPartialMapSupport[M, Item, K, V, isCollectionOf.CtorResult](isMapOf, buildToPartial))
                  case None =>
                    Some(
                      mkHearthPartialIterableSupport[M, Item, isCollectionOf.CtorResult](isCollectionOf, buildToPartial)
                    )
                }
              }
          }
        }
    }

    // Kept in separate methods (regular type parameters) - the cross-quotes helper-def pattern.

    private def mkHearthPartialIterableSupport[M: Type, Item: Type, CtorResult0: Type](
        isCollectionOf: IsCollectionOf[M, Item],
        buildToPartial: Expr[scala.collection.mutable.Builder[Item, CtorResult0]] => Expr[partial.Result[M]]
    ): Existential[PartiallyBuildIterable[M, *]] =
      Existential[PartiallyBuildIterable[M, *], Item](
        new PartiallyBuildIterable[M, Item] {

          def partialFactory: Expr[Factory[Item, partial.Result[M]]] =
            partialFactoryFromBuilderCompat[Item, CtorResult0, M](
              // CtorResult0 IS isCollectionOf.CtorResult (passed by the caller) - identity cast bridging the
              // path-dependent type to the regular type parameter.
              isCollectionOf.factory.asInstanceOf[Expr[Factory[Item, CtorResult0]]],
              buildToPartial
            )

          def iterator(collection: Expr[M]): Expr[Iterator[Item]] =
            iterableIteratorCompat(isCollectionOf.asIterable(collection))

          def foreach(collection: Expr[M])(f: Expr[Item] => Expr[Unit]): Expr[Unit] =
            isCollectionOf.foreach(collection)(f)

          def to[Collection2: Type](
              collection: Expr[M],
              factory: Expr[Factory[Item, Collection2]]
          ): Expr[Collection2] = iteratorToCompat(iterator(collection), factory)

          val asMap: Option[(ExistentialType, ExistentialType)] = None

          override def toString: String =
            s"support provided by Hearth extension IsCollection (smart constructor) for ${Type.prettyPrint[M]}"
        }
      )

    private def mkHearthPartialMapSupport[M: Type, Pair: Type, K: Type, V: Type, CtorResult0: Type](
        isMapOf: IsMapOf[M, Pair],
        buildToPartial: Expr[scala.collection.mutable.Builder[Pair, CtorResult0]] => Expr[partial.Result[M]]
    ): Existential[PartiallyBuildIterable[M, *]] = {
      implicit val TupleKV: Type[(K, V)] = Type.of[(K, V)]
      // K/V are exactly isMapOf.Key/isMapOf.Value (extracted by the caller) - the casts below are identities that
      // only bridge the path-dependent types to the regular type parameters (cross-quotes helper-def pattern).
      def toTuple(pair: Expr[Pair]): Expr[(K, V)] =
        tuple2ExprCompat(isMapOf.key(pair).asInstanceOf[Expr[K]], isMapOf.value(pair).asInstanceOf[Expr[V]])
      def fromTuple(tuple: Expr[(K, V)]): Expr[Pair] =
        isMapOf.pair(
          tupleFirstCompat(tuple).asInstanceOf[Expr[isMapOf.Key]],
          tupleSecondCompat(tuple).asInstanceOf[Expr[isMapOf.Value]]
        )
      Existential[PartiallyBuildIterable[M, *], (K, V)](
        new PartiallyBuildIterable[M, (K, V)] {

          def partialFactory: Expr[Factory[(K, V), partial.Result[M]]] =
            partialTupleFactoryFromPairBuilderCompat[Pair, K, V, CtorResult0, M](
              isMapOf.factory.asInstanceOf[Expr[Factory[Pair, CtorResult0]]],
              fromTuple,
              buildToPartial
            )

          def iterator(collection: Expr[M]): Expr[Iterator[(K, V)]] =
            pairIteratorToTupleIteratorCompat[Pair, K, V](
              iterableIteratorCompat(isMapOf.asIterable(collection)),
              toTuple
            )

          def foreach(collection: Expr[M])(f: Expr[(K, V)] => Expr[Unit]): Expr[Unit] =
            isMapOf.foreach(collection)(pair => f(toTuple(pair)))

          def to[Collection2: Type](
              collection: Expr[M],
              factory: Expr[Factory[(K, V), Collection2]]
          ): Expr[Collection2] = iteratorToCompat(iterator(collection), factory)

          val asMap: Option[(ExistentialType, ExistentialType)] = Some(Type[K].as_?? -> Type[V].as_??)

          override def toString: String =
            s"support provided by Hearth extension IsMap (smart constructor) for ${Type.prettyPrint[M]}"
        }
      )
    }
  }
}
