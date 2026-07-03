package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.Factory

/** EXTENSION FALLBACK: [[TotallyBuildIterable.parse]] has a THIRD alternative consulting Hearth's
  * `IsCollection`/`IsMap` providers (built-ins AND ServiceLoader-registered `StandardMacroExtension`s) - the mechanism
  * that lets chimney-java-collections (and Kindlings-style collection extensions) plug into the
  * MapToMap/IterableToIterable rules without implicits. Precedence and guards (order matters):
  *   - it ranks BELOW `providedSupport` (the [[io.scalaland.chimney.integrations.TotallyBuildIterable]] implicit) and
  *     below `buildInSupport` ([[IterableOrArray]]'s hardcoded Map/Iterable/Iterator/Array/IArray shapes) - the
  *     fallback is only consulted for types the hardcoded support REJECTED,
  *   - `String` is filtered out: Hearth's built-in `IsCollectionProviderForString` would otherwise turn every String
  *     into a `Char` collection and change rule dispatch for String-typed fields,
  *   - `scala.Option`/`scala.Either` shapes are filtered out: Hearth's `IsCollectionProviderForScalaOption` models
  *     Option as an at-most-1-element collection - chimney handles optionals via [[OptionalValues]] and eithers via the
  *     EitherToEither rule and must keep doing so (Option-to-List etc. staying unsupported is covered behavior); the
  *     Either filter pins the semantics against third-party providers,
  *   - any type that Hearth's `IsOption` matches is filtered out (e.g. `java.util.Optional`, which Hearth's built-ins
  *     model BOTH as an option and as a collection): optional semantics win, mirroring the OptionToOption-before-
  *     IterableToIterable rule order; such types go through [[OptionalValues]]' own fallback instead,
  *   - only "total-shaped" providers are accepted: `build` must be a `CtorLikeOf.PlainValue`. Smart-constructor
  *     providers (e.g. Kindlings' NonEmptyList with `EitherStringOrValue`) cannot be a TOTAL factory - they surface
  *     through [[PartiallyBuildIterables]]' twin fallback instead. When `CtorResult =:= M` (all Hearth built-in
  *     scala/java collection providers) the provider's factory is used AS-IS; when `CtorResult != M` (e.g. Kindlings'
  *     `Chain`, which accumulates into a `List[E]` and converts at the end) the intermediate
  *     `Factory[Item, CtorResult]` is wrapped into a generated `Factory[Item, M]` whose `result()` applies the
  *     provider's total constructor,
  *   - `java.util.EnumSet`/`java.util.EnumMap` targets get their PROVIDER factory expr replaced with a Chimney-built
  *     equivalent - a Hearth 0.4.0 provider bug workaround, see [[JavaCollectionsPlatformCompat]],
  *   - it is SKIPPED when a `PartiallyBuildIterable`/`OptionalValue` implicit exists for the type: integrations
  *     implicits must beat extension providers ([[TotallyOrPartiallyBuildIterable]] tries Totally BEFORE Partially, and
  *     MapToMap/IterableToIterable run before ToOption, so without these guards an extension match would shadow the
  *     implicit),
  *   - map-ness is detected via `IsCollectionOf.asMap` (Hearth's `IsMap` is exactly `IsCollection` + `asMap`). Hearth
  *     map providers use provider-specific `Pair` types (e.g. `java.util.Map.Entry`), while chimney's Map support
  *     requires `Item =:= (K, V)` at codegen level - the fallback adapts by mapping the pair iterator to tuples and
  *     wrapping the provider's `Factory[Pair, M]` in a generated `Factory[(K, V), M]` (`pair(k, v)` inserted in
  *     `addOne`).
  * KNOWN PITFALL (accepted): Hearth providers gate on `Factory`/`ClassTag` summonability at PARSE time, while chimney's
  * hardcoded shapes only summon when the factory is actually USED. Invisible for hardcoded-matched types (they never
  * reach the fallback); for extension-provided types parse-time gating is the provider author's contract. On the JVM,
  * Hearth's built-in `java.util.*` collection providers make Java collections derivable WITHOUT the
  * chimney-java-collections import; with the import, the module's implicits keep winning via `providedSupport`. (On
  * JS/Native Hearth ships no Java providers - the fallback never matches there.)
  */
trait TotallyBuildIterables { this: Derivation & hearth.MacroCommons & hearth.std.StdExtensions =>

  // Cross-quotes helpers for the Hearth-provider fallback - hoisted to the trait level and kept in methods with
  // regular type parameters (the cross-quotes helper-def pattern).
  // `protected` (not `private`) - PartiallyBuildIterables' twin fallback reuses them through the cake.

  private lazy val hearthFallbackStringType: Type[String] = Type.of[String]
  // hearth#316: NOT implicit - implicit Type vals with cross-quoted initializers deadlock lazy-val init at macro
  // runtime on Scala 3.
  protected lazy val hearthFallbackNullType: Type[Null] = Type.of[Null]
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

    private lazy val MapCtor: Type.Ctor2[scala.collection.Map] = Type.Ctor2.of[scala.collection.Map]

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
      // HEARTH GOTCHA (hearth#319): bottom types conform to everything (`Null <:< java.util.Optional[?]` etc.),
      // so `<:<`-matching built-in providers match `Null`/`Nothing` and then CRASH eagerly while building their exprs
      // (upcast assertion at parse time). Never consult providers for bottom types.
      if (Type[M] <:< hearthFallbackNullType) None
      else if (Type[M] =:= hearthFallbackStringType) None // String-as-collection excluded
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
              // Integrations implicits beat extension providers - only summoned when a provider actually matched.
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
    ): Existential[TotallyBuildIterable[M, *]] = {
      // HEARTH 0.4.0 BUG WORKAROUND (hearth#324, detected at parse level, never inside splices): the provider's EnumSet branch
      // embeds a class token (factory) and inline-quoted trees (asIterable) that do not survive Chimney's Scala 2
      // re-typecheck - replace both exprs with Chimney-built equivalents (see JavaCollectionsPlatformCompat).
      val isEnumSet = isJavaEnumSetCompat[M]
      Existential[TotallyBuildIterable[M, *], Item](
        new TotallyBuildIterable[M, Item] {

          def totalFactory: Expr[Factory[Item, M]] =
            if (isEnumSet) javaEnumSetFactoryCompat[Item, M](classOfExprCompat[Item])
            else
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
            if (isEnumSet) javaCollectionIteratorCompat[Item, M](collection)
            else iterableIteratorCompat(isCollectionOf.asIterable(collection))

          def to[Collection2: Type](
              collection: Expr[M],
              factory: Expr[Factory[Item, Collection2]]
          ): Expr[Collection2] = iteratorToCompat(iterator(collection), factory)

          val asMap: Option[(ExistentialType, ExistentialType)] = None

          override def toString: String =
            s"support provided by Hearth extension IsCollection for ${Type.prettyPrint[M]}"
        }
      )
    }

    private def mkHearthMapSupport[M: Type, Pair: Type, K: Type, V: Type, CtorResult0: Type](
        isMapOf: IsMapOf[M, Pair],
        buildToValue: Option[Expr[scala.collection.mutable.Builder[Pair, CtorResult0]] => Expr[M]]
    ): Existential[TotallyBuildIterable[M, *]] = {
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
      // HEARTH 0.4.0 BUG WORKAROUND (hearth#324, detected at parse level, never inside splices): the provider's EnumMap branch
      // embeds a class token (factory) and inline-quoted trees (asIterable/key/value) that do not survive Chimney's
      // Scala 2 re-typecheck - replace both the (tuple-level) factory and the iterator with Chimney-built equivalents
      // (see JavaCollectionsPlatformCompat for the full story).
      val isEnumMap = isJavaEnumMapCompat[M]
      Existential[TotallyBuildIterable[M, *], (K, V)](
        new TotallyBuildIterable[M, (K, V)] {

          def totalFactory: Expr[Factory[(K, V), M]] =
            if (isEnumMap) javaEnumMapFactoryCompat[K, V, M](classOfExprCompat[K])
            else
              buildToValue match {
                case None =>
                  tupleFactoryFromPairFactoryCompat[Pair, K, V, M](
                    // CtorResult =:= M was checked by the caller - same runtime value, equivalent tree type.
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
            if (isEnumMap) javaMapIteratorCompat[K, V, M](collection)
            else
              pairIteratorToTupleIteratorCompat[Pair, K, V](
                iterableIteratorCompat(isMapOf.asIterable(collection)),
                toTuple
              )

          def to[Collection2: Type](
              collection: Expr[M],
              factory: Expr[Factory[(K, V), Collection2]]
          ): Expr[Collection2] = iteratorToCompat(iterator(collection), factory)

          val asMap: Option[(ExistentialType, ExistentialType)] = Some(Type[K].as_?? -> Type[V].as_??)

          override def toString: String =
            s"support provided by Hearth extension IsMap for ${Type.prettyPrint[M]}"
        }
      )
    }
  }
}
