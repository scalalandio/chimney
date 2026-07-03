package io.scalaland.chimney.internal.compiletime

import scala.collection.Factory

/** JVM implementation of the `java.util.EnumSet`/`java.util.EnumMap` compat for the Hearth-provider fallback layer (see
  * the JS/Native sources of the same name for the no-op stubs - Hearth ships its Java providers only on the JVM, so the
  * fallback can never match these types there).
  *
  * HEARTH 0.4.0 BUG WORKAROUND (report upstream): `IsCollectionProviderForJavaCollection`'s EnumSet branch and
  * `IsCollectionProviderForJavaMap`'s EnumMap branch embed the enum's `java.lang.Class` token into their `factory`
  * exprs through `Expr.ClassExprCodec`. On Scala 2 that codec lifts the token via a quasiquote
  * (`q"scala.Predef.classOf[${Type[A]}]"`) built inside HEARTH's own compilation unit - after Chimney's re-typecheck
  * (`c.untypecheck` + re-typecheck of the whole derived expression) the type splice prints as a path-dependent
  * reference into the provider's `parse` method (`item.Underlying`-like) and scalac fails with "not found: value item".
  * Only the CLASS TOKEN inside `factory` is poisoned - `asIterable`/`key`/`value`/`pair`/`build` come from Hearth's
  * regular cross-quotes (helper-method pattern) and re-typecheck fine.
  *
  * The fix (per the migration doctrine: never exclude a provider - fix its bug in compat): the fallback layer REPLACES
  * the provider's `factory` expr for EnumSet/EnumMap-shaped targets with a Chimney-built equivalent whose class token
  * is a plain class LITERAL of the concrete enum type ([[MacroCommonsCompat.classOfExprCompat]] - prints as
  * `classOf[fqcn.Enum]` and re-typechecks on both Scala versions). The generated factories mirror Hearth's own
  * (reflective `EnumSet.noneOf` / `new EnumMap[Nothing, V](cast)` tricks bypassing the `E <: Enum[E]` bounds that an
  * unbounded macro type parameter cannot satisfy). Applied on BOTH Scala versions, so the single replacement code path
  * is exercised by both test suites.
  *
  * RELATED HEARTH 0.4.0 LIMITATIONS discovered while fixing this (report upstream, pinned in HearthStdJavaTypesSpec):
  *   - the providers' EnumSet/EnumMap branches gate on `Type.classOfType[Item/Key]` (a macro-time `Class.forName` of
  *     the ENUM class), so they only match enums that are ALREADY COMPILED (dependency/JDK enums) - for an enum
  *     compiled in the same run they skip. Chimney's class-literal replacement would not need the runtime class at all,
  *     but it can only replace a factory of a provider that MATCHED,
  *   - when the EnumSet branch skips, `IsCollectionProviderForJavaIterable` still matches (EnumSet <:
  *     `java.lang.Iterable`), but its factory does `iterable.upcast[A]` which ASSERTS at expr-build time for any proper
  *     subtype of `java.lang.Iterable` - the replacement below is what makes same-unit-enum EnumSet targets work at
  *     all. EnumMap has no such second provider, so same-unit-enum EnumMap targets stay unsupported (the regular
  *     "Chimney can't derive" error).
  */
private[compiletime] trait JavaCollectionsPlatformCompat {
  this: ChimneyDefinitions & hearth.MacroCommons =>

  /** True when `M`'s runtime class is `java.util.EnumSet` (or a subclass - not nameable in user code, but this mirrors
    * the `isAssignableFrom` check Hearth's provider used to select its EnumSet branch). Name-based so the engine
    * sources stay platform-portable.
    */
  protected def isJavaEnumSetCompat[M: Type]: Boolean = classOfTypeHasSuperclass[M]("java.util.EnumSet")

  /** True when `M`'s runtime class is `java.util.EnumMap` (or a subclass) - see [[isJavaEnumSetCompat]]. */
  protected def isJavaEnumMapCompat[M: Type]: Boolean = classOfTypeHasSuperclass[M]("java.util.EnumMap")

  private def classOfTypeHasSuperclass[M: Type](name: String): Boolean =
    Type.classOfType[M].exists { cls =>
      Iterator
        .iterate[java.lang.Class[?]](cls)(_.getSuperclass)
        .takeWhile(_ != null)
        .exists(_.getName == name)
    }

  /** `Iterator[Item]` for an `M <: java.util.Collection[Item]` source - replacement for the EnumSet provider branch's
    * poisoned `asIterable` (its quote is written INLINE in the provider's `parse` with the existential-imported `Item`,
    * so its generated Scala 2 tree references the provider's local `item` value - same "not found: value item" family
    * as the class token; the other collection branches route their quotes through helper methods with regular type
    * parameters and are fine).
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def javaCollectionIteratorCompat[Item: Type, M: Type](collection: Expr[M]): Expr[Iterator[Item]] = {
    implicit val IteratorItem: Type[Iterator[Item]] = ScalaType.Iterator[Item]
    Expr.quote {
      scala.jdk.javaapi.CollectionConverters
        .asScala(Expr.splice(collection).asInstanceOf[java.util.Collection[Item]].iterator())
    }
  }

  /** `Iterator[(K, V)]` for an `M <: java.util.Map[K, V]` source - replacement for the EnumMap provider branch's
    * poisoned `asIterable`/`key`/`value` (see [[javaCollectionIteratorCompat]] - the EnumMap branch's quotes suffer the
    * same inline-quote problem). Bypasses the provider's `java.util.Map.Entry` pair type entirely.
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def javaMapIteratorCompat[K: Type, V: Type, M: Type](collection: Expr[M]): Expr[Iterator[(K, V)]] = {
    implicit val TupleKV: Type[(K, V)] = ScalaType.Tuple2[K, V]
    implicit val IteratorKV: Type[Iterator[(K, V)]] = ScalaType.Iterator[(K, V)]
    Expr.quote {
      scala.jdk.javaapi.CollectionConverters
        .asScala(Expr.splice(collection).asInstanceOf[java.util.Map[K, V]].entrySet().iterator())
        .map { (entry: java.util.Map.Entry[K, V]) =>
          (entry.getKey(), entry.getValue())
        }
    }
  }

  /** `Factory[Item, M]` for an `M =:= java.util.EnumSet[Item]` target - replacement for the provider's poisoned factory
    * (see the trait ScalaDoc). Mirrors Hearth's generated code shape.
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def javaEnumSetFactoryCompat[Item: Type, M: Type](
      itemClass: Expr[java.lang.Class[Item]]
  ): Expr[Factory[Item, M]] = {
    implicit val FactoryItemM: Type[Factory[Item, M]] = ScalaType.Factory[Item, M]
    Expr.quote {
      new scala.collection.Factory[Item, M] {
        override def fromSpecific(it: IterableOnce[Item]): M = newBuilder.addAll(it).result()
        override def newBuilder: scala.collection.mutable.Builder[Item, M] =
          new scala.collection.mutable.Builder[Item, M] {
            private val impl: java.util.EnumSet[?] = {
              // Bypass EnumSet.noneOf's `E <: Enum[E]` bound (not satisfiable by an unbounded Item) via reflection -
              // the same trick as Hearth's own provider, except with `Class.forName` instead of `classOf[Generic[?]]`
              // literals: class literals of PARAMETERIZED types print RAW (`classOf[java.util.EnumSet]`) after
              // Chimney's Scala 2 re-typecheck and fail with "class EnumSet takes type parameters".
              val cls: java.lang.Class[?] = Expr.splice(itemClass)
              java.lang.Class
                .forName("java.util.EnumSet")
                .getMethod("noneOf", java.lang.Class.forName("java.lang.Class"))
                .invoke(null, cls)
                .asInstanceOf[java.util.EnumSet[?]]
            }
            override def clear(): Unit = impl.clear()
            override def result(): M = impl.asInstanceOf[M]
            override def addOne(elem: Item): this.type = {
              val _ = impl.asInstanceOf[java.util.Set[AnyRef]].add(elem.asInstanceOf[AnyRef])
              this
            }
          }
      }
    }
  }

  /** `Factory[(K, V), M]` for an `M =:= java.util.EnumMap[K, V]` target - replacement for the provider's poisoned pair
    * factory. Built directly at the TUPLE level (the engine's Map contract), so the provider's `java.util.Map.Entry`
    * pair type is not involved at all on the construction side.
    */
  @scala.annotation.nowarn("msg=is never used")
  protected def javaEnumMapFactoryCompat[K: Type, V: Type, M: Type](
      keyClass: Expr[java.lang.Class[K]]
  ): Expr[Factory[(K, V), M]] = {
    implicit val TupleKV: Type[(K, V)] = ScalaType.Tuple2[K, V]
    implicit val FactoryKVM: Type[Factory[(K, V), M]] = ScalaType.Factory[(K, V), M]
    Expr.quote {
      new scala.collection.Factory[(K, V), M] {
        override def fromSpecific(it: IterableOnce[(K, V)]): M = newBuilder.addAll(it).result()
        override def newBuilder: scala.collection.mutable.Builder[(K, V), M] =
          new scala.collection.mutable.Builder[(K, V), M] {
            // Bypass EnumMap's `K <: Enum[K]` bound through Nothing (Nothing <: Enum[Nothing]) - the same trick as
            // Hearth's own provider.
            private val impl: java.util.Map[K, V] = new java.util.EnumMap[Nothing, V](
              Expr.splice(keyClass).asInstanceOf[java.lang.Class[Nothing]]
            ).asInstanceOf[java.util.Map[K, V]]
            override def clear(): Unit = impl.clear()
            override def result(): M = impl.asInstanceOf[M]
            override def addOne(elem: (K, V)): this.type = {
              val _ = impl.put(elem._1, elem._2)
              this
            }
          }
      }
    }
  }
}
