package io.scalaland.chimney.javacollections

import java.util as ju
import io.scalaland.chimney.ChimneySpec

class JavaFactorySpec extends ChimneySpec {

  // TODO: Iterator

  // TODO: Enumeration

  test("java.util.Collection instances should be resolved for both concrete and abstract types for non-Map types") {
    def convertAndVerifyStable[A, CC[A1] <: ju.Collection[A1]](values: A*)(implicit
        factory: JavaFactory[A, CC[A]]
    ): Unit = {
      import scala.jdk.CollectionConverters.*
      factory.fromSpecific(values).asScala.toSeq ==> values.toSeq
    }

    def convertAndVerifyUnstable[A: Ordering, CC[A1] <: ju.Collection[A1]](values: A*)(implicit
        factory: JavaFactory[A, CC[A]]
    ): Unit = {
      import scala.jdk.CollectionConverters.*
      factory.fromSpecific(values).asScala.toVector.sorted ==> values.iterator.toVector.sorted
    }

    def convertAndVerifySorted[A: Ordering, CC[A1] <: ju.Collection[A1]](values: A*)(implicit
        factory: JavaFactory[A, CC[A]]
    ): Unit = {
      import scala.jdk.CollectionConverters.*
      factory.fromSpecific(values).asScala.toVector ==> values.iterator.toVector.sorted
    }

    convertAndVerifyUnstable[String, ju.Collection]("d", "c", "b", "a")
    convertAndVerifyUnstable[String, ju.AbstractCollection]("d", "c", "b", "a")

    convertAndVerifyStable[String, ju.List]("d", "c", "b", "a")
    convertAndVerifyStable[String, ju.AbstractList]("d", "c", "b", "a")
    convertAndVerifyStable[String, ju.AbstractSequentialList]("d", "c", "b", "a")
    convertAndVerifyStable[String, ju.ArrayList]("d", "c", "b", "a")
    convertAndVerifyStable[String, ju.LinkedList]("d", "c", "b", "a")
    convertAndVerifyStable[String, ju.Vector]("d", "c", "b", "a")
    convertAndVerifyStable[String, ju.Stack]("d", "c", "b", "a")

    convertAndVerifyStable[String, ju.Deque]("d", "c", "b", "a")
    convertAndVerifyStable[String, ju.ArrayDeque]("d", "c", "b", "a")

    convertAndVerifySorted[String, ju.Queue]("d", "c", "b", "a")
    convertAndVerifySorted[String, ju.AbstractQueue]("d", "c", "b", "a")
    convertAndVerifySorted[String, ju.PriorityQueue]("d", "c", "b", "a")

    convertAndVerifyUnstable[String, ju.Set]("d", "c", "b", "a")
    convertAndVerifyUnstable[String, ju.AbstractSet]("d", "c", "b", "a")
    convertAndVerifySorted[String, ju.SortedSet]("d", "c", "b", "a")
    convertAndVerifySorted[String, ju.NavigableSet]("d", "c", "b", "a")
    convertAndVerifyUnstable[String, ju.HashSet]("d", "c", "b", "a")
    convertAndVerifyStable[String, ju.LinkedHashSet]("d", "c", "b", "a")
    convertAndVerifySorted[String, ju.TreeSet]("d", "c", "b", "a")

    // TODO: EnumSet
  }

  test(
    "java.util.Collection instances should be resolved for both concrete and abstract types for java.util.Dictionary types"
  ) {
    def convertAndVerifyUnstable[K: Ordering, V, CC[K1, V1] <: ju.Dictionary[K1, V1]](values: (K, V)*)(implicit
        factory: JavaFactory[(K, V), CC[K, V]]
    ): Unit = {
      import scala.jdk.CollectionConverters.*
      factory.fromSpecific(values).asScala.toMap ==> values.toMap
    }

    convertAndVerifyUnstable[String, Int, ju.Dictionary]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)
    convertAndVerifyUnstable[String, Int, ju.Hashtable]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)
  }

  test(
    "java.util.Collection instances should be resolved for both concrete and abstract types for java.util.Map types"
  ) {
    def convertAndVerifyStable[K, V, CC[K1, V1] <: ju.Map[K1, V1]](values: (K, V)*)(implicit
        factory: JavaFactory[(K, V), CC[K, V]]
    ): Unit = {
      import scala.jdk.CollectionConverters.*
      factory
        .fromSpecific(values)
        .entrySet()
        .iterator()
        .asScala
        .map(es => es.getKey -> es.getValue)
        .toVector ==> values.iterator.toVector
    }

    def convertAndVerifyUnstable[K: Ordering, V, CC[K1, V1] <: ju.Map[K1, V1]](values: (K, V)*)(implicit
        factory: JavaFactory[(K, V), CC[K, V]]
    ): Unit = {
      import scala.jdk.CollectionConverters.*
      factory.fromSpecific(values).asScala.toMap ==> values.toMap
    }

    def convertAndVerifySorted[K: Ordering, V, CC[K1, V1] <: ju.Map[K1, V1]](values: (K, V)*)(implicit
        factory: JavaFactory[(K, V), CC[K, V]]
    ): Unit = {
      import scala.jdk.CollectionConverters.*
      factory.fromSpecific(values).asScala.toVector ==> values.iterator.toVector.sortBy(_._1)
    }

    convertAndVerifyUnstable[String, Int, ju.Map]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)
    convertAndVerifyUnstable[String, Int, ju.AbstractMap]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)
    convertAndVerifySorted[String, Int, ju.SortedMap]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)
    convertAndVerifySorted[String, Int, ju.NavigableMap]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)
    convertAndVerifyUnstable[String, Int, ju.HashMap]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)
    convertAndVerifyStable[String, Int, ju.LinkedHashMap]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)
    convertAndVerifyUnstable[String, Int, ju.WeakHashMap]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)
    convertAndVerifySorted[String, Int, ju.TreeMap]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)

    // TODO: EnumMap
  }
}
