package io.scalaland.chimney.javacollections

import java.util as ju
import io.scalaland.chimney.ChimneySpec
import io.scalaland.chimney.fixtures.JavaEnum

class JavaFactorySpec extends ChimneySpec {

  test("java.util.Iterator instance should be resolved") {
    val iterator =
      implicitly[JavaFactory[String, java.util.Iterator[String]]].fromSpecific(Iterator("d", "c", "b", "a"))

    import scala.jdk.CollectionConverters.*
    iterator.asScala.toVector ==> Vector("d", "c", "b", "a")
  }

  test("java.util.Enumeration instance should be resolved") {
    val iterator =
      implicitly[JavaFactory[String, java.util.Enumeration[String]]].fromSpecific(Iterator("d", "c", "b", "a"))

    import scala.jdk.CollectionConverters.*
    iterator.asScala.toVector ==> Vector("d", "c", "b", "a")
  }

  test("java.util.Collection instances should be resolved for both concrete and abstract types") {
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
  }

  test("java.util.Dictionary instances should be resolved for both concrete and abstract types") {
    def convertAndVerifyUnstable[K: Ordering, V, CC[K1, V1] <: ju.Dictionary[K1, V1]](values: (K, V)*)(implicit
        factory: JavaFactory[(K, V), CC[K, V]]
    ): Unit = {
      import scala.jdk.CollectionConverters.*
      factory.fromSpecific(values).asScala.toMap ==> values.toMap
    }

    convertAndVerifyUnstable[String, Int, ju.Dictionary]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)
    convertAndVerifyUnstable[String, Int, ju.Hashtable]("d" -> 10, "c" -> 8, "b" -> 4, "a" -> 0)
  }

  test("java.util.Map instances should be resolved for both concrete and abstract types") {
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
  }

  test("java.util.Collection instances should be resolved for java.lang.Enum specializations") {
    val enumSet = implicitly[JavaFactory[JavaEnum, ju.EnumSet[JavaEnum]]]
      .fromSpecific(Iterator(JavaEnum.Blue, JavaEnum.Green, JavaEnum.Red))
    val enumMap = implicitly[JavaFactory[(JavaEnum, Int), ju.EnumMap[JavaEnum, Int]]].fromSpecific(
      Iterator(
        JavaEnum.Blue -> 10,
        JavaEnum.Green -> 5,
        JavaEnum.Red -> 0
      )
    )

    import scala.jdk.CollectionConverters.*
    enumSet.iterator().asScala.toVector ==> Vector(JavaEnum.Red, JavaEnum.Green, JavaEnum.Blue)
    enumMap.entrySet().iterator().asScala.map(es => es.getKey -> es.getValue).toVector ==> Vector(
      JavaEnum.Red -> 0,
      JavaEnum.Green -> 5,
      JavaEnum.Blue -> 10
    )
  }

  test("java.util.Dictionary instances should be resolved for java.util.Properties specialization") {
    val properties =
      implicitly[JavaFactory[(String, String), ju.Properties]].fromSpecific(Iterator("a" -> "1", "b" -> "2"))

    import scala.jdk.CollectionConverters.*
    properties.asScala ==> Map("a" -> "1", "b" -> "2")
  }

  test("java.util.BitSet instance should be resolved") {
    val bitSet = implicitly[JavaFactory[Int, ju.BitSet]].fromSpecific(Iterator(10, 8, 4, 2, 0))

    bitSet.toLongArray ==> Array((1L << 10) + (1L << 8) + (1L << 4) + (1L << 2) + (1L << 0))
  }

  test("java.util.stream.BaseStream instances should be resolved for concrete types") {
    def convertAndVerifyStable[A, CC <: ju.stream.BaseStream[?, CC]](values: A*)(implicit
        factory: JavaFactory[A, CC]
    ): Unit = {
      import scala.jdk.CollectionConverters.*
      factory
        .fromSpecific(values)
        .iterator()
        .asScala
        .toVector ==> values.iterator.toVector
    }

    convertAndVerifyStable[String, ju.stream.Stream[String]]("d", "c", "b", "a")
    convertAndVerifyStable[Int, ju.stream.IntStream](4, 3, 2, 1)
    convertAndVerifyStable[Long, ju.stream.LongStream](4L, 3L, 2L, 1L)
    convertAndVerifyStable[Double, ju.stream.DoubleStream](4.0, 3.0, 2.0, 1.0)
  }

  test("JavaFactory with conversions can be used as scala.collection.compat.Factory") {
    val javaFactory: JavaFactory[(String, Int), ju.Map[String, Int]] = implicitly

    import scala.collection.compat.*
    import JavaFactory.ConversionToScalaFactory.*
    val scalaFactory: Factory[(String, Int), ju.Map[String, Int]] = javaFactory

    // the code below doesn't work on 2.12 even with scala.collection.compat:
    //   List[(String, Int)]("1" -> 2, "3" -> 4).to(factory)
    // because CanBuildFrom cannot into Map

    import scala.jdk.CollectionConverters.*
    scalaFactory.fromSpecific(Iterator("a" -> 1, "b" -> 2)).asScala ==> Map("a" -> 1, "b" -> 2)
  }
}
