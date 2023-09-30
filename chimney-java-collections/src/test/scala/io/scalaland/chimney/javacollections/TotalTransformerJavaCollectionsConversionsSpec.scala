package io.scalaland.chimney.javacollections

import java.util as ju
import io.scalaland.chimney.{ChimneySpec, Transformer}
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.JavaEnum

import scala.collection.compat.*
import scala.collection.immutable.{ListMap, ListSet, SortedMap}
import scala.jdk.CollectionConverters.*
import scala.compat.java8.StreamConverters.* // Scala 2.12 doesn't have scala.jdk.StreamConverters

class TotalTransformerJavaCollectionsConversionsSpec extends ChimneySpec {

  implicit private val intToString: Transformer[Int, String] = _.toString

  group("conversion from Scala types to Java types") {

    test("to java.util.Optional type") {
      // identity transformation of inner type:

      (Some(1): Option[Int]).transformInto[ju.Optional[Int]] ==> ju.Optional.of(1)
      (None: Option[Int]).transformInto[ju.Optional[Int]] ==> ju.Optional.empty()
      1.transformInto[ju.Optional[Int]] ==> ju.Optional.of(1)

      // provided transformation of inner type:

      (Some(1): Option[Int]).transformInto[ju.Optional[String]] ==> ju.Optional.of("1")
      (None: Option[Int]).transformInto[ju.Optional[String]] ==> ju.Optional.empty()
      1.transformInto[ju.Optional[String]] ==> ju.Optional.of("1")
    }

    test("to java.util.Iterator type") {
      // identity transformation of inner type:

      Iterator(4, 3, 2, 1).transformInto[ju.Iterator[Int]].asScala.toList ==> List(4, 3, 2, 1)

      // provided transformation of inner type:

      Iterator(4, 3, 2, 1).transformInto[ju.Iterator[String]].asScala.toList ==> List("4", "3", "2", "1")
    }

    test("to java.util.Enumeration type") {
      // identity transformation of inner type:

      Iterator(4, 3, 2, 1).transformInto[ju.Enumeration[Int]].asScala.toList ==> List(4, 3, 2, 1)

      // provided transformation of inner type:

      Iterator(4, 3, 2, 1).transformInto[ju.Enumeration[String]].asScala.toList ==> List("4", "3", "2", "1")
    }

    test("to java.util.Collection types") {
      val input = Seq(4, 3, 2, 1)
      val outputStable = List("4", "3", "2", "1")
      val outputUnstable = Set("4", "3", "2", "1")
      val outputSorted = List("1", "2", "3", "4")

      // identity transformation of inner type:

      input.transformInto[ju.Collection[Int]].asScala.toSet ==> input.toSet

      // provided transformation of inner type:

      input.transformInto[ju.Collection[String]].asScala.toSet ==> outputUnstable
      input.transformInto[ju.AbstractCollection[String]].asScala.toSet ==> outputUnstable

      input.transformInto[ju.List[String]].asScala.toList ==> outputStable
      input.transformInto[ju.AbstractList[String]].asScala.toList ==> outputStable
      input.transformInto[ju.AbstractSequentialList[String]].asScala.toList ==> outputStable
      input.transformInto[ju.ArrayList[String]].asScala.toList ==> outputStable
      input.transformInto[ju.LinkedList[String]].asScala.toList ==> outputStable
      input.transformInto[ju.Vector[String]].asScala.toList ==> outputStable
      input.transformInto[ju.Stack[String]].asScala.toList ==> outputStable

      input.transformInto[ju.Deque[String]].asScala.toList ==> outputStable
      input.transformInto[ju.ArrayDeque[String]].asScala.toList ==> outputStable

      input.transformInto[ju.Queue[String]].asScala.toList ==> outputSorted
      input.transformInto[ju.AbstractQueue[String]].asScala.toList ==> outputSorted
      input.transformInto[ju.PriorityQueue[String]].asScala.toList ==> outputSorted

      input.transformInto[ju.Set[String]].asScala.toSet ==> outputUnstable
      input.transformInto[ju.AbstractSet[String]].asScala.toSet ==> outputUnstable
      input.transformInto[ju.SortedSet[String]].asScala.toList ==> outputSorted
      input.transformInto[ju.NavigableSet[String]].asScala.toList ==> outputSorted
      input.transformInto[ju.HashSet[String]].asScala.toSet ==> outputUnstable
      input.transformInto[ju.LinkedHashSet[String]].asScala.toList ==> outputStable
      input.transformInto[ju.TreeSet[String]].asScala.toList ==> outputSorted
    }

    test("to java.util.Dictionary types") {
      val input = ListMap(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)
      val output = Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")

      // identity transformation of inner type:

      input.transformInto[ju.Dictionary[Int, Int]].asScala ==> input.toMap
      input.transformInto[ju.Properties].asScala ==> output.toMap

      // provided transformation of inner type:

      input.transformInto[ju.Dictionary[String, String]].asScala ==> output
      (input.transformInto[ju.Hashtable[String, String]]: ju.Dictionary[String, String]).asScala ==> output
    }

    test("to java.util.Map types") {
      val input = ListMap(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)
      val outputStable = List("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val outputUnstable = Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val outputSorted = List("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4")

      // identity transformation of inner type:

      input.transformInto[ju.Map[Int, Int]].asScala ==> input.toMap

      // provided transformation of inner type:

      input.transformInto[ju.Map[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.AbstractMap[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.SortedMap[String, String]].asScala.toList ==> outputSorted
      input.transformInto[ju.NavigableMap[String, String]].asScala.toList ==> outputSorted
      input.transformInto[ju.HashMap[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.LinkedHashMap[String, String]].asScala.toList ==> outputStable
      input.transformInto[ju.TreeMap[String, String]].asScala.toList ==> outputSorted
    }

    test("to java.lang.Enum-supporting types") {
      Set(JavaEnum.Blue, JavaEnum.Green, JavaEnum.Red).transformInto[ju.EnumSet[JavaEnum]].asScala ==> Set(
        JavaEnum.Blue,
        JavaEnum.Green,
        JavaEnum.Red
      )

      // identity transformation of inner type:

      Map(JavaEnum.Blue -> 3, JavaEnum.Green -> 2, JavaEnum.Red -> 1)
        .transformInto[ju.EnumMap[JavaEnum, Int]]
        .asScala ==> Map(JavaEnum.Blue -> 3, JavaEnum.Green -> 2, JavaEnum.Red -> 1)

      // provided transformation of inner type:

      Map(JavaEnum.Blue -> 3, JavaEnum.Green -> 2, JavaEnum.Red -> 1)
        .transformInto[ju.EnumMap[JavaEnum, String]]
        .asScala ==> Map(JavaEnum.Blue -> "3", JavaEnum.Green -> "2", JavaEnum.Red -> "1")
    }

    test("to java.util.BitSet type") {
      Set(1, 2, 4, 8).transformInto[ju.BitSet].toLongArray ==> Array((1 << 1) + (1 << 2) + (1 << 4) + (1 << 8))
    }

    test("to java.util.stream.BaseStream types") {
      val strings = List("4", "3", "2", "1")
      val ints = List(4, 3, 2, 1)
      val longs = List(4L, 3L, 2L, 1L)
      val doubles = List(4.0, 3.0, 2.0, 1.0)

      // identity transformation of inner type:

      strings.transformInto[ju.stream.Stream[String]].toScala(List) ==> strings
      ints.transformInto[ju.stream.IntStream].toScala(List) ==> ints
      longs.transformInto[ju.stream.LongStream].toScala(List) ==> longs
      doubles.transformInto[ju.stream.DoubleStream].toScala(List) ==> doubles

      // provided transformation of inner type:

      implicit def longToString: Transformer[Long, String] = _.toInt.toString
      implicit def doubleToString: Transformer[Double, String] = _.toInt.toString
      ints.transformInto[ju.stream.Stream[String]].toScala(List) ==> strings
      longs.transformInto[ju.stream.Stream[String]].toScala(List) ==> strings
      doubles.transformInto[ju.stream.Stream[String]].toScala(List) ==> strings

      implicit def longToInt: Transformer[Long, Int] = _.toInt
      implicit def doubleToInt: Transformer[Double, Int] = _.toInt
      longs.transformInto[ju.stream.IntStream].toScala(List) ==> ints
      doubles.transformInto[ju.stream.IntStream].toScala(List) ==> ints

      implicit def intToLong: Transformer[Int, Long] = _.toLong
      implicit def doubleToLong: Transformer[Double, Long] = _.toLong
      ints.transformInto[ju.stream.LongStream].toScala(List) ==> longs
      doubles.transformInto[ju.stream.LongStream].toScala(List) ==> longs

      implicit def intToDouble: Transformer[Int, Double] = _.toDouble
      implicit def longToDouble: Transformer[Long, Double] = _.toDouble
      ints.transformInto[ju.stream.DoubleStream].toScala(List) ==> doubles
      longs.transformInto[ju.stream.DoubleStream].toScala(List) ==> doubles
    }
  }

  group("conversion from Java types to Scala types") {

    test("from java.util.Optional type") {
      // identity transformation of inner type:

      ju.Optional.of(1).transformInto[Option[Int]] ==> Some(1)
      ju.Optional.empty[Int]().transformInto[Option[Int]] ==> None

      // provided transformation of inner type:

      ju.Optional.of(1).transformInto[Option[String]] ==> Some("1")
      ju.Optional.empty[Int]().transformInto[Option[String]] ==> None
    }

    test("from java.util.Iterator type") {
      val input = new ju.ArrayList[Int]
      input.add(4)
      input.add(3)
      input.add(2)
      input.add(1)

      // identity transformation of inner type:

      input.iterator().transformInto[Iterator[Int]].toList ==> List(4, 3, 2, 1)

      // provided transformation of inner type:

      input.iterator().transformInto[Iterator[String]].toList ==> List("4", "3", "2", "1")
    }

    test("from java.util.Enumeration type") {
      val input = new ju.Vector[Int]
      input.add(4)
      input.add(3)
      input.add(2)
      input.add(1)

      // identity transformation of inner type:

      input.elements().transformInto[Iterator[Int]].toList ==> List(4, 3, 2, 1)

      // provided transformation of inner type:

      input.elements().transformInto[Iterator[String]].toList ==> List("4", "3", "2", "1")
    }

    test("from java.util.Collection types") {
      def initCollection(coll: ju.Collection[Int]): coll.type = {
        coll.add(4)
        coll.add(3)
        coll.add(2)
        coll.add(1)
        coll
      }

      val outputStable = List("4", "3", "2", "1")
      val outputUnstable = Set("4", "3", "2", "1")
      val outputSorted = List("1", "2", "3", "4")

      // identity transformation of inner type:

      initCollection(new ju.ArrayList[Int]).transformInto[List[Int]] ==> List(4, 3, 2, 1)

      // provided transformation of inner type:

      initCollection(new ju.ArrayList[Int]).transformInto[List[String]] ==> outputStable
      initCollection(new ju.LinkedList[Int]).transformInto[List[String]] ==> outputStable
      initCollection(new ju.Vector[Int]).transformInto[List[String]] ==> outputStable
      initCollection(new ju.Stack[Int]).transformInto[List[String]] ==> outputStable

      initCollection(new ju.ArrayDeque[Int]).transformInto[List[String]] ==> outputStable

      initCollection(new ju.PriorityQueue[Int]).transformInto[List[String]] ==> outputSorted

      initCollection(new ju.HashSet[Int]).transformInto[Set[String]] ==> outputUnstable
      initCollection(new ju.TreeSet[Int]).transformInto[ListSet[String]].toList ==> outputSorted
    }

    test("from java.util.Dictionary types") {
      def initDictionary(dict: ju.Dictionary[Int, Int]): dict.type = {
        dict.put(4, 4)
        dict.put(3, 3)
        dict.put(2, 2)
        dict.put(1, 1)
        dict
      }

      // identity transformation of inner type:

      initDictionary(new ju.Hashtable[Int, Int])
        .transformInto[Map[Int, Int]] ==> Map(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)
      locally {
        val p = new ju.Properties()
        p.put("4", "4")
        p.put("3", "3")
        p.put("2", "2")
        p.put("1", "1")
        p
      }
        .transformInto[Map[Any, Any]] ==> Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")

      // provided transformation of inner type:

      initDictionary(new ju.Hashtable[Int, Int])
        .transformInto[Map[String, String]] ==> Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
    }

    test("from java.util.Map types") {
      def initMap(map: ju.Map[Int, Int]): map.type = {
        map.put(4, 4)
        map.put(3, 3)
        map.put(2, 2)
        map.put(1, 1)
        map
      }

      val unstableOutput = Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val stableOutput = ListMap("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val sortedOutput = SortedMap("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")

      // identity transformation of inner type:

      initMap(new ju.HashMap[Int, Int]).transformInto[Map[Int, Int]] ==> Map(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)

      // provided transformation of inner type:

      initMap(new ju.HashMap[Int, Int]).transformInto[Map[String, String]] ==> unstableOutput
      initMap(new ju.LinkedHashMap[Int, Int]).transformInto[ListMap[String, String]].toVector ==> stableOutput.toVector
      initMap(new ju.TreeMap[Int, Int]).transformInto[SortedMap[String, String]].toVector ==> sortedOutput.toVector
    }

    test("from java.lang.Enum-supporting type") {
      val enumSet = ju.EnumSet.allOf(classOf[JavaEnum])
      enumSet.transformInto[Set[JavaEnum]] ==> Set(JavaEnum.Red, JavaEnum.Green, JavaEnum.Blue)

      val enumMap = new ju.EnumMap[JavaEnum, Int](classOf[JavaEnum])
      JavaEnum.values().foreach(e => enumMap.put(e, e.ordinal()))

      // identity transformation of inner type:

      enumMap.transformInto[Map[JavaEnum, Int]] ==> Map(JavaEnum.Red -> 0, JavaEnum.Green -> 1, JavaEnum.Blue -> 2)

      // provided transformation of inner type:

      enumMap
        .transformInto[Map[JavaEnum, String]] ==> Map(JavaEnum.Red -> "0", JavaEnum.Green -> "1", JavaEnum.Blue -> "2")
    }

    test("from java.util.BitSet type") {
      val input = new ju.BitSet
      input.set(1)
      input.set(2)
      input.set(3)
      input.set(4)

      // identity transformation of inner type:

      input.transformInto[Set[Int]] ==> Set(1, 2, 3, 4)

      // provided transformation of inner type:

      input.transformInto[Set[String]] ==> Set("1", "2", "3", "4")
    }

    test("from java.util.stream.BaseStream types") {
      // identity transformation of inner type:

      ju.stream.Stream.of("4", "3", "2", "1").transformInto[List[String]] ==> List("4", "3", "2", "1")
      ju.stream.IntStream.of(4, 3, 2, 1).transformInto[List[Int]] ==> List(4, 3, 2, 1)
      ju.stream.LongStream.of(4L, 3L, 2L, 1L).transformInto[List[Long]] ==> List(4L, 3L, 2L, 1L)
      ju.stream.DoubleStream.of(4.0, 3.0, 2.0, 1.0).transformInto[List[Double]] ==> List(4.0, 3.0, 2.0, 1.0)

      // provided transformation of inner type:

      implicit def longToInt: Transformer[Long, Int] = _.toInt
      implicit def doubleToInt: Transformer[Double, Int] = _.toInt
      ju.stream.LongStream.of(4L, 3L, 2L, 1L).transformInto[List[Int]] ==> List(4, 3, 2, 1)
      ju.stream.DoubleStream.of(4.0, 3.0, 2.0, 1.0).transformInto[List[Int]] ==> List(4, 3, 2, 1)
    }
  }

  group("conversion from Java types to Java types") {

    test("for java.util.Optional type") {
      // identity transformation of inner type:

      ju.Optional.of(1).transformInto[ju.Optional[Int]] ==> ju.Optional.of(1)
      ju.Optional.empty[Int]().transformInto[ju.Optional[Int]] ==> ju.Optional.empty[String]()

      // provided transformation of inner type:

      ju.Optional.of(1).transformInto[ju.Optional[String]] ==> ju.Optional.of("1")
      ju.Optional.empty[Int]().transformInto[ju.Optional[String]] ==> ju.Optional.empty[String]()
    }

    test("for java.util.Iterator type") {
      val input = new ju.ArrayList[Int]
      input.add(4)
      input.add(3)
      input.add(2)
      input.add(1)

      // identity transformation of inner type:

      input.transformInto[ju.Iterator[Int]].asScala.toList ==> List(4, 3, 2, 1)
      input.iterator().transformInto[ju.List[Int]].asScala.toList ==> List(4, 3, 2, 1)

      // provided transformation of inner type:

      input.iterator().transformInto[ju.Iterator[String]].asScala.toList ==> List("4", "3", "2", "1")
    }

    test("for java.util.Enumeration type") {
      val input = new ju.Vector[Int]
      input.add(4)
      input.add(3)
      input.add(2)
      input.add(1)

      // identity transformation of inner type:

      input.transformInto[ju.List[Int]].asScala.toList ==> List(4, 3, 2, 1)
      input.elements().transformInto[ju.List[Int]].asScala.toList ==> List(4, 3, 2, 1)

      // provided transformation of inner type:

      input.elements().transformInto[ju.Enumeration[String]].asScala.toList ==> List("4", "3", "2", "1")
    }

    test("for java.util.Collection types") {
      val input = new ju.ArrayList[Int]
      input.add(4)
      input.add(3)
      input.add(2)
      input.add(1)
      val outputStable = List("4", "3", "2", "1")
      val outputUnstable = Set("4", "3", "2", "1")
      val outputSorted = List("1", "2", "3", "4")

      // identity transformation of inner type:

      input.transformInto[ju.Collection[Int]].asScala.toList ==> List(4, 3, 2, 1)

      // provided transformation of inner type:

      input.transformInto[ju.Collection[String]].asScala.toSet ==> outputUnstable
      input.transformInto[ju.AbstractCollection[String]].asScala.toSet ==> outputUnstable

      input.transformInto[ju.List[String]].asScala.toList ==> outputStable
      input.transformInto[ju.AbstractList[String]].asScala.toList ==> outputStable
      input.transformInto[ju.AbstractSequentialList[String]].asScala.toList ==> outputStable
      input.transformInto[ju.ArrayList[String]].asScala.toList ==> outputStable
      input.transformInto[ju.LinkedList[String]].asScala.toList ==> outputStable
      input.transformInto[ju.Vector[String]].asScala.toList ==> outputStable
      input.transformInto[ju.Stack[String]].asScala.toList ==> outputStable

      input.transformInto[ju.Deque[String]].asScala.toList ==> outputStable
      input.transformInto[ju.ArrayDeque[String]].asScala.toList ==> outputStable

      input.transformInto[ju.Queue[String]].asScala.toList ==> outputSorted
      input.transformInto[ju.AbstractQueue[String]].asScala.toList ==> outputSorted
      input.transformInto[ju.PriorityQueue[String]].asScala.toList ==> outputSorted

      input.transformInto[ju.Set[String]].asScala.toSet ==> outputUnstable
      input.transformInto[ju.AbstractSet[String]].asScala.toSet ==> outputUnstable
      input.transformInto[ju.SortedSet[String]].asScala.toList ==> outputSorted
      input.transformInto[ju.NavigableSet[String]].asScala.toList ==> outputSorted
      input.transformInto[ju.HashSet[String]].asScala.toSet ==> outputUnstable
      input.transformInto[ju.LinkedHashSet[String]].asScala.toList ==> outputStable
      input.transformInto[ju.TreeSet[String]].asScala.toList ==> outputSorted
    }

    test("for java.util.Dictionary types") {
      val input = new ju.LinkedHashMap[Int, Int]
      input.put(4, 4)
      input.put(3, 3)
      input.put(2, 2)
      input.put(1, 1)
      val output = Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")

      // identity transformation of inner type:

      input.transformInto[ju.Dictionary[Int, Int]].asScala ==> input.asScala.toMap

      // provided transformation of inner type:

      input.transformInto[ju.Dictionary[String, String]].asScala ==> output
      (input.transformInto[ju.Hashtable[String, String]]: ju.Dictionary[String, String]).asScala ==> output
    }

    test("for java.util.Map types") {
      val input = new ju.LinkedHashMap[Int, Int]
      input.put(4, 4)
      input.put(3, 3)
      input.put(2, 2)
      input.put(1, 1)
      val outputStable = List("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val outputUnstable = Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val outputSorted = List("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4")

      // identity transformation of inner type:

      input.transformInto[ju.Map[Int, Int]].asScala ==> input.asScala.toMap

      // provided transformation of inner type:

      input.transformInto[ju.Map[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.AbstractMap[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.SortedMap[String, String]].asScala.toList ==> outputSorted
      input.transformInto[ju.NavigableMap[String, String]].asScala.toList ==> outputSorted
      input.transformInto[ju.HashMap[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.LinkedHashMap[String, String]].asScala.toList ==> outputStable
      input.transformInto[ju.TreeMap[String, String]].asScala.toList ==> outputSorted
    }

    test("for java.util.Dictionary types") {
      val input = new ju.LinkedHashMap[Int, Int]
      input.put(4, 4)
      input.put(3, 3)
      input.put(2, 2)
      input.put(1, 1)
      val output = Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")

      // identity transformation of inner type:

      input.transformInto[ju.Dictionary[Int, Int]].asScala ==> input.asScala.toMap

      // provided transformation of inner type:

      input.transformInto[ju.Dictionary[String, String]].asScala ==> output
      (input.transformInto[ju.Hashtable[String, String]]: ju.Dictionary[String, String]).asScala ==> output
    }

    test("for java.util.Map types") {
      val input = new ju.LinkedHashMap[Int, Int]
      input.put(4, 4)
      input.put(3, 3)
      input.put(2, 2)
      input.put(1, 1)
      val outputStable = List("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val outputUnstable = Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val outputSorted = List("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4")

      // identity transformation of inner type:

      input.transformInto[ju.Map[Int, Int]].asScala ==> input.asScala.toMap

      // provided transformation of inner type:

      input.transformInto[ju.Map[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.AbstractMap[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.SortedMap[String, String]].asScala.toList ==> outputSorted
      input.transformInto[ju.NavigableMap[String, String]].asScala.toList ==> outputSorted
      input.transformInto[ju.HashMap[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.LinkedHashMap[String, String]].asScala.toList ==> outputStable
      input.transformInto[ju.TreeMap[String, String]].asScala.toList ==> outputSorted
    }

    test("for java.util.stream.BaseStream types") {
      // identity transformation of inner type:

      ju.stream.Stream.of(4, 3, 2, 1).transformInto[ju.stream.IntStream].toScala(List) ==> List(4, 3, 2, 1)
      ju.stream.Stream.of(4L, 3L, 2L, 1L).transformInto[ju.stream.LongStream].toScala(List) ==> List(4L, 3L, 2L, 1L)
      ju.stream.Stream.of(4.0, 3.0, 2.0, 1.0).transformInto[ju.stream.DoubleStream].toScala(List) ==> List(
        4.0,
        3.0,
        2.0,
        1.0
      )
      ju.stream.IntStream.of(4, 3, 2, 1).transformInto[ju.stream.Stream[Int]].toScala(List) ==> List(4, 3, 2, 1)
      ju.stream.LongStream.of(4L, 3L, 2L, 1L).transformInto[ju.stream.Stream[Long]].toScala(List) ==> List(
        4L,
        3L,
        2L,
        1L
      )
      ju.stream.DoubleStream.of(4.0, 3.0, 2.0, 1.0).transformInto[ju.stream.Stream[Double]].toScala(List) ==> List(
        4.0,
        3.0,
        2.0,
        1.0
      )

      // provided transformation of inner type:

      implicit def longToString: Transformer[Long, Int] = _.toInt
      implicit def doubleToString: Transformer[Double, Int] = _.toInt
      ju.stream.IntStream.of(4, 3, 2, 1).transformInto[ju.stream.Stream[String]].toScala(List) ==> List(
        "4",
        "3",
        "2",
        "1"
      )
      ju.stream.LongStream.of(4L, 3L, 2L, 1L).transformInto[ju.stream.Stream[Int]].toScala(List) ==> List(
        4,
        3,
        2,
        1
      )
      ju.stream.DoubleStream.of(4.0, 3.0, 2.0, 1.0).transformInto[ju.stream.Stream[Int]].toScala(List) ==> List(
        4,
        3,
        2,
        1
      )
    }
  }
}
