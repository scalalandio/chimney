package io.scalaland.chimney.javacollections

import java.util as ju
import io.scalaland.chimney.{partial, ChimneySpec, PartialTransformer, Transformer}
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.JavaEnum

import scala.collection.compat.*
import scala.collection.immutable.{ListMap, ListSet, SortedMap}
import scala.jdk.CollectionConverters.*
import scala.compat.java8.StreamConverters.* // Scala 2.12 doesn't have scala.jdk.StreamConverters

class PartialTransformerJavaCollectionsConversionsSpec extends ChimneySpec {

  implicit private val stringToInt: PartialTransformer[String, Int] = PartialTransformer.fromFunction(_.toInt)

  group("conversion from Scala types to Java types") {

    test("to java.util.Optional type") {
      // identity transformation of inner type:

      (Some("1"): Option[String]).transformIntoPartial[ju.Optional[String]].asOption.get ==> ju.Optional.of("1")
      (None: Option[String]).transformIntoPartial[ju.Optional[String]].asOption.get ==> ju.Optional.empty()
      "1".transformIntoPartial[ju.Optional[String]].asOption.get ==> ju.Optional.of("1")

      // provided transformation of inner type:

      (Some("1"): Option[String]).transformIntoPartial[ju.Optional[Int]].asOption.get ==> ju.Optional.of(1)
      (None: Option[String]).transformIntoPartial[ju.Optional[Int]].asOption.get ==> ju.Optional.empty()
      "1".transformIntoPartial[ju.Optional[Int]].asOption.get ==> ju.Optional.of(1)

      // failed parsing of inner type:

      Option("a").transformIntoPartial[ju.Optional[Int]].asErrorPathMessageStrings ==> Iterable(
        "" -> "For input string: \"a\""
      )
      "a".transformIntoPartial[ju.Optional[Int]].asErrorPathMessageStrings ==> Iterable("" -> "For input string: \"a\"")
    }

    test("to java.util.Iterator type") {
      // identity transformation of inner type:

      Iterator("4", "3", "2", "1").transformIntoPartial[ju.Iterator[String]].asOption.get.asScala.toList ==> List(
        "4",
        "3",
        "2",
        "1"
      )

      // provided transformation of inner type:

      Iterator("4", "3", "2", "1").transformIntoPartial[ju.Iterator[Int]].asOption.get.asScala.toList ==> List(
        4,
        3,
        2,
        1
      )

      // failed parsing of inner type:

      Iterator("a", "3", "b", "1").transformIntoPartial[ju.Iterator[Int]].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
    }

    test("to java.util.Enumeration type") {
      // identity transformation of inner type:

      Iterator("4", "3", "2", "1").transformIntoPartial[ju.Enumeration[String]].asOption.get.asScala.toList ==> List(
        "4",
        "3",
        "2",
        "1"
      )

      // provided transformation of inner type:

      Iterator("4", "3", "2", "1").transformIntoPartial[ju.Enumeration[Int]].asOption.get.asScala.toList ==> List(
        4,
        3,
        2,
        1
      )

      // failed parsing of inner type:

      Iterator("a", "3", "b", "1").transformIntoPartial[ju.Enumeration[Int]].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
    }

    test("to java.util.Collection types") {
      val input = Seq("4", "3", "2", "1")
      val outputStable = List(4, 3, 2, 1)
      val outputUnstable = Set(4, 3, 2, 1)
      val outputSorted = List(1, 2, 3, 4)

      // identity transformation of inner type:

      input.transformIntoPartial[ju.Collection[String]].asOption.get.asScala.toSet ==> input.toSet

      // provided transformation of inner type:

      input.transformIntoPartial[ju.Collection[Int]].asOption.get.asScala.toSet ==> outputUnstable
      input.transformIntoPartial[ju.AbstractCollection[Int]].asOption.get.asScala.toSet ==> outputUnstable

      input.transformIntoPartial[ju.List[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.AbstractList[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.AbstractSequentialList[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.ArrayList[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.LinkedList[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.Vector[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.Stack[Int]].asOption.get.asScala.toList ==> outputStable

      input.transformIntoPartial[ju.Deque[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.ArrayDeque[Int]].asOption.get.asScala.toList ==> outputStable

      input.transformIntoPartial[ju.Queue[Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.AbstractQueue[Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.PriorityQueue[Int]].asOption.get.asScala.toList ==> outputSorted

      input.transformIntoPartial[ju.Set[Int]].asOption.get.asScala.toSet ==> outputUnstable
      input.transformIntoPartial[ju.AbstractSet[Int]].asOption.get.asScala.toSet ==> outputUnstable
      input.transformIntoPartial[ju.SortedSet[Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.NavigableSet[Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.HashSet[Int]].asOption.get.asScala.toSet ==> outputUnstable
      input.transformIntoPartial[ju.LinkedHashSet[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.TreeSet[Int]].asOption.get.asScala.toList ==> outputSorted

      // failed parsing of inner type:

      Seq("a", "3", "b", "1").transformIntoPartial[ju.List[Int]].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
      Seq("a", "3", "b", "1").transformIntoPartial[ju.Deque[Int]].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
      Seq("a", "3", "b", "1").transformIntoPartial[ju.Queue[Int]].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
      Seq("a", "3", "b", "1").transformIntoPartial[ju.Set[Int]].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
    }

    test("to java.util.Dictionary types") {
      val input = ListMap("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val output = Map(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)

      // identity transformation of inner type:

      input.transformIntoPartial[ju.Dictionary[String, String]].asOption.get.asScala ==> input.toMap
      input.transformIntoPartial[ju.Properties].asOption.get.asScala ==> input.toMap

      // provided transformation of inner type:

      input.transformIntoPartial[ju.Dictionary[Int, Int]].asOption.get.asScala ==> output
      (input.transformIntoPartial[ju.Hashtable[Int, Int]]: partial.Result[
        ju.Dictionary[Int, Int]
      ]).asOption.get.asScala ==> output

      // failed parsing of inner type:

      ListMap("a" -> "4", "3" -> "b", "c" -> "d", "1" -> "1")
        .transformIntoPartial[ju.Dictionary[Int, Int]]
        .asErrorPathMessageStrings ==> Iterable(
        "(a).keys(a)" -> "For input string: \"a\"",
        "(3)" -> "For input string: \"b\"",
        "(c).keys(c)" -> "For input string: \"c\"",
        "(c)" -> "For input string: \"d\""
      )
    }

    test("to java.util.Map types") {
      val input = ListMap("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val outputStable = List(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)
      val outputUnstable = Map(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)
      val outputSorted = List(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4)

      // identity transformation of inner type:

      input.transformIntoPartial[ju.Map[String, String]].asOption.get.asScala ==> input.toMap

      // provided transformation of inner type:

      input.transformIntoPartial[ju.Map[Int, Int]].asOption.get.asScala ==> outputUnstable
      input.transformIntoPartial[ju.AbstractMap[Int, Int]].asOption.get.asScala ==> outputUnstable
      input.transformIntoPartial[ju.SortedMap[Int, Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.NavigableMap[Int, Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.HashMap[Int, Int]].asOption.get.asScala ==> outputUnstable
      input.transformIntoPartial[ju.LinkedHashMap[Int, Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.TreeMap[Int, Int]].asOption.get.asScala.toList ==> outputSorted

      // failed parsing of inner type:

      ListMap("a" -> "4", "3" -> "b", "c" -> "d", "1" -> "1")
        .transformIntoPartial[ju.Map[Int, Int]]
        .asErrorPathMessageStrings ==> Iterable(
        "(a).keys(a)" -> "For input string: \"a\"",
        "(3)" -> "For input string: \"b\"",
        "(c).keys(c)" -> "For input string: \"c\"",
        "(c)" -> "For input string: \"d\""
      )
    }

    test("to java.lang.Enum-supporting types") {
      Set(JavaEnum.Blue, JavaEnum.Green, JavaEnum.Red)
        .transformIntoPartial[ju.EnumSet[JavaEnum]]
        .asOption
        .get
        .asScala ==> Set(
        JavaEnum.Blue,
        JavaEnum.Green,
        JavaEnum.Red
      )

      // identity transformation of inner type:

      Map(JavaEnum.Blue -> "3", JavaEnum.Green -> "2", JavaEnum.Red -> "1")
        .transformIntoPartial[ju.EnumMap[JavaEnum, String]]
        .asOption
        .get
        .asScala ==> Map(JavaEnum.Blue -> "3", JavaEnum.Green -> "2", JavaEnum.Red -> "1")

      // provided transformation of inner type:

      Map(JavaEnum.Blue -> "3", JavaEnum.Green -> "2", JavaEnum.Red -> "1")
        .transformIntoPartial[ju.EnumMap[JavaEnum, Int]]
        .asOption
        .get
        .asScala ==> Map(JavaEnum.Blue -> 3, JavaEnum.Green -> 2, JavaEnum.Red -> 1)

      // failed parsing of inner type:

      ListMap(JavaEnum.Red -> "a", JavaEnum.Green -> "b", JavaEnum.Blue -> "c")
        .transformIntoPartial[ju.EnumMap[JavaEnum, Int]]
        .asErrorPathMessageStrings ==> Iterable(
        "(Red)" -> "For input string: \"a\"",
        "(Green)" -> "For input string: \"b\"",
        "(Blue)" -> "For input string: \"c\""
      )
    }

    test("to java.util.BitSet type") {
      // identity transformation of inner type:

      Set(1, 2, 4, 8).transformIntoPartial[ju.BitSet].asOption.get.toLongArray ==> Array(
        (1 << 1) + (1 << 2) + (1 << 4) + (1 << 8)
      )

      // provided transformation of inner type:

      Set("1", "2", "4", "8").transformIntoPartial[ju.BitSet].asOption.get.toLongArray ==> Array(
        (1 << 1) + (1 << 2) + (1 << 4) + (1 << 8)
      )

      // failed parsing of inner type:

      Set("a", "2", "b", "8").transformIntoPartial[ju.BitSet].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
    }

    test("to java.util.stream.BaseStream types") {
      val strings = List("4", "3", "2", "1")
      val ints = List(4, 3, 2, 1)
      val longs = List(4L, 3L, 2L, 1L)
      val doubles = List(4.0, 3.0, 2.0, 1.0)

      // identity transformation of inner type:

      strings.transformIntoPartial[ju.stream.Stream[String]].asOption.get.toScala(List) ==> strings
      ints.transformIntoPartial[ju.stream.IntStream].asOption.get.toScala(List) ==> ints
      longs.transformIntoPartial[ju.stream.LongStream].asOption.get.toScala(List) ==> longs
      doubles.transformIntoPartial[ju.stream.DoubleStream].asOption.get.toScala(List) ==> doubles

      // provided transformation of inner type:

      implicit def intToString: Transformer[Int, String] = _.toString
      implicit def longToString: Transformer[Long, String] = _.toInt.toString
      implicit def doubleToString: Transformer[Double, String] = _.toInt.toString
      ints.transformIntoPartial[ju.stream.Stream[String]].asOption.get.toScala(List) ==> strings
      longs.transformIntoPartial[ju.stream.Stream[String]].asOption.get.toScala(List) ==> strings
      doubles.transformIntoPartial[ju.stream.Stream[String]].asOption.get.toScala(List) ==> strings

      implicit def longToInt: Transformer[Long, Int] = _.toInt
      implicit def doubleToInt: Transformer[Double, Int] = _.toInt
      strings.transformIntoPartial[ju.stream.IntStream].asOption.get.toScala(List) ==> ints
      longs.transformIntoPartial[ju.stream.IntStream].asOption.get.toScala(List) ==> ints
      doubles.transformIntoPartial[ju.stream.IntStream].asOption.get.toScala(List) ==> ints

      implicit val stringToLong: PartialTransformer[String, Long] = PartialTransformer.fromFunction(_.toLong)
      implicit def intToLong: Transformer[Int, Long] = _.toLong
      implicit def doubleToLong: Transformer[Double, Long] = _.toLong
      strings.transformIntoPartial[ju.stream.LongStream].asOption.get.toScala(List) ==> longs
      ints.transformIntoPartial[ju.stream.LongStream].asOption.get.toScala(List) ==> longs
      doubles.transformIntoPartial[ju.stream.LongStream].asOption.get.toScala(List) ==> longs

      implicit val stringToDouble: PartialTransformer[String, Double] = PartialTransformer.fromFunction(_.toDouble)
      implicit def intToDouble: Transformer[Int, Double] = _.toDouble
      implicit def longToDouble: Transformer[Long, Double] = _.toDouble
      strings.transformIntoPartial[ju.stream.DoubleStream].asOption.get.toScala(List) ==> doubles
      ints.transformIntoPartial[ju.stream.DoubleStream].asOption.get.toScala(List) ==> doubles
      longs.transformIntoPartial[ju.stream.DoubleStream].asOption.get.toScala(List) ==> doubles

      // failed parsing of inner type:

      Seq("a", "1", "b", "2").transformIntoPartial[ju.stream.IntStream].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
      Seq("a", "1", "b", "2").transformIntoPartial[ju.stream.LongStream].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
      Seq("a", "1", "b", "2").transformIntoPartial[ju.stream.DoubleStream].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
    }
  }

  group("conversion from Java types to Scala types") {

    test("from java.util.Optional type") {
      // identity transformation of inner type:

      ju.Optional.of("1").transformIntoPartial[Option[String]].asOption.get ==> Some("1")
      ju.Optional.empty[String]().transformIntoPartial[Option[String]].asOption.get ==> None

      // provided transformation of inner type:

      ju.Optional.of("1").transformIntoPartial[Option[Int]].asOption.get ==> Some(1)
      ju.Optional.empty[String]().transformIntoPartial[Option[Int]].asOption.get ==> None

      // failed parsing of inner type:

      ju.Optional.of("a").transformIntoPartial[Option[Int]].asErrorPathMessageStrings ==> Iterable(
        "" -> "For input string: \"a\""
      )
    }

    test("from java.util.Iterator type") {
      val input = new ju.ArrayList[String]
      input.add("4")
      input.add("3")
      input.add("2")
      input.add("1")

      // identity transformation of inner type:

      input.iterator().transformIntoPartial[Iterator[String]].asOption.get.toList ==> List("4", "3", "2", "1")

      // provided transformation of inner type:

      input.iterator().transformIntoPartial[Iterator[Int]].asOption.get.toList ==> List(4, 3, 2, 1)

      // failed parsing of inner type:

      val input2 = new ju.ArrayList[String]
      input2.add("a")
      input2.add("3")
      input2.add("b")
      input2.add("1")
      input2.iterator().transformIntoPartial[Iterator[Int]].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
    }

    test("from java.util.Enumeration type") {
      val input = new ju.Vector[String]
      input.add("4")
      input.add("3")
      input.add("2")
      input.add("1")

      // identity transformation of inner type:

      input.elements().transformIntoPartial[Iterator[String]].asOption.get.toList ==> List("4", "3", "2", "1")

      // provided transformation of inner type:

      input.elements().transformIntoPartial[Iterator[Int]].asOption.get.toList ==> List(4, 3, 2, 1)

      // failed parsing of inner type:

      val input2 = new ju.Vector[String]
      input2.add("a")
      input2.add("3")
      input2.add("b")
      input2.add("1")
      input2.elements().transformIntoPartial[Iterator[Int]].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
    }

    test("from java.util.Collection types") {
      def initCollection(coll: ju.Collection[String]): coll.type = {
        coll.add("4")
        coll.add("3")
        coll.add("2")
        coll.add("1")
        coll
      }
      def initBadCollection(coll: ju.Collection[String]): coll.type = {
        coll.add("a")
        coll.add("3")
        coll.add("b")
        coll.add("1")
        coll
      }

      val outputStable = List(4, 3, 2, 1)
      val outputUnstable = Set(4, 3, 2, 1)
      val outputSorted = List(1, 2, 3, 4)

      // identity transformation of inner type:

      initCollection(new ju.ArrayList[String]).transformIntoPartial[List[String]].asOption.get ==> List(
        "4",
        "3",
        "2",
        "1"
      )

      // provided transformation of inner type:

      initCollection(new ju.ArrayList[String]).transformIntoPartial[List[Int]].asOption.get ==> outputStable
      initCollection(new ju.LinkedList[String]).transformIntoPartial[List[Int]].asOption.get ==> outputStable
      initCollection(new ju.Vector[String]).transformIntoPartial[List[Int]].asOption.get ==> outputStable
      initCollection(new ju.Stack[String]).transformIntoPartial[List[Int]].asOption.get ==> outputStable

      initCollection(new ju.ArrayDeque[String]).transformIntoPartial[List[Int]].asOption.get ==> outputStable

      initCollection(new ju.PriorityQueue[String]).transformIntoPartial[List[Int]].asOption.get ==> outputSorted

      initCollection(new ju.HashSet[String]).transformIntoPartial[Set[Int]].asOption.get ==> outputUnstable
      initCollection(new ju.TreeSet[String]).transformIntoPartial[ListSet[Int]].asOption.get.toList ==> outputSorted

      // failed parsing of inner type:

      initBadCollection(new ju.ArrayList[String])
        .transformIntoPartial[List[Int]]
        .asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
      initBadCollection(new ju.LinkedList[String])
        .transformIntoPartial[List[Int]]
        .asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
      initBadCollection(new ju.Vector[String]).transformIntoPartial[List[Int]].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
      initBadCollection(new ju.Stack[String]).transformIntoPartial[List[Int]].asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )

      initBadCollection(new ju.ArrayDeque[String])
        .transformIntoPartial[List[Int]]
        .asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
    }

    test("from java.util.Dictionary types") {
      def initDictionary(dict: ju.Dictionary[String, String]): dict.type = {
        dict.put("4", "4")
        dict.put("3", "3")
        dict.put("2", "2")
        dict.put("1", "1")
        dict
      }

      // identity transformation of inner type:

      initDictionary(new ju.Hashtable[String, String])
        .transformIntoPartial[Map[String, String]]
        .asOption
        .get ==> Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      locally {
        val p = new ju.Properties()
        p.put("4", "4")
        p.put("3", "3")
        p.put("2", "2")
        p.put("1", "1")
        p
      }
        .transformIntoPartial[Map[Any, Any]]
        .asOption
        .get ==> Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")

      // provided transformation of inner type:

      initDictionary(new ju.Hashtable[String, String])
        .transformIntoPartial[Map[Int, Int]]
        .asOption
        .get ==> Map(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)

      // failed parsing of inner type:

      locally {
        val p = new ju.Properties()
        p.put("a", "4")
        p.put("3", "b")
        p.put("c", "d")
        p.put("1", "1")
        p
      }.transformIntoPartial[Map[Int, Int]].asErrorPathMessageStrings ==> Iterable(
        "(a).keys(a)" -> "For input string: \"a\"",
        "(3)" -> "For input string: \"b\"",
        "(c).keys(c)" -> "For input string: \"c\"",
        "(c)" -> "For input string: \"d\""
      )
    }

    test("from java.util.Map types") {
      def initMap(map: ju.Map[String, String]): map.type = {
        map.put("4", "4")
        map.put("3", "3")
        map.put("2", "2")
        map.put("1", "1")
        map
      }
      def initBadMap(map: ju.Map[String, String]): map.type = {
        map.put("a", "4")
        map.put("3", "b")
        map.put("c", "d")
        map.put("1", "1")
        map
      }

      val unstableOutput = Map(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)
      val stableOutput = ListMap(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)
      val sortedOutput = SortedMap(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4)

      // identity transformation of inner type:

      initMap(new ju.HashMap[String, String]).transformIntoPartial[Map[String, String]].asOption.get ==> Map(
        "4" -> "4",
        "3" -> "3",
        "2" -> "2",
        "1" -> "1"
      )

      // provided transformation of inner type:

      initMap(new ju.HashMap[String, String]).transformIntoPartial[Map[Int, Int]].asOption.get ==> unstableOutput
      initMap(new ju.LinkedHashMap[String, String])
        .transformIntoPartial[ListMap[Int, Int]]
        .asOption
        .get
        .toVector ==> stableOutput.toVector
      initMap(new ju.TreeMap[String, String])
        .transformIntoPartial[SortedMap[Int, Int]]
        .asOption
        .get
        .toVector ==> sortedOutput.toVector

      // failed parsing of inner type:

      initBadMap(new ju.LinkedHashMap[String, String])
        .transformIntoPartial[ListMap[Int, Int]]
        .asErrorPathMessageStrings ==> Iterable(
        "(a).keys(a)" -> "For input string: \"a\"",
        "(3)" -> "For input string: \"b\"",
        "(c).keys(c)" -> "For input string: \"c\"",
        "(c)" -> "For input string: \"d\""
      )
    }

    test("from java.lang.Enum-supporting type") {
      val enumSet = ju.EnumSet.allOf(classOf[JavaEnum])
      enumSet.transformIntoPartial[Set[JavaEnum]].asOption.get ==> Set(JavaEnum.Red, JavaEnum.Green, JavaEnum.Blue)

      val enumMap = new ju.EnumMap[JavaEnum, String](classOf[JavaEnum])
      JavaEnum.values().foreach(e => enumMap.put(e, e.ordinal().toString))

      // identity transformation of inner type:

      enumMap.transformIntoPartial[Map[JavaEnum, String]].asOption.get ==> Map(
        JavaEnum.Red -> "0",
        JavaEnum.Green -> "1",
        JavaEnum.Blue -> "2"
      )

      // provided transformation of inner type:

      enumMap
        .transformIntoPartial[Map[JavaEnum, Int]]
        .asOption
        .get ==> Map(JavaEnum.Red -> 0, JavaEnum.Green -> 1, JavaEnum.Blue -> 2)

      // failed parsing of inner type:

      val enumMap2 = new ju.EnumMap[JavaEnum, String](classOf[JavaEnum])
      enumMap2.put(JavaEnum.Red, "a")
      enumMap2.put(JavaEnum.Green, "1")
      enumMap2.put(JavaEnum.Blue, "b")
      enumMap2
        .transformIntoPartial[Map[JavaEnum, Int]]
        .asErrorPathMessageStrings ==> Iterable(
        "(Red)" -> "For input string: \"a\"",
        "(Blue)" -> "For input string: \"b\""
      )
    }

    test("from java.util.BitSet type") {
      val input = new ju.BitSet
      input.set(1)
      input.set(2)
      input.set(3)
      input.set(4)

      // identity transformation of inner type:

      input.transformIntoPartial[Set[Int]].asOption.get ==> Set(1, 2, 3, 4)

      // provided transformation of inner type:

      implicit val intToString: io.scalaland.chimney.Transformer[Int, String] = _.toString
      input.transformIntoPartial[Set[String]].asOption.get ==> Set("1", "2", "3", "4")
    }

    test("from java.util.stream.BaseStream types") {
      // identity transformation of inner type:

      ju.stream.Stream.of("4", "3", "2", "1").transformIntoPartial[List[String]].asOption.get ==> List(
        "4",
        "3",
        "2",
        "1"
      )
      ju.stream.IntStream.of(4, 3, 2, 1).transformIntoPartial[List[Int]].asOption.get ==> List(4, 3, 2, 1)
      ju.stream.LongStream.of(4L, 3L, 2L, 1L).transformIntoPartial[List[Long]].asOption.get ==> List(4L, 3L, 2L, 1L)
      ju.stream.DoubleStream.of(4.0, 3.0, 2.0, 1.0).transformIntoPartial[List[Double]].asOption.get ==> List(
        4.0,
        3.0,
        2.0,
        1.0
      )

      // provided transformation of inner type:

      implicit def longToInt: Transformer[Long, Int] = _.toInt
      implicit def doubleToInt: Transformer[Double, Int] = _.toInt
      ju.stream.Stream.of("4", "3", "2", "1").transformIntoPartial[List[Int]].asOption.get ==> List(4, 3, 2, 1)
      ju.stream.LongStream.of(4L, 3L, 2L, 1L).transformIntoPartial[List[Int]].asOption.get ==> List(4, 3, 2, 1)
      ju.stream.DoubleStream.of(4.0, 3.0, 2.0, 1.0).transformIntoPartial[List[Int]].asOption.get ==> List(4, 3, 2, 1)

      // failed parsing of inner type:

      ju.stream.Stream
        .of("a", "3", "b", "1")
        .transformIntoPartial[List[Int]]
        .asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
    }
  }

  group("conversion from Java types to Java types") {

    test("for java.util.Optional type") {
      // identity transformation of inner type:

      ju.Optional.of("1").transformIntoPartial[ju.Optional[String]].asOption.get ==> ju.Optional.of("1")
      ju.Optional.empty[String]().transformIntoPartial[ju.Optional[String]].asOption.get ==> ju.Optional.empty[Int]()
      ju.Optional.of("1").transformIntoPartial[String].asOption ==> Some("1")
      ju.Optional.empty[String]().transformIntoPartial[String].asOption ==> None

      // provided transformation of inner type:

      ju.Optional.of("1").transformIntoPartial[ju.Optional[Int]].asOption.get ==> ju.Optional.of(1)
      ju.Optional.empty[String]().transformIntoPartial[ju.Optional[Int]].asOption.get ==> ju.Optional.empty[Int]()
      ju.Optional.of("1").transformIntoPartial[Int].asOption ==> Some(1)
      ju.Optional.of("A").transformIntoPartial[Int].asOption ==> None
      ju.Optional.empty[String]().transformIntoPartial[Int].asOption ==> None
    }

    test("for java.util.Iterator type") {
      val input = new ju.ArrayList[String]
      input.add("4")
      input.add("3")
      input.add("2")
      input.add("1")

      // identity transformation of inner type:

      input.transformIntoPartial[ju.Iterator[String]].asOption.get.asScala.toList ==> List("4", "3", "2", "1")
      input.iterator().transformIntoPartial[ju.List[String]].asOption.get.asScala.toList ==> List("4", "3", "2", "1")

      // provided transformation of inner type:

      input.iterator().transformIntoPartial[ju.Iterator[Int]].asOption.get.asScala.toList ==> List(4, 3, 2, 1)
    }

    test("for java.util.Enumeration type") {
      val input = new ju.Vector[String]
      input.add("4")
      input.add("3")
      input.add("2")
      input.add("1")

      // identity transformation of inner type:

      input.transformIntoPartial[ju.List[String]].asOption.get.asScala.toList ==> List("4", "3", "2", "1")
      input.elements().transformIntoPartial[ju.List[String]].asOption.get.asScala.toList ==> List("4", "3", "2", "1")

      // provided transformation of inner type:

      input.elements().transformIntoPartial[ju.Enumeration[Int]].asOption.get.asScala.toList ==> List(4, 3, 2, 1)
    }

    test("for java.util.Collection types") {
      val input = new ju.ArrayList[String]
      input.add("4")
      input.add("3")
      input.add("2")
      input.add("1")
      val outputStable = List(4, 3, 2, 1)
      val outputUnstable = Set(4, 3, 2, 1)
      val outputSorted = List(1, 2, 3, 4)

      // identity transformation of inner type:

      input.transformIntoPartial[ju.Collection[String]].asOption.get.asScala.toList ==> List("4", "3", "2", "1")

      // provided transformation of inner type:

      input.transformIntoPartial[ju.Collection[Int]].asOption.get.asScala.toSet ==> outputUnstable
      input.transformIntoPartial[ju.AbstractCollection[Int]].asOption.get.asScala.toSet ==> outputUnstable

      input.transformIntoPartial[ju.List[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.AbstractList[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.AbstractSequentialList[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.ArrayList[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.LinkedList[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.Vector[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.Stack[Int]].asOption.get.asScala.toList ==> outputStable

      input.transformIntoPartial[ju.Deque[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.ArrayDeque[Int]].asOption.get.asScala.toList ==> outputStable

      input.transformIntoPartial[ju.Queue[Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.AbstractQueue[Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.PriorityQueue[Int]].asOption.get.asScala.toList ==> outputSorted

      input.transformIntoPartial[ju.Set[Int]].asOption.get.asScala.toSet ==> outputUnstable
      input.transformIntoPartial[ju.AbstractSet[Int]].asOption.get.asScala.toSet ==> outputUnstable
      input.transformIntoPartial[ju.SortedSet[Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.NavigableSet[Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.HashSet[Int]].asOption.get.asScala.toSet ==> outputUnstable
      input.transformIntoPartial[ju.LinkedHashSet[Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.TreeSet[Int]].asOption.get.asScala.toList ==> outputSorted
    }

    test("for java.util.Dictionary types") {
      val input = new ju.LinkedHashMap[String, String]
      input.put("4", "4")
      input.put("3", "3")
      input.put("2", "2")
      input.put("1", "1")
      val output = Map(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)

      // identity transformation of inner type:

      input.transformIntoPartial[ju.Dictionary[String, String]].asOption.get.asScala ==> input.asScala.toMap

      // provided transformation of inner type:

      input.transformIntoPartial[ju.Dictionary[Int, Int]].asOption.get.asScala ==> output
      (input.transformIntoPartial[ju.Hashtable[Int, Int]]: partial.Result[
        ju.Dictionary[Int, Int]
      ]).asOption.get.asScala ==> output
    }

    test("for java.util.Map types") {
      val input = new ju.LinkedHashMap[String, String]
      input.put("4", "4")
      input.put("3", "3")
      input.put("2", "2")
      input.put("1", "1")
      val outputStable = List(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)
      val outputUnstable = Map(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)
      val outputSorted = List(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4)

      // identity transformation of inner type:

      input.transformIntoPartial[ju.Map[String, String]].asOption.get.asScala ==> input.asScala.toMap

      // provided transformation of inner type:

      input.transformIntoPartial[ju.Map[Int, Int]].asOption.get.asScala ==> outputUnstable
      input.transformIntoPartial[ju.AbstractMap[Int, Int]].asOption.get.asScala ==> outputUnstable
      input.transformIntoPartial[ju.SortedMap[Int, Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.NavigableMap[Int, Int]].asOption.get.asScala.toList ==> outputSorted
      input.transformIntoPartial[ju.HashMap[Int, Int]].asOption.get.asScala ==> outputUnstable
      input.transformIntoPartial[ju.LinkedHashMap[Int, Int]].asOption.get.asScala.toList ==> outputStable
      input.transformIntoPartial[ju.TreeMap[Int, Int]].asOption.get.asScala.toList ==> outputSorted
    }

    test("for java.util.stream.BaseStream types") {
      // identity transformation of inner type:

      ju.stream.Stream.of(4, 3, 2, 1).transformIntoPartial[ju.stream.IntStream].asOption.get.toScala(List) ==> List(
        4,
        3,
        2,
        1
      )
      ju.stream.Stream
        .of(4L, 3L, 2L, 1L)
        .transformIntoPartial[ju.stream.LongStream]
        .asOption
        .get
        .toScala(List) ==> List(4L, 3L, 2L, 1L)
      ju.stream.Stream
        .of(4.0, 3.0, 2.0, 1.0)
        .transformIntoPartial[ju.stream.DoubleStream]
        .asOption
        .get
        .toScala(List) ==> List(4.0, 3.0, 2.0, 1.0)
      ju.stream.IntStream
        .of(4, 3, 2, 1)
        .transformIntoPartial[ju.stream.Stream[Int]]
        .asOption
        .get
        .toScala(List) ==> List(4, 3, 2, 1)
      ju.stream.LongStream
        .of(4L, 3L, 2L, 1L)
        .transformIntoPartial[ju.stream.Stream[Long]]
        .asOption
        .get
        .toScala(List) ==> List(4L, 3L, 2L, 1L)
      ju.stream.DoubleStream
        .of(4.0, 3.0, 2.0, 1.0)
        .transformIntoPartial[ju.stream.Stream[Double]]
        .asOption
        .get
        .toScala(List) ==> List(4.0, 3.0, 2.0, 1.0)

      // provided transformation of inner type:

      implicit def longToInt: Transformer[Long, Int] = _.toInt
      implicit def doubleToInt: Transformer[Double, Int] = _.toInt
      ju.stream.Stream
        .of("4", "3", "2", "1")
        .transformIntoPartial[ju.stream.Stream[Int]]
        .asOption
        .get
        .toScala(List) ==> List(4, 3, 2, 1)
      ju.stream.Stream
        .of("4", "3", "2", "1")
        .transformIntoPartial[ju.stream.IntStream]
        .asOption
        .get
        .toScala(List) ==> List(4, 3, 2, 1)
      ju.stream.LongStream
        .of(4L, 3L, 2L, 1L)
        .transformIntoPartial[ju.stream.Stream[Int]]
        .asOption
        .get
        .toScala(List) ==> List(4, 3, 2, 1)
      ju.stream.LongStream
        .of(4L, 3L, 2L, 1L)
        .transformIntoPartial[ju.stream.IntStream]
        .asOption
        .get
        .toScala(List) ==> List(4, 3, 2, 1)
      ju.stream.DoubleStream
        .of(4.0, 3.0, 2.0, 1.0)
        .transformIntoPartial[ju.stream.Stream[Int]]
        .asOption
        .get
        .toScala(List) ==> List(4, 3, 2, 1)
      ju.stream.DoubleStream
        .of(4.0, 3.0, 2.0, 1.0)
        .transformIntoPartial[ju.stream.IntStream]
        .asOption
        .get
        .toScala(List) ==> List(4, 3, 2, 1)

      // failed parsing of inner type:

      ju.stream.Stream
        .of("a", "3", "b", "1")
        .transformIntoPartial[ju.stream.Stream[Int]]
        .asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
      ju.stream.Stream
        .of("a", "3", "b", "1")
        .transformIntoPartial[ju.stream.IntStream]
        .asErrorPathMessageStrings ==> Iterable(
        "(0)" -> "For input string: \"a\"",
        "(2)" -> "For input string: \"b\""
      )
    }
  }
}
