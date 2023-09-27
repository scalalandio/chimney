package io.scalaland.chimney.javacollections

import java.util as ju
import io.scalaland.chimney.{ChimneySpec, Transformer}
import io.scalaland.chimney.dsl.*

import scala.collection.immutable.{ListMap, ListSet, SortedMap}
import scala.jdk.CollectionConverters.*

class TotalTransformerJavaCollectionsConversionsSpec extends ChimneySpec {

  group("conversion from Scala types to Java types") {

    // TODO: to java.Optional
    // TODO: to java.util.Iterator
    // TODO: to java.util.Enumeration

    test("to java.util.Collection types") {
      implicit val intToString: Transformer[Int, String] = _.toString

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
      implicit val intToString: Transformer[Int, String] = _.toString

      val input = ListMap(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)
      val output = Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")

      // identity transformation of inner type:

      input.transformInto[ju.Dictionary[Int, Int]].asScala ==> input.toMap

      // provided transformation of inner type:

      input.transformInto[ju.Dictionary[String, String]].asScala ==> output
      (input.transformInto[ju.Hashtable[String, String]]: ju.Dictionary[String, String]).asScala ==> output
    }

    test("to java.util.Map types") {
      implicit val intToString: Transformer[Int, String] = _.toString

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

    test("to java.lang.Enum-supporting types".ignore) {} // TODO: how to express type bounds in implicit def?
  }

  group("conversion from Java types to Scala types") {

    // TODO: from java.util.Optional
    // TODO: from java.util.Iterator
    // TODO: from java.util.Enumeration

    test("from java.util.Collection types".ignore) {
      implicit val intToString: Transformer[Int, String] = _.toString

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

      ju.List.of(4, 3, 2, 1).transformInto[List[String]] ==> outputStable
      initCollection(new ju.ArrayList[Int]).transformInto[List[String]] ==> outputStable
      initCollection(new ju.LinkedList[Int]).transformInto[List[String]] ==> outputStable
      initCollection(new ju.Vector[Int]).transformInto[List[String]] ==> outputStable
      initCollection(new ju.Stack[Int]).transformInto[List[String]] ==> outputStable

      initCollection(new ju.ArrayDeque[Int]).transformInto[List[String]] ==> outputStable

      initCollection(new ju.PriorityQueue[Int]).transformInto[List[String]] ==> outputSorted

      ju.Set.of(4, 3, 2, 1).transformInto[Set[String]] ==> outputUnstable
      initCollection(new ju.HashSet[Int]).transformInto[Set[String]] ==> outputUnstable
      initCollection(new ju.TreeSet[Int]).transformInto[ListSet[String]].toList ==> outputSorted
    }

    test("from java.util.Dictionary types") {
      implicit val intToString: Transformer[Int, String] = _.toString

      def initDictionary(dict: ju.Dictionary[Int, Int]): dict.type = {
        dict.put(4, 4)
        dict.put(3, 3)
        dict.put(2, 2)
        dict.put(1, 1)
        dict
      }

      initDictionary(new ju.Hashtable[Int, Int])
        .transformInto[Map[String, String]] ==> Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
    }

    test("from java.util.Map types") {
      implicit val intToString: Transformer[Int, String] = _.toString

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

      ju.Map.of(4, 4, 3, 3, 2, 2, 1, 1).transformInto[Map[Int, Int]] ==> Map(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)

      // provided transformation of inner type:

      ju.Map.of(4, 4, 3, 3, 2, 2, 1, 1).transformInto[Map[String, String]] ==> unstableOutput
      initMap(new ju.HashMap[Int, Int]).transformInto[Map[String, String]] ==> unstableOutput
      initMap(new ju.LinkedHashMap[Int, Int]).transformInto[ListMap[String, String]].toVector ==> stableOutput.toVector
      initMap(new ju.TreeMap[Int, Int]).transformInto[SortedMap[String, String]].toVector ==> sortedOutput.toVector
    }
  }

  group("conversion from Java types to Java types") {

    // TODO: for java.util.Optional
    test("for java.util.Collection types".ignore) {} // TODO
    test("for java.util.Dictionary types".ignore) {} // TODO
    test("for java.util.Map types".ignore) {} // TODO
  }

  group("java.util.BitSet conversions") {

    test("from Scala types".ignore) {} // TODO
    test("to Scala types".ignore) {} // TODO
  }
}
