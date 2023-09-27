package io.scalaland.chimney.javacollections

import java.util as ju
import io.scalaland.chimney.{ChimneySpec, Transformer}
import io.scalaland.chimney.dsl.*

import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters.*

class TotalTransformerJavaCollectionsConversionsSpec extends ChimneySpec {

  // TODO: options

  // TODO iterator and enumeration
  // Seq(4,3,2,1).transformInto[ju.Iterator[String]].asScala.toList ==> List("4","3","2","1")
  // Seq(4,3,2,1).transformInto[ju.Enumeration[String]].asScala.toList ==> List("4","3","2","1")

  group("conversion from Scala types to java.util.Collection types") {

    test("to non-Map types") {
      implicit val intToString: Transformer[Int, String] = _.toString

      val input = Seq(4, 3, 2, 1)
      val outputStable = List("4", "3", "2", "1")
      val outputUnstable = Set("4", "3", "2", "1")
      val outputSorted = List("1", "2", "3", "4")

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

      input.transformInto[ju.Dictionary[String, String]].asScala ==> output
      (input.transformInto[ju.Hashtable[String, String]]: ju.Dictionary[String, String]).asScala ==> output
    }

    test("to java.util.Map types") {
      implicit val intToString: Transformer[Int, String] = _.toString

      val input = ListMap(4 -> 4, 3 -> 3, 2 -> 2, 1 -> 1)
      val outputStable = List("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val outputUnstable = Map("4" -> "4", "3" -> "3", "2" -> "2", "1" -> "1")
      val outputSorted = List("1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4")

      input.transformInto[ju.Map[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.AbstractMap[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.SortedMap[String, String]].asScala.toList ==> outputSorted
      input.transformInto[ju.NavigableMap[String, String]].asScala.toList ==> outputSorted
      input.transformInto[ju.HashMap[String, String]].asScala ==> outputUnstable
      input.transformInto[ju.LinkedHashMap[String, String]].asScala.toList ==> outputStable
      input.transformInto[ju.TreeMap[String, String]].asScala.toList ==> outputSorted
    }

    test("to java.lang.Enum-supporting types".ignore) {} // TODO
  }

  group("conversion from java.util.Collection types to Scala types") {

    test("from non-Map types".ignore) {} // TODO
    test("from java.util.Dictionary types".ignore) {} // TODO
    test("from java.util.Map types".ignore) {} // TODO
  }

  group("conversion from java.util.Collection types to java.util.Collection types") {

    test("for non-Map types".ignore) {} // TODO
    test("for java.util.Dictionary types".ignore) {} // TODO
    test("for java.util.Map types".ignore) {} // TODO
  }

  group("java.util.BitSet conversions") {

    test("from Scala types".ignore) {} // TODO
    test("to Scala types".ignore) {} // TODO
  }
}
