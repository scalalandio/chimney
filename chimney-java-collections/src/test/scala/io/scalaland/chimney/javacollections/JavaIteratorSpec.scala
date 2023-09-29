package io.scalaland.chimney.javacollections

import java.util as ju
import io.scalaland.chimney.ChimneySpec
import io.scalaland.chimney.fixtures.JavaEnum

import scala.jdk.CollectionConverters.*

class JavaIteratorSpec extends ChimneySpec {

  test("java.util.Iterator instance should be resolved") {
    var counter = 0
    implicitly[JavaIterator[Int, ju.Iterator[Int]]].foreach((0 to 10).iterator.asJava)(counter += _)
    counter ==> 55
  }

  test("java.util.Enumeration instance should be resolved") {
    var counter = 0
    implicitly[JavaIterator[Int, ju.Enumeration[Int]]].foreach((0 to 10).iterator.asJavaEnumeration)(counter += _)
    counter ==> 55
  }

  test("java.util.Collection instances should be resolved for both concrete and abstract types") {
    def populateThenIterateOverAndVerify[CC[A0] <: ju.Collection[A0]](empty: => CC[Int])(implicit
        iterator: JavaIterator[Int, CC[Int]]
    ): Unit = {
      val collection = empty
      (0 to 10).foreach(collection.add(_))
      var counter = 0
      iterator.foreach(collection)(counter += _)
      counter ==> 55
    }

    populateThenIterateOverAndVerify[ju.Collection](new ju.ArrayList[Int])
    populateThenIterateOverAndVerify[ju.AbstractCollection](new ju.ArrayList[Int])

    populateThenIterateOverAndVerify[ju.List](new ju.ArrayList[Int])
    populateThenIterateOverAndVerify[ju.AbstractList](new ju.ArrayList[Int])
    populateThenIterateOverAndVerify[ju.AbstractSequentialList](new ju.LinkedList[Int])
    populateThenIterateOverAndVerify[ju.ArrayList](new ju.ArrayList[Int])
    populateThenIterateOverAndVerify[ju.LinkedList](new ju.LinkedList[Int])
    populateThenIterateOverAndVerify[ju.Vector](new ju.Vector[Int])
    populateThenIterateOverAndVerify[ju.Stack](new ju.Stack[Int])

    populateThenIterateOverAndVerify[ju.Deque](new ju.ArrayDeque[Int])
    populateThenIterateOverAndVerify[ju.ArrayDeque](new ju.ArrayDeque[Int])

    populateThenIterateOverAndVerify[ju.Queue](new ju.PriorityQueue[Int])
    populateThenIterateOverAndVerify[ju.AbstractQueue](new ju.PriorityQueue[Int])
    populateThenIterateOverAndVerify[ju.PriorityQueue](new ju.PriorityQueue[Int])

    populateThenIterateOverAndVerify[ju.Set](new ju.HashSet[Int])
    populateThenIterateOverAndVerify[ju.AbstractSet](new ju.HashSet[Int])
    populateThenIterateOverAndVerify[ju.SortedSet](new ju.TreeSet[Int])
    populateThenIterateOverAndVerify[ju.NavigableSet](new ju.TreeSet[Int])
    populateThenIterateOverAndVerify[ju.HashSet](new ju.HashSet[Int])
    populateThenIterateOverAndVerify[ju.LinkedHashSet](new ju.LinkedHashSet[Int])
    populateThenIterateOverAndVerify[ju.TreeSet](new ju.TreeSet[Int])
  }

  test("java.util.Dictionary instances should be resolved for both concrete and abstract types") {
    def populateThenIterateOverAndVerify[CC[K0, V0] <: ju.Dictionary[K0, V0]](empty: => CC[Int, Int])(implicit
        iterator: JavaIterator[(Int, Int), CC[Int, Int]]
    ): Unit = {
      val collection = empty
      (0 to 10).foreach(i => collection.put(i, i))
      var counter = 0
      iterator.foreach(collection) { case (k, v) => counter += (k + v) }
      counter ==> 110
    }

    populateThenIterateOverAndVerify[ju.Dictionary](new ju.Hashtable[Int, Int])
    populateThenIterateOverAndVerify[ju.Hashtable](new ju.Hashtable[Int, Int])
  }

  test("java.util.Map instances should be resolved for both concrete and abstract types") {
    def populateThenIterateOverAndVerify[CC[K0, V0] <: ju.Map[K0, V0]](empty: => CC[Int, Int])(implicit
        iterator: JavaIterator[(Int, Int), CC[Int, Int]]
    ): Unit = {
      val collection = empty
      (0 to 10).foreach(i => collection.put(i, i))
      var counter = 0
      iterator.foreach(collection) { case (k, v) => counter += (k + v) }
      counter ==> 110
    }

    populateThenIterateOverAndVerify[ju.Map](new ju.HashMap[Int, Int])
    populateThenIterateOverAndVerify[ju.AbstractMap](new ju.HashMap[Int, Int])
    populateThenIterateOverAndVerify[ju.SortedMap](new ju.TreeMap[Int, Int])
    populateThenIterateOverAndVerify[ju.NavigableMap](new ju.TreeMap[Int, Int])
    populateThenIterateOverAndVerify[ju.HashMap](new ju.HashMap[Int, Int])
    populateThenIterateOverAndVerify[ju.LinkedHashMap](new ju.LinkedHashMap[Int, Int])
    populateThenIterateOverAndVerify[ju.WeakHashMap](new ju.WeakHashMap[Int, Int])
    populateThenIterateOverAndVerify[ju.TreeMap](new ju.TreeMap[Int, Int])
  }

  test("java.util.Collection instances should be resolved for java.lang.Enum specializations") {
    val enumSet = ju.EnumSet.allOf(classOf[JavaEnum])
    var counter = 0
    implicitly[JavaIterator[JavaEnum, ju.EnumSet[JavaEnum]]].foreach(enumSet)(_ => counter += 1)
    counter ==> 3

    val enumMap = new ju.EnumMap[JavaEnum, Int](classOf[JavaEnum])
    JavaEnum.values().zipWithIndex.foreach { case (e, i) => enumMap.put(e, i + 1) }
    counter = 0
    implicitly[JavaIterator[(JavaEnum, Int), ju.EnumMap[JavaEnum, Int]]].foreach(enumMap) { case (_, v) =>
      counter += v
    }
    counter ==> 6
  }

  test("java.util.Collection instances should be resolved for java.util.Properties specialization") {
    val properties = new ju.Properties()
    (0 to 10).foreach(i => properties.put(i.toString, i.toString))
    var counter = 0
    implicitly[JavaIterator[(String, String), ju.Properties]].foreach(properties) { case (k, v) =>
      counter += (k.toInt + v.toInt)
    }
    counter ==> 110
  }

  test("java.util.BitSet instance should be resolved") {
    val bitSet = new ju.BitSet
    (0 to 10).foreach(bitSet.set(_))
    var counter = 0
    implicitly[JavaIterator[Int, ju.BitSet]].foreach(bitSet)(counter += _)
    counter ==> 55
  }
}
