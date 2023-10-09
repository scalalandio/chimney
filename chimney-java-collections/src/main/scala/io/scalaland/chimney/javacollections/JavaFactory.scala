package io.scalaland.chimney.javacollections

import java.util as ju
import scala.collection.compat.*
import scala.jdk.CollectionConverters.*
import scala.reflect.{classTag, ClassTag}

/** Interface dedicated to Java collections allowing generalisations similar to Scala's `Factory`.
  *
  * It is a separate interface to prevent overlapping implicits in case someone already created Factory instances for
  * Java's collections.
  *
  * Contrary to Scala's `Factory` it is invariant, because we have no possibility to put instances in companion objects
  * and implicit priorities didn't work as a mechanism of preventing ambiguities when upcasting.
  *
  * @tparam A  collection's element's type
  * @tparam CC collection's type with applied type parameters
  *
  * @since 0.8.0
  */
trait JavaFactory[A, CC] {

  def fromIterator(it: ju.Iterator[A]): CC
  def newBuilder: JavaFactory.Builder[A, CC]

  final def fromSpecific(it: IterableOnce[A]): CC = fromIterator(it.iterator.asJava)
  final def narrow[CC2 >: CC]: JavaFactory[A, CC2] = this.asInstanceOf[JavaFactory[A, CC2]]
}

/** @since 0.8.0 */
object JavaFactory {

  /** @since 0.8.0 */
  trait Builder[A, CC] {
    def addOne(a: A): Unit

    def result(): CC
  }

  /** Import content to allow using JavaFactory as [[scala.collection.compat.Factory]].
    *
    * @since 0.8.0
    */
  object ConversionToScalaFactory extends JavaFactoryCompat

  final class IteratorFactory[A] extends JavaFactory[A, ju.Iterator[A]] {

    def fromIterator(it: ju.Iterator[A]): ju.Iterator[A] = it

    def newBuilder: Builder[A, ju.Iterator[A]] = new Builder[A, ju.Iterator[A]] {
      private val collection = new ju.ArrayList[A]()

      def addOne(a: A): Unit = {
        collection.add(a); ()
      }

      def result(): ju.Iterator[A] = collection.iterator()
    }
  }

  final class EnumerationFactory[A] extends JavaFactory[A, ju.Enumeration[A]] {

    def fromIterator(it: ju.Iterator[A]): ju.Enumeration[A] = new ju.Enumeration[A] {
      def hasMoreElements: Boolean = it.hasNext()

      def nextElement(): A = it.next()
    }

    def newBuilder: Builder[A, ju.Enumeration[A]] = new Builder[A, ju.Enumeration[A]] {
      private val collection = new ju.ArrayList[A]()

      def addOne(a: A): Unit = {
        collection.add(a); ()
      }

      def result(): ju.Enumeration[A] = fromIterator(collection.iterator())
    }
  }

  final class CollectionFactory[A, CC[A1] <: ju.Collection[A1]](
      create: => CC[A]
  ) extends JavaFactory[A, CC[A]] {
    def fromIterator(it: ju.Iterator[A]): CC[A] = {
      val collection = create
      while (it.hasNext()) {
        collection.add(it.next())
        ()
      }
      collection
    }

    def newBuilder: Builder[A, CC[A]] = new Builder[A, CC[A]] {
      private val collection = create

      final def addOne(a: A): Unit = {
        collection.add(a); ()
      }

      final def result(): CC[A] = collection
    }
  }

  final class MapFactory[K, V, CC[K1, V1] <: ju.Map[K1, V1]](
      create: => CC[K, V]
  ) extends JavaFactory[(K, V), CC[K, V]] {
    def fromIterator(it: ju.Iterator[(K, V)]): CC[K, V] = {
      val collection = create
      while (it.hasNext()) {
        val (k, v) = it.next()
        collection.put(k, v)
        ()
      }
      collection
    }

    def newBuilder: Builder[(K, V), CC[K, V]] = new Builder[(K, V), CC[K, V]] {
      private val collection = create

      final def addOne(pair: (K, V)): Unit = {
        collection.put(pair._1, pair._2); ()
      }

      final def result(): CC[K, V] = collection
    }
  }

  final class BitSetFactory extends JavaFactory[Int, ju.BitSet] {
    def fromIterator(it: ju.Iterator[Int]): ju.BitSet = {
      val collection = new ju.BitSet()
      while (it.hasNext()) {
        collection.set(it.next())
        ()
      }
      collection
    }

    def newBuilder: Builder[Int, ju.BitSet] = new Builder[Int, ju.BitSet] {
      private val collection = new ju.BitSet()

      final def addOne(a: Int): Unit = {
        collection.set(a); ()
      }

      final def result(): ju.BitSet = collection
    }
  }

  final class StreamBuilder[A] extends JavaFactory[A, ju.stream.Stream[A]] {
    def fromIterator(it: ju.Iterator[A]): ju.stream.Stream[A] =
      ju.stream.StreamSupport.stream(ju.Spliterators.spliteratorUnknownSize(it, 0), false)

    def newBuilder: Builder[A, ju.stream.Stream[A]] = new Builder[A, ju.stream.Stream[A]] {
      private val inner = ju.stream.Stream.builder[A]()

      final def addOne(a: A): Unit = {
        inner.add(a); ()
      }

      final def result(): ju.stream.Stream[A] = inner.build()
    }
  }

  final class IntStreamBuilder extends JavaFactory[Int, ju.stream.IntStream] {
    def fromIterator(it: ju.Iterator[Int]): ju.stream.IntStream =
      ju.stream.StreamSupport.stream(ju.Spliterators.spliteratorUnknownSize(it, 0), false).mapToInt(a => a)

    def newBuilder: Builder[Int, ju.stream.IntStream] = new Builder[Int, ju.stream.IntStream] {
      private val inner = ju.stream.IntStream.builder()

      final def addOne(a: Int): Unit = {
        inner.add(a); ()
      }

      final def result(): ju.stream.IntStream = inner.build()
    }
  }

  final class LongStreamBuilder extends JavaFactory[Long, ju.stream.LongStream] {
    def fromIterator(it: ju.Iterator[Long]): ju.stream.LongStream =
      ju.stream.StreamSupport.stream(ju.Spliterators.spliteratorUnknownSize(it, 0), false).mapToLong(a => a)

    def newBuilder: Builder[Long, ju.stream.LongStream] = new Builder[Long, ju.stream.LongStream] {
      private val inner = ju.stream.LongStream.builder()

      final def addOne(a: Long): Unit = {
        inner.add(a); ()
      }

      final def result(): ju.stream.LongStream = inner.build()
    }
  }

  final class DoubleStreamBuilder extends JavaFactory[Double, ju.stream.DoubleStream] {
    def fromIterator(it: ju.Iterator[Double]): ju.stream.DoubleStream =
      ju.stream.StreamSupport.stream(ju.Spliterators.spliteratorUnknownSize(it, 0), false).mapToDouble(a => a)

    def newBuilder: Builder[Double, ju.stream.DoubleStream] = new Builder[Double, ju.stream.DoubleStream] {
      private val inner = ju.stream.DoubleStream.builder()

      final def addOne(a: Double): Unit = {
        inner.add(a); ()
      }

      final def result(): ju.stream.DoubleStream = inner.build()
    }
  }

  // java.util.Iterator

  /** @since 0.8.0 */
  implicit def javaFactoryForIterator[A]: JavaFactory[A, ju.Iterator[A]] = new IteratorFactory[A]

  // java.util.Enumeration

  /** @since 0.8.0 */
  implicit def javaFactoryForEnumeration[A]: JavaFactory[A, ju.Enumeration[A]] = new EnumerationFactory[A]

  // java.util.Collection

  /** @since 0.8.0 */
  implicit def javaFactoryForCollection[A]: JavaFactory[A, ju.Collection[A]] =
    javaFactoryForAbstractCollection[A].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForAbstractCollection[A]: JavaFactory[A, ju.AbstractCollection[A]] =
    javaFactoryForArrayList[A].narrow

  // java.util.List

  /** @since 0.8.0 */
  implicit def javaFactoryForList[A]: JavaFactory[A, ju.List[A]] = javaFactoryForAbstractList[A].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForAbstractList[A]: JavaFactory[A, ju.AbstractList[A]] =
    javaFactoryForAbstractSequentialList[A].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForAbstractSequentialList[A]: JavaFactory[A, ju.AbstractSequentialList[A]] =
    javaFactoryForLinkedList[A].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForArrayList[A]: JavaFactory[A, ju.ArrayList[A]] = new CollectionFactory(new ju.ArrayList[A])

  /** @since 0.8.0 */
  implicit def javaFactoryForLinkedList[A]: JavaFactory[A, ju.LinkedList[A]] = new CollectionFactory(
    new ju.LinkedList[A]
  )

  /** @since 0.8.0 */
  implicit def javaFactoryForVector[A]: JavaFactory[A, ju.Vector[A]] = new CollectionFactory(new ju.Vector[A])

  /** @since 0.8.0 */
  implicit def javaFactoryForStack[A]: JavaFactory[A, ju.Stack[A]] = new CollectionFactory(new ju.Stack[A])

  // java.util.Deque

  /** @since 0.8.0 */
  implicit def javaFactoryForDeque[A]: JavaFactory[A, ju.Deque[A]] = javaFactoryForArrayDeque[A].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForArrayDeque[A]: JavaFactory[A, ju.ArrayDeque[A]] = new CollectionFactory(
    new ju.ArrayDeque[A]
  )

  // java.util.Queue

  /** @since 0.8.0 */
  implicit def javaFactoryForQueue[A: Ordering]: JavaFactory[A, ju.Queue[A]] =
    javaFactoryForAbstractQueue[A].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForAbstractQueue[A: Ordering]: JavaFactory[A, ju.AbstractQueue[A]] =
    javaFactoryForPriorityQueue[A].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForPriorityQueue[A: Ordering]: JavaFactory[A, ju.PriorityQueue[A]] = new CollectionFactory(
    new ju.PriorityQueue[A](Ordering[A])
  )

  // java.util.Set

  /** @since 0.8.0 */
  implicit def javaFactoryForSet[A]: JavaFactory[A, ju.Set[A]] = javaFactoryForAbstractSet[A].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForAbstractSet[A]: JavaFactory[A, ju.AbstractSet[A]] = javaFactoryForHashSet[A].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForSortedSet[A: Ordering]: JavaFactory[A, ju.SortedSet[A]] =
    javaFactoryForTreeSet[A].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForNavigableSet[A: Ordering]: JavaFactory[A, ju.NavigableSet[A]] =
    javaFactoryForTreeSet[A].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForHashSet[A]: JavaFactory[A, ju.HashSet[A]] = new CollectionFactory(new ju.HashSet[A])

  /** @since 0.8.0 */
  implicit def javaFactoryForLinkedHashSet[A]: JavaFactory[A, ju.LinkedHashSet[A]] = new CollectionFactory(
    new ju.LinkedHashSet[A]
  )

  /** @since 0.8.0 */
  implicit def javaFactoryForTreeSet[A: Ordering]: JavaFactory[A, ju.TreeSet[A]] = new CollectionFactory(
    new ju.TreeSet[A](Ordering[A])
  )

  /** @since 0.8.0 */
  implicit def javaFactoryForEnumSet[A <: java.lang.Enum[A]: ClassTag]: JavaFactory[A, ju.EnumSet[A]] =
    new CollectionFactory[A, ju.Set](ju.EnumSet.noneOf[A](classTag[A].runtimeClass.asInstanceOf[Class[A]]))
      .asInstanceOf[JavaFactory[A, ju.EnumSet[A]]]

  // java.util.BitSet

  /** @since 0.8.0 */
  implicit val javaFactoryForBitSet: JavaFactory[Int, ju.BitSet] = new BitSetFactory

  // java.util.Dictionary

  /** @since 0.8.0 */
  implicit def javaFactoryForDictionary[K, V]: JavaFactory[(K, V), ju.Dictionary[K, V]] =
    javaFactoryForHashtable[K, V].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForHashtable[K, V]: JavaFactory[(K, V), ju.Hashtable[K, V]] = new MapFactory(
    new ju.Hashtable[K, V]
  )

  /** @since 0.8.0 */
  implicit val javaFactoryForProperties: JavaFactory[(String, String), ju.Properties] = new MapFactory(
    new ju.Properties()
  ).asInstanceOf[JavaFactory[(String, String), ju.Properties]]

  // java.util.Map

  /** @since 0.8.0 */
  implicit def javaFactoryForMap[K, V]: JavaFactory[(K, V), ju.Map[K, V]] =
    javaFactoryForAbstractMap[K, V].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForAbstractMap[K, V]: JavaFactory[(K, V), ju.AbstractMap[K, V]] =
    javaFactoryForHashMap[K, V].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForSortedMap[K: Ordering, V]: JavaFactory[(K, V), ju.SortedMap[K, V]] =
    javaFactoryForNavigableMap[K, V].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForNavigableMap[K: Ordering, V]: JavaFactory[(K, V), ju.NavigableMap[K, V]] =
    javaFactoryForTreeMap[K, V].narrow

  /** @since 0.8.0 */
  implicit def javaFactoryForHashMap[K, V]: JavaFactory[(K, V), ju.HashMap[K, V]] =
    new MapFactory[K, V, ju.HashMap](new ju.HashMap[K, V])

  /** @since 0.8.0 */
  implicit def javaFactoryForIdentityHashMap[K, V]: JavaFactory[(K, V), ju.IdentityHashMap[K, V]] =
    new MapFactory(new ju.IdentityHashMap[K, V])

  /** @since 0.8.0 */
  implicit def javaFactoryForLinkedHashMap[K, V]: JavaFactory[(K, V), ju.LinkedHashMap[K, V]] = new MapFactory(
    new ju.LinkedHashMap[K, V]
  )

  /** @since 0.8.0 */
  implicit def javaFactoryForWeakHashMap[K, V]: JavaFactory[(K, V), ju.WeakHashMap[K, V]] =
    new MapFactory(new ju.WeakHashMap[K, V])

  /** @since 0.8.0 */
  implicit def javaFactoryForTreeMap[K: Ordering, V]: JavaFactory[(K, V), ju.TreeMap[K, V]] = new MapFactory(
    new ju.TreeMap[K, V](Ordering[K])
  )

  /** @since 0.8.0 */
  implicit def javaFactoryForEnumMap[K <: java.lang.Enum[K]: ClassTag, V]: JavaFactory[(K, V), ju.EnumMap[K, V]] =
    new MapFactory[K, V, ju.Map](new ju.EnumMap[K, V](classTag[K].runtimeClass.asInstanceOf[Class[K]]))
      .asInstanceOf[JavaFactory[(K, V), ju.EnumMap[K, V]]]

  // java.util.stream.BaseStream

  /** @since 0.8.0 */
  implicit def javaFactoryForStream[A]: JavaFactory[A, ju.stream.Stream[A]] = new StreamBuilder[A]

  /** @since 0.8.0 */
  implicit def javaFactoryForIntStream: JavaFactory[Int, ju.stream.IntStream] = new IntStreamBuilder

  /** @since 0.8.0 */
  implicit def javaFactoryForLongStream: JavaFactory[Long, ju.stream.LongStream] = new LongStreamBuilder

  /** @since 0.8.0 */
  implicit def javaFactoryForDoubleStream: JavaFactory[Double, ju.stream.DoubleStream] = new DoubleStreamBuilder
}
