package io.scalaland.chimney.javacollections

import scala.collection.compat.*
import scala.reflect.{classTag, ClassTag}

/** Prevent mixing with other Factories
  *
  * @since 0.8.1
  */
trait JavaFactory[A, CC] {

  def fromIterator(it: java.util.Iterator[A]): CC
  final def fromSpecific(it: IterableOnce[A]): CC = {
    import scala.jdk.CollectionConverters.*
    fromIterator(it.iterator.asJava)
  }
  def newBuilder: JavaFactory.Builder[A, CC]
}

/** @since 0.8.1 */
object JavaFactory {

  /** @since 0.8.1 */
  trait Builder[A, CC] {
    def addOne(a: A): Unit
    def result(): CC
  }

  // Iterator

  /** @since 0.8.1 */
  implicit def javaFactoryForIterator[A]: JavaFactory[A, java.util.Iterator[A]] = new IteratorImpl[A]

  // Lists

  /** @since 0.8.1 */
  implicit def javaFactoryForArrayList[A]: JavaFactory[A, java.util.ArrayList[A]] =
    new SeqImpl[A, java.util.ArrayList](new java.util.ArrayList[A], (r, a) => r.add(a))

  /** @since 0.8.1 */
  implicit def javaFactoryForLinkedList[A]: JavaFactory[A, java.util.LinkedList[A]] =
    new SeqImpl[A, java.util.LinkedList](new java.util.LinkedList[A], (r, a) => r.add(a))

  /** @since 0.8.1 */
  implicit def javaFactoryForVector[A]: JavaFactory[A, java.util.Vector[A]] =
    new SeqImpl[A, java.util.Vector](new java.util.Vector[A], (r, a) => r.add(a))

  // Queues

  /** @since 0.8.1 */
  implicit def javaFactoryForArrayDeque[A]: JavaFactory[A, java.util.ArrayDeque[A]] =
    new SeqImpl[A, java.util.ArrayDeque](new java.util.ArrayDeque[A], (r, a) => r.add(a))

  /** @since 0.8.1 */
  implicit def javaFactoryForPriorityQueue[A]: JavaFactory[A, java.util.PriorityQueue[A]] =
    new SeqImpl[A, java.util.PriorityQueue](new java.util.PriorityQueue[A], (r, a) => r.add(a))

  /** @since 0.8.1 */
  implicit def javaFactoryForStack[A]: JavaFactory[A, java.util.Stack[A]] =
    new SeqImpl[A, java.util.Stack](new java.util.Stack[A], (r, a) => r.add(a))

  // Sets

  /** @since 0.8.1 */
  implicit def javaFactoryForEnumSet[A <: java.lang.Enum[A]: ClassTag]: JavaFactory[A, java.util.EnumSet[A]] =
    new SeqImpl[A, java.util.Set](
      java.util.EnumSet.noneOf[A](classTag[A].runtimeClass.asInstanceOf[Class[A]]),
      (r, a) => r.add(a)
    ).asInstanceOf[JavaFactory[A, java.util.EnumSet[A]]]

  /** @since 0.8.1 */
  implicit def javaFactoryForHashSet[A]: JavaFactory[A, java.util.HashSet[A]] =
    new SeqImpl[A, java.util.HashSet](new java.util.HashSet[A], (r, a) => r.add(a))

  /** @since 0.8.1 */
  implicit def javaFactoryForLinkedHashSet[A]: JavaFactory[A, java.util.LinkedHashSet[A]] =
    new SeqImpl[A, java.util.LinkedHashSet](new java.util.LinkedHashSet[A], (r, a) => r.add(a))

  /** @since 0.8.1 */
  implicit def javaFactoryForTreeSet[A]: JavaFactory[A, java.util.TreeSet[A]] =
    new SeqImpl[A, java.util.TreeSet](new java.util.TreeSet[A], (r, a) => r.add(a))

  // Maps

  /** @since 0.8.1 */
  implicit def javaFactoryForHashtable[K, V]: JavaFactory[(K, V), java.util.Hashtable[K, V]] =
    new MapImpl[K, V, java.util.Hashtable](new java.util.Hashtable[K, V], (m, k, v) => m.put(k, v))

  /** @since 0.8.1 */
  implicit def javaFactoryForEnumMap[K <: java.lang.Enum[K]: ClassTag, V]
      : JavaFactory[(K, V), java.util.EnumMap[K, V]] =
    new MapImpl[K, V, java.util.Map](
      new java.util.EnumMap[K, V](classTag[K].runtimeClass.asInstanceOf[Class[K]]),
      (m, k, v) => m.put(k, v)
    )
      .asInstanceOf[JavaFactory[(K, V), java.util.EnumMap[K, V]]]

  /** @since 0.8.1 */
  implicit def javaFactoryForHashMap[K, V]: JavaFactory[(K, V), java.util.HashMap[K, V]] =
    new MapImpl[K, V, java.util.HashMap](new java.util.HashMap[K, V], (m, k, v) => m.put(k, v))

  /** @since 0.8.1 */
  implicit def javaFactoryForIdentityHashMap[K, V]: JavaFactory[(K, V), java.util.IdentityHashMap[K, V]] =
    new MapImpl[K, V, java.util.IdentityHashMap](new java.util.IdentityHashMap[K, V], (m, k, v) => m.put(k, v))

  /** @since 0.8.1 */
  implicit def javaFactoryForLinkedHashMap[K, V]: JavaFactory[(K, V), java.util.LinkedHashMap[K, V]] =
    new MapImpl[K, V, java.util.LinkedHashMap](new java.util.LinkedHashMap[K, V], (m, k, v) => m.put(k, v))

  /** @since 0.8.1 */
  implicit def javaFactoryForWeakHashMap[K, V]: JavaFactory[(K, V), java.util.WeakHashMap[K, V]] =
    new MapImpl[K, V, java.util.WeakHashMap](new java.util.WeakHashMap[K, V], (m, k, v) => m.put(k, v))

  /** @since 0.8.1 */
  implicit def javaFactoryForTreeMap[K, V]: JavaFactory[(K, V), java.util.TreeMap[K, V]] =
    new MapImpl[K, V, java.util.TreeMap](new java.util.TreeMap[K, V], (m, k, v) => m.put(k, v))

  // implementations

  final private class IteratorImpl[A] extends JavaFactory[A, java.util.Iterator[A]] {

    def fromIterator(it: java.util.Iterator[A]): java.util.Iterator[A] = it

    def newBuilder: Builder[A, java.util.Iterator[A]] = new Builder[A, java.util.Iterator[A]] {
      private val collection = new java.util.ArrayList[A]()
      def addOne(a: A): Unit = { collection.add(a); () }
      def result(): java.util.Iterator[A] = collection.iterator()
    }
  }

  final private class SeqImpl[A, CC[A1] <: java.lang.Iterable[A1]](
      create: => CC[A],
      append: (CC[A], A) => Any
  ) extends JavaFactory[A, CC[A]] {
    def fromIterator(it: java.util.Iterator[A]): CC[A] = {
      val collection = create
      while (it.hasNext()) {
        append(collection, it.next())
        ()
      }
      collection
    }

    def newBuilder: Builder[A, CC[A]] = new Builder[A, CC[A]] {
      private val collection = create
      final def addOne(a: A): Unit = { append(collection, a); () }
      final def result(): CC[A] = collection
    }
  }

  final private class MapImpl[K, V, CC[K1, V1] <: java.util.Map[K1, V1]](
      create: => CC[K, V],
      append: (CC[K, V], K, V) => Any
  ) extends JavaFactory[(K, V), CC[K, V]] {
    def fromIterator(it: java.util.Iterator[(K, V)]): CC[K, V] = {
      val collection = create
      while (it.hasNext()) {
        val (k, v) = it.next()
        append(collection, k, v)
        ()
      }
      collection
    }

    def newBuilder: Builder[(K, V), CC[K, V]] = new Builder[(K, V), CC[K, V]] {
      private val collection = create
      final def addOne(pair: (K, V)): Unit = { append(collection, pair._1, pair._2); () }
      final def result(): CC[K, V] = collection
    }
  }
}
