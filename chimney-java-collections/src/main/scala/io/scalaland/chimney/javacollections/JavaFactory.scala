package io.scalaland.chimney.javacollections

import java.util as ju

import scala.collection.compat.*
import scala.reflect.{classTag, ClassTag}

/** Prevent mixing with other Factories
  *
  * @since 0.8.1
  */
trait JavaFactory[A, +CC] {

  def fromIterator(it: ju.Iterator[A]): CC
  final def fromSpecific(it: IterableOnce[A]): CC = {
    import scala.jdk.CollectionConverters.*
    fromIterator(it.iterator.asJava)
  }
  def newBuilder: JavaFactory.Builder[A, CC]
}

/** @since 0.8.1 */
object JavaFactory extends JavaFactoryLowPriorityImplicits1 {

  /** @since 0.8.1 */
  trait Builder[A, +CC] {
    def addOne(a: A): Unit
    def result(): CC
  }

  final protected class IteratorImpl[A] extends JavaFactory[A, ju.Iterator[A]] {

    def fromIterator(it: ju.Iterator[A]): ju.Iterator[A] = it

    def newBuilder: Builder[A, ju.Iterator[A]] = new Builder[A, ju.Iterator[A]] {
      private val collection = new ju.ArrayList[A]()
      def addOne(a: A): Unit = { collection.add(a); () }
      def result(): ju.Iterator[A] = collection.iterator()
    }
  }

  final protected class SeqImpl[A, CC[A1] <: ju.Collection[A1]](
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
      final def addOne(a: A): Unit = { collection.add(a); () }
      final def result(): CC[A] = collection
    }
  }

  final protected class MapImpl[K, V, CC[K1, V1] <: ju.Map[K1, V1]](
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
      final def addOne(pair: (K, V)): Unit = { collection.put(pair._1, pair._2); () }
      final def result(): CC[K, V] = collection
    }
  }
}

private[javacollections] trait JavaFactoryLowPriorityImplicits1 extends JavaFactoryLowPriorityImplicits2 {
  this: JavaFactory.type =>

  // Iterator

  /** @since 0.8.1 */
  implicit def javaFactoryForIterator[A]: JavaFactory[A, ju.Iterator[A]] = new IteratorImpl[A]

  // Lists

  /** @since 0.8.1 */
  implicit def javaFactoryForVector[A]: JavaFactory[A, ju.Vector[A]] = new SeqImpl(new ju.Vector[A])

  // Queues

  /** @since 0.8.1 */
  implicit def javaFactoryForArrayDeque[A]: JavaFactory[A, ju.ArrayDeque[A]] = new SeqImpl(new ju.ArrayDeque[A])

  // Sets

  /** @since 0.8.1 */
  implicit def javaFactoryForEnumSet[A <: java.lang.Enum[A]: ClassTag]: JavaFactory[A, ju.EnumSet[A]] =
    new SeqImpl[A, ju.Set](ju.EnumSet.noneOf[A](classTag[A].runtimeClass.asInstanceOf[Class[A]]))
      .asInstanceOf[JavaFactory[A, ju.EnumSet[A]]]

  // Maps

  /** @since 0.8.1 */
  implicit def javaFactoryForEnumMap[K <: java.lang.Enum[K]: ClassTag, V]: JavaFactory[(K, V), ju.EnumMap[K, V]] =
    new MapImpl[K, V, ju.Map](new ju.EnumMap[K, V](classTag[K].runtimeClass.asInstanceOf[Class[K]]))
      .asInstanceOf[JavaFactory[(K, V), ju.EnumMap[K, V]]]
}

private[javacollections] trait JavaFactoryLowPriorityImplicits2 { this: JavaFactory.type =>

  // Lists

  /** @since 0.8.1 */
  implicit def javaFactoryForArrayList[A]: JavaFactory[A, ju.ArrayList[A]] = new SeqImpl(new ju.ArrayList[A])

  /** @since 0.8.1 */
  implicit def javaFactoryForLinkedList[A]: JavaFactory[A, ju.LinkedList[A]] = new SeqImpl(new ju.LinkedList[A])

  // Queues

  /** @since 0.8.1 */
  implicit def javaFactoryForPriorityQueue[A]: JavaFactory[A, ju.PriorityQueue[A]] = new SeqImpl(
    new ju.PriorityQueue[A]
  )

  /** @since 0.8.1 */
  implicit def javaFactoryForStack[A]: JavaFactory[A, ju.Stack[A]] = new SeqImpl(new ju.Stack[A])

  // Sets

  /** @since 0.8.1 */
  implicit def javaFactoryForHashSet[A]: JavaFactory[A, ju.HashSet[A]] = new SeqImpl(new ju.HashSet[A])

  /** @since 0.8.1 */
  implicit def javaFactoryForLinkedHashSet[A]: JavaFactory[A, ju.LinkedHashSet[A]] = new SeqImpl(
    new ju.LinkedHashSet[A]
  )

  /** @since 0.8.1 */
  implicit def javaFactoryForTreeSet[A]: JavaFactory[A, ju.TreeSet[A]] = new SeqImpl(new ju.TreeSet[A])

  // Maps

  /** @since 0.8.1 */
  implicit def javaFactoryForHashtable[K, V]: JavaFactory[(K, V), ju.Hashtable[K, V]] = new MapImpl(
    new ju.Hashtable[K, V]
  )

  /** @since 0.8.1 */
  implicit def javaFactoryForHashMap[K, V]: JavaFactory[(K, V), ju.HashMap[K, V]] =
    new MapImpl[K, V, ju.HashMap](new ju.HashMap[K, V])

  /** @since 0.8.1 */
  implicit def javaFactoryForIdentityHashMap[K, V]: JavaFactory[(K, V), ju.IdentityHashMap[K, V]] =
    new MapImpl(new ju.IdentityHashMap[K, V])

  /** @since 0.8.1 */
  implicit def javaFactoryForLinkedHashMap[K, V]: JavaFactory[(K, V), ju.LinkedHashMap[K, V]] = new MapImpl(
    new ju.LinkedHashMap[K, V]
  )

  /** @since 0.8.1 */
  implicit def javaFactoryForWeakHashMap[K, V]: JavaFactory[(K, V), ju.WeakHashMap[K, V]] =
    new MapImpl(new ju.WeakHashMap[K, V])

  /** @since 0.8.1 */
  implicit def javaFactoryForTreeMap[K, V]: JavaFactory[(K, V), ju.TreeMap[K, V]] = new MapImpl(new ju.TreeMap[K, V])
}
