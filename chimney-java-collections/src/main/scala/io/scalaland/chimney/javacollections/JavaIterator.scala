package io.scalaland.chimney.javacollections

import java.util as ju

import scala.jdk.CollectionConverters.*

/** Interface dedicated to Java collections allowing iterating over types with unrelated interfaces.
  *
  * @since 0.8.1
  */
trait JavaIterator[A, CC] {
  def iterator(collection: CC): Iterator[A]

  final def foreach(collection: CC)(f: A => Any): Unit = iterator(collection).foreach(f)
}
object JavaIterator extends JavaIteratorLowPriorityImplicits {

  final class IteratorIterator[A] extends JavaIterator[A, ju.Iterator[A]] {
    def iterator(collection: ju.Iterator[A]): Iterator[A] = collection.asScala
  }

  final class EnumerationIterator[A] extends JavaIterator[A, ju.Enumeration[A]] {
    def iterator(collection: ju.Enumeration[A]): Iterator[A] = collection.asScala
  }

  final class IterableIterator[A, CC[A0] <: java.lang.Iterable[A0]] extends JavaIterator[A, CC[A]] {
    def iterator(collection: CC[A]): Iterator[A] = collection.asScala.iterator
  }

  final class DictionaryIterator[K, V, CC[K0, V0] <: ju.Dictionary[K0, V0]] extends JavaIterator[(K, V), CC[K, V]] {
    def iterator(collection: CC[K, V]): Iterator[(K, V)] = collection.asScala.iterator
  }

  final class MapIterator[K, V, CC[K0, V0] <: ju.Map[K0, V0]] extends JavaIterator[(K, V), CC[K, V]] {
    def iterator(collection: CC[K, V]): Iterator[(K, V)] = collection.asScala.iterator
  }

  final class BitSetIterator extends JavaIterator[Int, ju.BitSet] {
    def iterator(collection: ju.BitSet): Iterator[Int] = (0 to collection.size()).filter(collection.get).iterator
  }

  // java.util.Iterator

  /** @since 0.8.1 */
  implicit def javaIteratorForIterator[A]: JavaIterator[A, ju.Iterator[A]] = new IteratorIterator[A]

  // java.util.Collection

  /** @since 0.8.1 */
  implicit def javaIteratorForIterable[A, CC[A0] <: java.lang.Iterable[A0]]: JavaIterator[A, CC[A]] =
    new IterableIterator[A, CC]

  // java.util.Map

  /** @since 0.8.1 */
  implicit def javaIteratorForMap[K, V, CC[K0, V0] <: ju.Map[K0, V0]]: JavaIterator[(K, V), CC[K, V]] =
    new MapIterator[K, V, CC]

  // java.util.BitSet

  /** @since 0.8.1 */
  implicit def javaIteratorForBitSet: JavaIterator[Int, ju.BitSet] = new BitSetIterator

  // java.util.Properties

  /** @since 0.8.1 */
  implicit val javaIteratorForProperties: JavaIterator[(String, String), ju.Properties] =
    javaIteratorForDictionary[String, String, ju.Hashtable].asInstanceOf[JavaIterator[(String, String), ju.Properties]]
}

private[javacollections] trait JavaIteratorLowPriorityImplicits {

  import JavaIterator.*

  // java.util.Enumeration

  /** @since 0.8.1 */
  implicit def javaIteratorForEnumeration[A]: JavaIterator[A, ju.Enumeration[A]] = new EnumerationIterator[A]

  // java.util.Dictionary

  /** @since 0.8.1 */
  implicit def javaIteratorForDictionary[K, V, CC[K0, V0] <: ju.Dictionary[K0, V0]]: JavaIterator[(K, V), CC[K, V]] =
    new DictionaryIterator[K, V, CC]
}
