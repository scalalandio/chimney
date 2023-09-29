package io.scalaland.chimney.javacollections

import java.util as ju

import scala.jdk.CollectionConverters.*

/** Interface dedicated to Java collections allowing iterating over types with unrelated interfaces.
  *
  * @since 0.8.1
  */
trait JavaIterator[CC] {
  type A

  def iterator(collection: CC): Iterator[A]

  final def foreach(collection: CC)(f: A => Any): Unit = iterator(collection).foreach(f)
}
object JavaIterator extends JavaIteratorLowPriorityImplicits {

  type Aux[A0, CC] = JavaIterator[CC] { type A = A0 }

  final class IteratorIterator[A0] extends JavaIterator[ju.Iterator[A0]] {
    type A = A0
    def iterator(collection: ju.Iterator[A0]): Iterator[A] = collection.asScala
  }

  final class EnumerationIterator[A0] extends JavaIterator[ju.Enumeration[A0]] {
    type A = A0
    def iterator(collection: ju.Enumeration[A0]): Iterator[A] = collection.asScala
  }

  final class IterableIterator[A0, CC[A1] <: java.lang.Iterable[A1]] extends JavaIterator[CC[A0]] {
    type A = A0
    def iterator(collection: CC[A0]): Iterator[A] = collection.asScala.iterator
  }

  final class DictionaryIterator[K, V, CC[K0, V0] <: ju.Dictionary[K0, V0]] extends JavaIterator[CC[K, V]] {
    type A = (K, V)
    def iterator(collection: CC[K, V]): Iterator[A] = collection.asScala.iterator
  }

  final class MapIterator[K, V, CC[K0, V0] <: ju.Map[K0, V0]] extends JavaIterator[CC[K, V]] {
    type A = (K, V)
    def iterator(collection: CC[K, V]): Iterator[A] = collection.asScala.iterator
  }

  final class BitSetIterator extends JavaIterator[ju.BitSet] {
    type A = Int
    def iterator(collection: ju.BitSet): Iterator[Int] = (0 to collection.size()).filter(collection.get).iterator
  }

  // java.util.Iterator

  /** @since 0.8.1 */
  implicit def javaIteratorForIterator[A]: Aux[A, ju.Iterator[A]] = new IteratorIterator[A]

  // java.util.Collection

  /** @since 0.8.1 */
  implicit def javaIteratorForIterable[A, CC[A0] <: java.lang.Iterable[A0]]: Aux[A, CC[A]] = new IterableIterator[A, CC]

  // java.util.Map

  /** @since 0.8.1 */
  implicit def javaIteratorForMap[K, V, CC[K0, V0] <: ju.Map[K0, V0]]: Aux[(K, V), CC[K, V]] = new MapIterator[K, V, CC]

  // java.util.BitSet

  /** @since 0.8.1 */
  implicit def javaIteratorForBitSet: Aux[Int, ju.BitSet] = new BitSetIterator

  // java.util.Properties

  /** @since 0.8.1 */
  implicit val javaIteratorForProperties: Aux[(String, String), ju.Properties] =
    javaIteratorForDictionary[String, String, ju.Hashtable[String, String]]
      .asInstanceOf[Aux[(String, String), ju.Properties]]
}

private[javacollections] trait JavaIteratorLowPriorityImplicits {

  import JavaIterator.*

  // java.util.Enumeration

  /** @since 0.8.1 */
  implicit def javaIteratorForEnumeration[A]: Aux[A, ju.Enumeration[A]] = new EnumerationIterator[A]

  // java.util.Dictionary

  /** @since 0.8.1 */
  implicit def javaIteratorForDictionary[K, V, CC[K0, V0] <: ju.Dictionary[K0, V0]]: Aux[(K, V), CC[K, V]] =
    new DictionaryIterator[K, V, CC]
}
