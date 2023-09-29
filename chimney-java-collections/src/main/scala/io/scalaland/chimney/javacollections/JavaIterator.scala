package io.scalaland.chimney.javacollections

import java.util as ju

import scala.jdk.CollectionConverters.*

/** Interface dedicated to Java collections allowing iterating over types with unrelated interfaces.
  *
  * @tparam A  collection's element's type
  * @tparam CC collection's type with applied type parameters
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

  final class StreamIterator[A, CC <: ju.stream.BaseStream[A, CC]] extends JavaIterator[A, CC] {
    def iterator(collection: CC): Iterator[A] = collection.iterator().asScala
  }

  // java.util.Iterator

  /** @since 0.8.1 */
  implicit def javaIteratorForIterator[A]: JavaIterator[A, ju.Iterator[A]] = new IteratorIterator[A]

  // java.util.Collection

  /** @since 0.8.1 */
  implicit def javaIteratorForIterable[A, CC[A0] <: java.lang.Iterable[A0]]: JavaIterator[A, CC[A]] =
    new IterableIterator[A, CC]

  /** @since 0.8.1 */
  implicit def javaIteratorForEnumSet[E <: java.lang.Enum[E]]: JavaIterator[E, ju.EnumSet[E]] =
    new IterableIterator[E, ju.Set].asInstanceOf[JavaIterator[E, ju.EnumSet[E]]]

  // java.util.Map

  /** @since 0.8.1 */
  implicit def javaIteratorForMap[K, V, CC[K0, V0] <: ju.Map[K0, V0]]: JavaIterator[(K, V), CC[K, V]] =
    new MapIterator[K, V, CC]

  /** @since 0.8.1 */
  implicit def javaIteratorForEnumMap[K <: java.lang.Enum[K], V]: JavaIterator[(K, V), ju.EnumMap[K, V]] =
    new MapIterator[K, V, ju.Map].asInstanceOf[JavaIterator[(K, V), ju.EnumMap[K, V]]]

  // java.util.BitSet

  /** @since 0.8.1 */
  implicit def javaIteratorForBitSet: JavaIterator[Int, ju.BitSet] = new BitSetIterator

  // java.util.Properties

  /** @since 0.8.1 */
  implicit val javaIteratorForProperties: JavaIterator[(String, String), ju.Properties] =
    javaIteratorForDictionary[String, String, ju.Hashtable].asInstanceOf[JavaIterator[(String, String), ju.Properties]]

  // java.util.stream.BaseStream

  /** @since 0.8.1 */
  implicit def javaIteratorForStream[A]: JavaIterator[A, ju.stream.Stream[A]] =
    new StreamIterator[A, ju.stream.Stream[A]]

  /** @since 0.8.1 */
  implicit def javaIteratorForIntStream: JavaIterator[Int, ju.stream.IntStream] =
    new StreamIterator[java.lang.Integer, ju.stream.IntStream].asInstanceOf[JavaIterator[Int, ju.stream.IntStream]]

  /** @since 0.8.1 */
  implicit def javaIteratorForLongStream: JavaIterator[Long, ju.stream.LongStream] =
    new StreamIterator[java.lang.Long, ju.stream.LongStream].asInstanceOf[JavaIterator[Long, ju.stream.LongStream]]

  /** @since 0.8.1 */
  implicit def javaIteratorForDoubleStream: JavaIterator[Double, ju.stream.DoubleStream] =
    new StreamIterator[java.lang.Double, ju.stream.DoubleStream]
      .asInstanceOf[JavaIterator[Double, ju.stream.DoubleStream]]
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
