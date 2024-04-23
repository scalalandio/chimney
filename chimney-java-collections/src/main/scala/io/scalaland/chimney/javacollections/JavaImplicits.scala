package io.scalaland.chimney.javacollections

import java.util as ju

import io.scalaland.chimney.integrations.*

import scala.collection.compat.Factory
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

// TODO: rename once all integrations are migrated
trait JavaImplicits {

  // java.util.Optional

  /** @since 1.0.0 */
  implicit def javaOptionalIsOptionalValue[Value]: OptionalValue[ju.Optional[Value], Value] =
    new OptionalValue[ju.Optional[Value], Value] {
      def empty: ju.Optional[Value] = ju.Optional.empty()
      def of(value: Value): ju.Optional[Value] = ju.Optional.ofNullable(value)
      def fold[A](oa: ju.Optional[Value], onNone: => A, onSome: Value => A): A =
        if (oa.isEmpty) onNone else onSome(oa.orElseThrow())
    }

  // java.util.Iterator

  /** @since 1.0.0 */
  implicit def javaIterableIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.Iterator[Item], Item] =
    new TotallyBuildIterable[ju.Iterator[Item], Item] {
      def totalFactory: Factory[Item, ju.Iterator[Item]] = new FactoryCompat[Item, ju.Iterator[Item]] {
        def newBuilder: mutable.Builder[Item, ju.Iterator[Item]] = new FactoryCompat.Builder[Item, ju.Iterator[Item]] {
          private val impl = new ju.ArrayList[Item]()
          def clear(): Unit = impl.clear()
          def result(): ju.Iterator[Item] = impl.iterator()
          def addOne(elem: Item): this.type = { impl.add(elem); this }
        }
      }
      def iterator(collection: ju.Iterator[Item]): Iterator[Item] = collection.asScala
    }

  // java.util.Enumeration

  /** @since 1.0.0 */
  implicit def javaEnumerationIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.Enumeration[Item], Item] =
    new TotallyBuildIterable[ju.Enumeration[Item], Item] {
      def totalFactory: Factory[Item, ju.Enumeration[Item]] = new FactoryCompat[Item, ju.Enumeration[Item]] {
        def newBuilder: mutable.Builder[Item, ju.Enumeration[Item]] =
          new FactoryCompat.Builder[Item, ju.Enumeration[Item]] {
            private val impl = new ju.ArrayList[Item]()
            def clear(): Unit = impl.clear()
            def result(): ju.Enumeration[Item] = new ju.Enumeration[Item] {
              private val it = impl.iterator()
              def hasMoreElements: Boolean = it.hasNext()
              def nextElement(): Item = it.next()
            }
            override def addOne(elem: Item): this.type = { impl.add(elem); this }
          }
      }
      def iterator(collection: ju.Enumeration[Item]): Iterator[Item] = collection.asScala
    }

  // java.util.Collection

  /** @since 1.0.0 */
  implicit def javaCollectionIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.Collection[Item], Item] =
    javaAbstractCollectionIsTotallyBuildIterable[Item].narrow

  /** @since 1.0.0 */
  implicit def javaAbstractCollectionIsTotallyBuildIterable[Item]
      : TotallyBuildIterable[ju.AbstractCollection[Item], Item] =
    javaArrayListIsTotallyBuildIterable[Item].narrow

  // java.util.List

  /** @since 1.0.0 */
  implicit def javaListIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.List[Item], Item] =
    javaAbstractListIsTotallyBuildIterable[Item].narrow

  /** @since 1.0.0 */
  implicit def javaAbstractListIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.AbstractList[Item], Item] =
    javaArrayListIsTotallyBuildIterable[Item].narrow

  /** @since 1.0.0 */
  implicit def javaAbstractSequentialListIsTotallyBuildIterable[Item]
      : TotallyBuildIterable[ju.AbstractSequentialList[Item], Item] =
    javaLinkedListIsTotallyBuildIterable[Item].narrow

  /** @since 1.0.0 */
  implicit def javaArrayListIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.ArrayList[Item], Item] =
    collectionTotallyBuildIterable(new ju.ArrayList[Item])

  /** @since 1.0.0 */
  implicit def javaLinkedListIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.LinkedList[Item], Item] =
    collectionTotallyBuildIterable(new ju.LinkedList[Item])

  /** @since 1.0.0 */
  implicit def javaVectorIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.Vector[Item], Item] =
    collectionTotallyBuildIterable(new ju.Vector[Item])

  /** @since 1.0.0 */
  implicit def javaStackIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.Stack[Item], Item] =
    collectionTotallyBuildIterable(new ju.Stack[Item])

  // java.util.Deque

  /** @since 1.0.0 */
  implicit def javaDequeIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.Deque[Item], Item] =
    javaArrayDequeIsTotallyBuildIterable[Item].narrow

  /** @since 1.0.0 */
  implicit def javaArrayDequeIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.ArrayDeque[Item], Item] =
    collectionTotallyBuildIterable(new ju.ArrayDeque[Item])

  // java.util.Queue

  /** @since 1.0.0 */
  implicit def javaQueueIsTotallyBuildIterable[Item: Ordering]: TotallyBuildIterable[ju.Queue[Item], Item] =
    javaAbstractQueueIsTotallyBuildIterable[Item].narrow

  /** @since 1.0.0 */
  implicit def javaAbstractQueueIsTotallyBuildIterable[Item: Ordering]
      : TotallyBuildIterable[ju.AbstractQueue[Item], Item] =
    javaPriorityQueueIsTotallyBuildIterable[Item].narrow

  /** @since 1.0.0 */
  implicit def javaPriorityQueueIsTotallyBuildIterable[Item: Ordering]
      : TotallyBuildIterable[ju.PriorityQueue[Item], Item] =
    collectionTotallyBuildIterable(new ju.PriorityQueue[Item](Ordering[Item]))

  // java.util.Set

  /** @since 1.0.0 */
  implicit def javaSetIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.Set[Item], Item] =
    javaAbstractSetIsTotallyBuildIterable[Item].narrow

  /** @since 1.0.0 */
  implicit def javaAbstractSetIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.AbstractSet[Item], Item] =
    javaHashSetIsTotallyBuildIterable[Item].narrow

  /** @since 1.0.0 */
  implicit def javaSortedSetIsTotallyBuildIterable[Item: Ordering]: TotallyBuildIterable[ju.SortedSet[Item], Item] =
    javaNavigableSetIsTotallyBuildIterable[Item].narrow

  /** @since 1.0.0 */
  implicit def javaNavigableSetIsTotallyBuildIterable[Item: Ordering]
      : TotallyBuildIterable[ju.NavigableSet[Item], Item] =
    javaTreeSetIsTotallyBuildIterable[Item].narrow

  /** @since 1.0.0 */
  implicit def javaHashSetIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.HashSet[Item], Item] =
    collectionTotallyBuildIterable(new ju.HashSet[Item])

  /** @since 1.0.0 */
  implicit def javaLinkedHashSetIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.LinkedHashSet[Item], Item] =
    collectionTotallyBuildIterable(new ju.LinkedHashSet[Item])

  /** @since 1.0.0 */
  implicit def javaTreeSetIsTotallyBuildIterable[Item: Ordering]: TotallyBuildIterable[ju.TreeSet[Item], Item] =
    collectionTotallyBuildIterable(new ju.TreeSet[Item](Ordering[Item]))

  // java.util.BitSet

  // java.util.Dictionary

  // java.util.Map

  // java.util.stream.BaseStream

  private def collectionTotallyBuildIterable[Item, CC <: ju.Collection[Item]](
      empty: => CC
  ): TotallyBuildIterable[CC, Item] =
    new TotallyBuildIterable[CC, Item] {
      def totalFactory: Factory[Item, CC] = new FactoryCompat[Item, CC] {
        def newBuilder: mutable.Builder[Item, CC] =
          new FactoryCompat.Builder[Item, CC] {
            private val impl = empty
            def clear(): Unit = impl.clear()
            def result(): CC = impl
            def addOne(elem: Item): this.type = { impl.add(elem); this }
          }
      }
      def iterator(collection: CC): Iterator[Item] = collection.iterator().asScala
    }
}
