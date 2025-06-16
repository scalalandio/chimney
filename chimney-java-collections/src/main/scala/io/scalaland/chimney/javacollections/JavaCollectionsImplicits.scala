package io.scalaland.chimney.javacollections

import java.util as ju
import io.scalaland.chimney.integrations.*

import scala.collection.Factory
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.reflect.{classTag, ClassTag}

/** @since 1.0.0 */
trait JavaCollectionsImplicits {

  // java.util.Optional

  /** @since 1.0.0 */
  implicit def javaOptionalIsOptionalValue[Value]: OptionalValue[ju.Optional[Value], Value] =
    new OptionalValue[ju.Optional[Value], Value] {
      def empty: ju.Optional[Value] = ju.Optional.empty()
      def of(value: Value): ju.Optional[Value] = ju.Optional.ofNullable(value)
      def fold[A](oa: ju.Optional[Value], onNone: => A, onSome: Value => A): A =
        if (oa.isPresent) onSome(oa.get()) else onNone
    }

  // java.util.Iterator

  /** @since 1.0.0 */
  implicit def javaIteratorIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.Iterator[Item], Item] =
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
    javaAbstractCollectionIsTotallyBuildIterable[Item].widen

  /** @since 1.0.0 */
  implicit def javaAbstractCollectionIsTotallyBuildIterable[Item]
      : TotallyBuildIterable[ju.AbstractCollection[Item], Item] =
    javaArrayListIsTotallyBuildIterable[Item].widen

  // java.util.List

  /** @since 1.0.0 */
  implicit def javaListIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.List[Item], Item] =
    javaAbstractListIsTotallyBuildIterable[Item].widen

  /** @since 1.0.0 */
  implicit def javaAbstractListIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.AbstractList[Item], Item] =
    javaArrayListIsTotallyBuildIterable[Item].widen

  /** @since 1.0.0 */
  implicit def javaAbstractSequentialListIsTotallyBuildIterable[Item]
      : TotallyBuildIterable[ju.AbstractSequentialList[Item], Item] =
    javaLinkedListIsTotallyBuildIterable[Item].widen

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
    javaArrayDequeIsTotallyBuildIterable[Item].widen

  /** @since 1.0.0 */
  implicit def javaArrayDequeIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.ArrayDeque[Item], Item] =
    collectionTotallyBuildIterable(new ju.ArrayDeque[Item])

  // java.util.Queue

  /** @since 1.0.0 */
  implicit def javaQueueIsTotallyBuildIterable[Item: Ordering]: TotallyBuildIterable[ju.Queue[Item], Item] =
    javaAbstractQueueIsTotallyBuildIterable[Item].widen

  /** @since 1.0.0 */
  implicit def javaAbstractQueueIsTotallyBuildIterable[Item: Ordering]
      : TotallyBuildIterable[ju.AbstractQueue[Item], Item] =
    javaPriorityQueueIsTotallyBuildIterable[Item].widen

  /** @since 1.0.0 */
  implicit def javaPriorityQueueIsTotallyBuildIterable[Item: Ordering]
      : TotallyBuildIterable[ju.PriorityQueue[Item], Item] =
    collectionTotallyBuildIterable(new ju.PriorityQueue[Item](Ordering[Item]))

  // java.util.Set

  /** @since 1.0.0 */
  implicit def javaSetIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.Set[Item], Item] =
    javaAbstractSetIsTotallyBuildIterable[Item].widen

  /** @since 1.0.0 */
  implicit def javaAbstractSetIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.AbstractSet[Item], Item] =
    javaHashSetIsTotallyBuildIterable[Item].widen

  /** @since 1.0.0 */
  implicit def javaSortedSetIsTotallyBuildIterable[Item: Ordering]: TotallyBuildIterable[ju.SortedSet[Item], Item] =
    javaNavigableSetIsTotallyBuildIterable[Item].widen

  /** @since 1.0.0 */
  implicit def javaNavigableSetIsTotallyBuildIterable[Item: Ordering]
      : TotallyBuildIterable[ju.NavigableSet[Item], Item] =
    javaTreeSetIsTotallyBuildIterable[Item].widen

  /** @since 1.0.0 */
  implicit def javaHashSetIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.HashSet[Item], Item] =
    collectionTotallyBuildIterable(new ju.HashSet[Item])

  /** @since 1.0.0 */
  implicit def javaLinkedHashSetIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.LinkedHashSet[Item], Item] =
    collectionTotallyBuildIterable(new ju.LinkedHashSet[Item])

  /** @since 1.0.0 */
  implicit def javaTreeSetIsTotallyBuildIterable[Item: Ordering]: TotallyBuildIterable[ju.TreeSet[Item], Item] =
    collectionTotallyBuildIterable(new ju.TreeSet[Item](Ordering[Item]))

  /** @since 1.0.0 */
  implicit def javaEnumSetIsTotallyBuildIterable[Item <: java.lang.Enum[Item]: ClassTag]
      : TotallyBuildIterable[ju.EnumSet[Item], Item] =
    collectionTotallyBuildIterable(ju.EnumSet.noneOf[Item](classTag[Item].runtimeClass.asInstanceOf[Class[Item]]))

  // java.util.BitSet

  /** @since 1.0.0 */
  implicit val javaBitSetIsTotallyBuildIterable: TotallyBuildIterable[ju.BitSet, Int] =
    new TotallyBuildIterable[ju.BitSet, Int] {
      def totalFactory: Factory[Int, ju.BitSet] = new FactoryCompat[Int, ju.BitSet] {
        def newBuilder: mutable.Builder[Int, ju.BitSet] =
          new FactoryCompat.Builder[Int, ju.BitSet] {
            private val impl = new ju.BitSet()
            def clear(): Unit = impl.clear()
            def result(): ju.BitSet = impl
            def addOne(elem: Int): this.type = { impl.set(elem); this }
          }
      }
      def iterator(collection: ju.BitSet): Iterator[Int] = (0 to collection.size()).filter(collection.get).iterator
    }

  // java.util.Dictionary

  /** @since 1.0.0 */
  implicit def javaDictionaryIsTotallyBuildMap[Key, Value]: TotallyBuildMap[ju.Dictionary[Key, Value], Key, Value] =
    javaHashtableIsTotallyBuildMap[Key, Value].widen

  /** @since 1.0.0 */
  implicit def javaHashtableIsTotallyBuildMap[Key, Value]: TotallyBuildMap[ju.Hashtable[Key, Value], Key, Value] =
    mapTotallyBuildMap(new ju.Hashtable[Key, Value])

  /** @since 1.0.0 */
  implicit def javaPropertiesIsTotallyBuildMap: TotallyBuildMap[ju.Properties, String, String] =
    mapTotallyBuildMap[java.lang.Object, java.lang.Object, ju.Map[java.lang.Object, java.lang.Object]](
      new ju.Properties()
    ).asInstanceOf[TotallyBuildMap[ju.Properties, String, String]]

  // java.util.Map

  /** @since 1.0.0 */
  implicit def javaMapIsTotallyBuildMap[Key, Value]: TotallyBuildMap[ju.Map[Key, Value], Key, Value] =
    javaAbstractMapIsTotallyBuildMap[Key, Value].widen

  /** @since 1.0.0 */
  implicit def javaAbstractMapIsTotallyBuildMap[Key, Value]: TotallyBuildMap[ju.AbstractMap[Key, Value], Key, Value] =
    javaHashMapIsTotallyBuildMap[Key, Value].widen

  /** @since 1.0.0 */
  implicit def javaSortedMapIsTotallyBuildMap[Key: Ordering, Value]
      : TotallyBuildMap[ju.SortedMap[Key, Value], Key, Value] =
    javaNavigableMapIsTotallyBuildMap[Key, Value].widen

  /** @since 1.0.0 */
  implicit def javaNavigableMapIsTotallyBuildMap[Key: Ordering, Value]
      : TotallyBuildMap[ju.NavigableMap[Key, Value], Key, Value] =
    javaTreeMapIsTotallyBuildMap[Key, Value].widen

  /** @since 1.0.0 */
  implicit def javaHashMapIsTotallyBuildMap[Key, Value]: TotallyBuildMap[ju.HashMap[Key, Value], Key, Value] =
    mapTotallyBuildMap(new ju.HashMap[Key, Value])

  /** @since 1.0.0 */
  implicit def javaIdentityHashMapIsTotallyBuildMap[Key, Value]
      : TotallyBuildMap[ju.IdentityHashMap[Key, Value], Key, Value] =
    mapTotallyBuildMap(new ju.IdentityHashMap[Key, Value])

  /** @since 1.0.0 */
  implicit def javaLinkedHashMapIsTotallyBuildMap[Key, Value]
      : TotallyBuildMap[ju.LinkedHashMap[Key, Value], Key, Value] =
    mapTotallyBuildMap(new ju.LinkedHashMap[Key, Value])

  /** @since 1.0.0 */
  implicit def javaWeakHashMapIsTotallyBuildMap[Key, Value]: TotallyBuildMap[ju.WeakHashMap[Key, Value], Key, Value] =
    mapTotallyBuildMap(new ju.WeakHashMap[Key, Value])

  /** @since 1.0.0 */
  implicit def javaTreeMapIsTotallyBuildMap[Key: Ordering, Value]: TotallyBuildMap[ju.TreeMap[Key, Value], Key, Value] =
    mapTotallyBuildMap(new ju.TreeMap[Key, Value](Ordering[Key]))

  /** @since 1.0.0 */
  implicit def javaEnumMapIsTotallyBuildMap[Key <: java.lang.Enum[Key]: ClassTag, Value]
      : TotallyBuildMap[ju.EnumMap[Key, Value], Key, Value] =
    mapTotallyBuildMap(new ju.EnumMap[Key, Value](classTag[Key].runtimeClass.asInstanceOf[Class[Key]]))

  // java.util.stream.BaseStream

  /** @since 1.0.0 */
  implicit def javaStreamIsTotallyBuildIterable[Item]: TotallyBuildIterable[ju.stream.Stream[Item], Item] =
    new TotallyBuildIterable[ju.stream.Stream[Item], Item] {
      def totalFactory: Factory[Item, ju.stream.Stream[Item]] = new FactoryCompat[Item, ju.stream.Stream[Item]] {
        def newBuilder: mutable.Builder[Item, ju.stream.Stream[Item]] =
          new FactoryCompat.Builder[Item, ju.stream.Stream[Item]] {
            private var impl = ju.stream.Stream.builder[Item]()
            def clear(): Unit = impl = ju.stream.Stream.builder[Item]()
            def result(): ju.stream.Stream[Item] = impl.build()
            def addOne(elem: Item): this.type = { impl.add(elem); this }
          }
      }
      def iterator(collection: ju.stream.Stream[Item]): Iterator[Item] = collection.iterator().asScala
    }

  /** @since 1.0.0 */
  implicit val javaIntStreamIsTotallyBuildIterable: TotallyBuildIterable[ju.stream.IntStream, Int] =
    new TotallyBuildIterable[ju.stream.IntStream, Int] {
      def totalFactory: Factory[Int, ju.stream.IntStream] = new FactoryCompat[Int, ju.stream.IntStream] {
        def newBuilder: mutable.Builder[Int, ju.stream.IntStream] =
          new FactoryCompat.Builder[Int, ju.stream.IntStream] {
            private var impl = ju.stream.IntStream.builder()
            def clear(): Unit = impl = ju.stream.IntStream.builder()
            def result(): ju.stream.IntStream = impl.build()
            def addOne(elem: Int): this.type = { impl.add(elem); this }
          }
      }
      def iterator(collection: ju.stream.IntStream): Iterator[Int] =
        collection.iterator().asScala.asInstanceOf[Iterator[Int]]
    }

  /** @since 1.0.0 */
  implicit val javaLongStreamIsTotallyBuildIterable: TotallyBuildIterable[ju.stream.LongStream, Long] =
    new TotallyBuildIterable[ju.stream.LongStream, Long] {
      def totalFactory: Factory[Long, ju.stream.LongStream] = new FactoryCompat[Long, ju.stream.LongStream] {
        def newBuilder: mutable.Builder[Long, ju.stream.LongStream] =
          new FactoryCompat.Builder[Long, ju.stream.LongStream] {
            private var impl = ju.stream.LongStream.builder()
            def clear(): Unit = impl = ju.stream.LongStream.builder()
            def result(): ju.stream.LongStream = impl.build()
            def addOne(elem: Long): this.type = { impl.add(elem); this }
          }
      }
      def iterator(collection: ju.stream.LongStream): Iterator[Long] =
        collection.iterator().asScala.asInstanceOf[Iterator[Long]]
    }

  /** @since 1.0.0 */
  implicit val javaDoubleStreamIsTotallyBuildIterable: TotallyBuildIterable[ju.stream.DoubleStream, Double] =
    new TotallyBuildIterable[ju.stream.DoubleStream, Double] {
      def totalFactory: Factory[Double, ju.stream.DoubleStream] = new FactoryCompat[Double, ju.stream.DoubleStream] {
        def newBuilder: mutable.Builder[Double, ju.stream.DoubleStream] =
          new FactoryCompat.Builder[Double, ju.stream.DoubleStream] {
            private var impl = ju.stream.DoubleStream.builder()
            def clear(): Unit = impl = ju.stream.DoubleStream.builder()
            def result(): ju.stream.DoubleStream = impl.build()
            def addOne(elem: Double): this.type = { impl.add(elem); this }
          }
      }
      def iterator(collection: ju.stream.DoubleStream): Iterator[Double] =
        collection.iterator().asScala.asInstanceOf[Iterator[Double]]
    }

  // reused

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

  private def mapTotallyBuildMap[Key, Value, CC <: ju.Map[Key, Value]](empty: => CC): TotallyBuildMap[CC, Key, Value] =
    new TotallyBuildMap[CC, Key, Value] {
      def totalFactory: Factory[(Key, Value), CC] = new FactoryCompat[(Key, Value), CC] {
        def newBuilder: mutable.Builder[(Key, Value), CC] =
          new FactoryCompat.Builder[(Key, Value), CC] {
            private val impl = empty
            def clear(): Unit = impl.clear()
            def result(): CC = impl
            def addOne(elem: (Key, Value)): this.type = { impl.put(elem._1, elem._2); this }
          }
      }
      def iterator(collection: CC): Iterator[(Key, Value)] = collection.asScala.iterator
    }
}
