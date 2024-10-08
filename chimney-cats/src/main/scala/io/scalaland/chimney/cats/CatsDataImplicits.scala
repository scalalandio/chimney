package io.scalaland.chimney.cats

import cats.{Order, Traverse}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySeq, NonEmptySet, NonEmptyVector}
import io.scalaland.chimney.integrations.*
import io.scalaland.chimney.partial

import scala.collection.compat.Factory
import scala.collection.mutable

/** @since 1.0.0 */
trait CatsDataImplicits extends CatsDataImplicitsCompat {

  /** @since 1.5.0 */
  implicit def catsTotalOuterTransformerFromTraverse[F[_]: Traverse, A, B]: TotalOuterTransformer[F[A], F[B], A, B] =
    new TotalOuterTransformer[F[A], F[B], A, B] {

      def transformWithTotalInner(src: F[A], inner: A => B): F[B] = Traverse[F].map(src)(inner)

      def transformWithPartialInner(
          src: F[A],
          failFast: Boolean,
          inner: A => partial.Result[B]
      ): partial.Result[F[B]] = Traverse[F].traverseWithIndexM(src) { (a, idx) =>
        inner(a).prependErrorPath(partial.PathElement.Index(idx))
      }
    }

  /** @since 1.5.0 */
  implicit def catsTotalOuterTransformerForNonEmptyMap[A, B, C: Order, D]
      : TotalOuterTransformer[NonEmptyMap[A, B], NonEmptyMap[C, D], (A, B), (C, D)] =
    new TotalOuterTransformer[NonEmptyMap[A, B], NonEmptyMap[C, D], (A, B), (C, D)] {

      def transformWithTotalInner(src: NonEmptyMap[A, B], inner: ((A, B)) => (C, D)): NonEmptyMap[C, D] =
        src.mapBoth((k, v) => inner(k -> v))

      def transformWithPartialInner(
          src: NonEmptyMap[A, B],
          failFast: Boolean,
          inner: ((A, B)) => partial.Result[(C, D)]
      ): partial.Result[NonEmptyMap[C, D]] = partial.Result
        .traverse[Seq[(C, D)], (A, B), (C, D)](src.toSortedMap.iterator, inner, failFast)
        .map(seq => NonEmptyMap.of(seq.head, seq.tail*))
    }

  /** @since 1.5.0 */
  implicit def catsTotalOuterTransformerForNonEmptySet[A, B: Order]
      : TotalOuterTransformer[NonEmptySet[A], NonEmptySet[B], A, B] =
    new TotalOuterTransformer[NonEmptySet[A], NonEmptySet[B], A, B] {

      def transformWithTotalInner(src: NonEmptySet[A], inner: A => B): NonEmptySet[B] = src.map(inner)

      def transformWithPartialInner(
          src: NonEmptySet[A],
          failFast: Boolean,
          inner: A => partial.Result[B]
      ): partial.Result[NonEmptySet[B]] = partial.Result
        .traverse[Seq[B], A, B](src.toSortedSet.iterator, inner, failFast)
        .map(seq => NonEmptySet.of(seq.head, seq.tail*))
    }

  /** @since 1.0.0 */
  implicit def catsChainIsTotallyBuildIterable[A]: TotallyBuildIterable[Chain[A], A] =
    new TotallyBuildIterable[Chain[A], A] {
      def totalFactory: Factory[A, Chain[A]] = new FactoryCompat[A, Chain[A]] {
        def newBuilder: mutable.Builder[A, Chain[A]] = new FactoryCompat.Builder[A, Chain[A]] {
          private var impl = Chain.empty[A]
          def clear(): Unit = impl = Chain.empty[A]
          def result(): Chain[A] = impl
          def addOne(elem: A): this.type = { impl = impl.append(elem); this }
        }
      }
      def iterator(collection: Chain[A]): Iterator[A] = collection.iterator
    }

  /** @since 1.0.0 */
  implicit def catsNonEmptyChainIsPartiallyBuildIterable[A]: PartiallyBuildIterable[NonEmptyChain[A], A] =
    new PartiallyBuildIterable[NonEmptyChain[A], A] {
      def partialFactory: Factory[A, partial.Result[NonEmptyChain[A]]] =
        new FactoryCompat[A, partial.Result[NonEmptyChain[A]]] {
          def newBuilder: mutable.Builder[A, partial.Result[NonEmptyChain[A]]] =
            new FactoryCompat.Builder[A, partial.Result[NonEmptyChain[A]]] {
              private var impl = Chain.empty[A]
              def clear(): Unit = impl = Chain.empty[A]
              def result(): partial.Result[NonEmptyChain[A]] = partial.Result.fromOption(NonEmptyChain.fromChain(impl))
              def addOne(elem: A): this.type = { impl = impl.append(elem); this }
            }
        }
      def iterator(collection: NonEmptyChain[A]): Iterator[A] = collection.iterator
    }

  /** @since 1.0.0 */
  implicit def catsNonEmptyListIsPartiallyBuildIterable[A]: PartiallyBuildIterable[NonEmptyList[A], A] =
    new PartiallyBuildIterable[NonEmptyList[A], A] {
      def partialFactory: Factory[A, partial.Result[NonEmptyList[A]]] =
        new FactoryCompat[A, partial.Result[NonEmptyList[A]]] {
          def newBuilder: mutable.Builder[A, partial.Result[NonEmptyList[A]]] =
            new FactoryCompat.Builder[A, partial.Result[NonEmptyList[A]]] {
              private val impl = List.newBuilder[A]
              def clear(): Unit = impl.clear()
              def result(): partial.Result[NonEmptyList[A]] =
                partial.Result.fromOption(NonEmptyList.fromList(impl.result()))
              def addOne(elem: A): this.type = { impl += elem; this }
            }
        }
      def iterator(collection: NonEmptyList[A]): Iterator[A] = collection.iterator
    }

  /** @since 1.0.0 */
  implicit def catsNonEmptyMapIsPartiallyBuildMap[K: Ordering, V]: PartiallyBuildMap[NonEmptyMap[K, V], K, V] =
    new PartiallyBuildMap[NonEmptyMap[K, V], K, V] {
      def partialFactory: Factory[(K, V), partial.Result[NonEmptyMap[K, V]]] =
        new FactoryCompat[(K, V), partial.Result[NonEmptyMap[K, V]]] {
          def newBuilder: mutable.Builder[(K, V), partial.Result[NonEmptyMap[K, V]]] =
            new FactoryCompat.Builder[(K, V), partial.Result[NonEmptyMap[K, V]]] {
              private val impl = scala.collection.immutable.SortedMap.newBuilder[K, V]
              def clear(): Unit = impl.clear()
              def result(): partial.Result[NonEmptyMap[K, V]] =
                partial.Result.fromOption(NonEmptyMap.fromMap(impl.result()))
              def addOne(elem: (K, V)): this.type = { impl += elem; this }
            }
        }
      def iterator(collection: NonEmptyMap[K, V]): Iterator[(K, V)] = collection.toSortedMap.iterator
    }

  /** @since 1.0.0 */
  implicit def catsNonEmptySeqIsPartiallyBuildIterable[A]: PartiallyBuildIterable[NonEmptySeq[A], A] =
    new PartiallyBuildIterable[NonEmptySeq[A], A] {
      def partialFactory: Factory[A, partial.Result[NonEmptySeq[A]]] =
        new FactoryCompat[A, partial.Result[NonEmptySeq[A]]] {
          def newBuilder: mutable.Builder[A, partial.Result[NonEmptySeq[A]]] =
            new FactoryCompat.Builder[A, partial.Result[NonEmptySeq[A]]] {
              private val impl = mutable.ListBuffer.empty[A]
              def clear(): Unit = impl.clear()
              def result(): partial.Result[NonEmptySeq[A]] =
                partial.Result.fromOption(NonEmptySeq.fromSeq(impl.result()))
              def addOne(elem: A): this.type = { impl += elem; this }
            }
        }
      def iterator(collection: NonEmptySeq[A]): Iterator[A] = collection.iterator
    }

  /** @since 1.0.0 */
  implicit def catsNonEmptySetIsPartiallyBuildIterable[A: Ordering]: PartiallyBuildIterable[NonEmptySet[A], A] =
    new PartiallyBuildIterable[NonEmptySet[A], A] {
      def partialFactory: Factory[A, partial.Result[NonEmptySet[A]]] =
        new FactoryCompat[A, partial.Result[NonEmptySet[A]]] {
          def newBuilder: mutable.Builder[A, partial.Result[NonEmptySet[A]]] =
            new FactoryCompat.Builder[A, partial.Result[NonEmptySet[A]]] {
              private val impl = scala.collection.immutable.SortedSet.newBuilder[A]
              def clear(): Unit = impl.clear()
              def result(): partial.Result[NonEmptySet[A]] =
                partial.Result.fromOption(NonEmptySet.fromSet(impl.result()))
              def addOne(elem: A): this.type = { impl += elem; this }
            }
        }
      def iterator(collection: NonEmptySet[A]): Iterator[A] = collection.toNonEmptyList.iterator
    }

  /** @since 1.0.0 */
  implicit def catsNonEmptyVectorIsPartiallyBuildIterable[A]: PartiallyBuildIterable[NonEmptyVector[A], A] =
    new PartiallyBuildIterable[NonEmptyVector[A], A] {
      def partialFactory: Factory[A, partial.Result[NonEmptyVector[A]]] =
        new FactoryCompat[A, partial.Result[NonEmptyVector[A]]] {
          def newBuilder: mutable.Builder[A, partial.Result[NonEmptyVector[A]]] =
            new FactoryCompat.Builder[A, partial.Result[NonEmptyVector[A]]] {
              private val impl = Vector.newBuilder[A]
              def clear(): Unit = impl.clear()
              def result(): partial.Result[NonEmptyVector[A]] =
                partial.Result.fromOption(NonEmptyVector.fromVector(impl.result()))
              def addOne(elem: A): this.type = { impl += elem; this }
            }
        }
      def iterator(collection: NonEmptyVector[A]): Iterator[A] = collection.iterator
    }
}
