package io.scalaland.chimney.cats

import cats.data.NonEmptyLazyList
import io.scalaland.chimney.integrations.*
import io.scalaland.chimney.partial

import scala.collection.compat.Factory
import scala.collection.mutable

/** @since 1.0.0 */
private[cats] trait CatsDataImplicitsCompat extends CatsDataImplicitsLowPriority {

  /** @since 1.0.0 */
  implicit def catsNonEmptyLazyListIsPartiallyBuildIterable[A]: PartiallyBuildIterable[NonEmptyLazyList[A], A] =
    new PartiallyBuildIterable[NonEmptyLazyList[A], A] {
      def partialFactory: Factory[A, partial.Result[NonEmptyLazyList[A]]] =
        new FactoryCompat[A, partial.Result[NonEmptyLazyList[A]]] {
          def newBuilder: mutable.Builder[A, partial.Result[NonEmptyLazyList[A]]] =
            new FactoryCompat.Builder[A, partial.Result[NonEmptyLazyList[A]]] {
              private val impl = mutable.ListBuffer.empty[A]
              def clear(): Unit = impl.clear()
              def result(): partial.Result[NonEmptyLazyList[A]] =
                partial.Result.fromOption(NonEmptyLazyList.fromSeq(impl.result()))
              def addOne(elem: A): this.type = { impl += elem; this }
            }
        }
      def iterator(collection: NonEmptyLazyList[A]): Iterator[A] = collection.iterator
    }
}
