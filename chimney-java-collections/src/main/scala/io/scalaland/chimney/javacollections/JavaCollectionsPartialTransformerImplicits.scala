package io.scalaland.chimney.javacollections

import io.scalaland.chimney.{partial, PartialTransformer}

import scala.collection.compat.*

/** @since 0.8.1 */
trait JavaCollectionsPartialTransformerImplicits extends JavaCollectionsCompat {

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaCollectionToScalaCollection[
      JColl[A0] <: java.lang.Iterable[A0],
      SColl[A0] <: IterableOnce[A0],
      A,
      B
  ](implicit aToB: PartialTransformer[A, B], factory: Factory[B, SColl[B]]): PartialTransformer[JColl[A], SColl[B]] =
    (collection, failFast) =>
      partial.Result.traverse[SColl[B], A, B](
        Iterator.unfold(collection.iterator())(it => if (it.hasNext()) Some(it.next() -> it) else None),
        aToB.transform(_, failFast),
        failFast
      )

  /** @since 0.8.1 */
  implicit def totalTransformerFromScalaCollectionToJavaCollection[
      SColl[A0] <: IterableOnce[A0],
      JColl[A0] <: java.lang.Iterable[A0],
      A,
      B
  ](implicit
      aToB: PartialTransformer[A, B],
      factory: JavaFactory[B, JColl[B]]
  ): PartialTransformer[SColl[A], JColl[B]] =
    (collection, failFast) =>
      partial.Result.traverse[JColl[B], A, B](collection.iterator, aToB.transform(_, failFast), failFast)

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaCollectionToJavaCollection[
      JColl1[A0] <: java.lang.Iterable[A0],
      JColl2[A0] <: java.lang.Iterable[A0],
      A,
      B
  ](implicit
      aToB: PartialTransformer[A, B],
      factory: JavaFactory[B, JColl2[B]]
  ): PartialTransformer[JColl1[A], JColl2[B]] =
    (collection, failFast) =>
      partial.Result.traverse[JColl2[B], A, B](
        Iterator.unfold(collection.iterator())(it => if (it.hasNext()) Some(it.next() -> it) else None),
        aToB.transform(_, failFast),
        failFast
      )
}
