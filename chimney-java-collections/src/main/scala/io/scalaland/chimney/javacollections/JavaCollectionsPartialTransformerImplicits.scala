package io.scalaland.chimney.javacollections

import io.scalaland.chimney.{partial, PartialTransformer}

import scala.collection.compat.*

/** @since 0.8.1 */
trait JavaCollectionsPartialTransformerImplicits extends JavaCollectionsCompat {

  // Optionals

  /** @since 0.8.1 */
  implicit def partialTransformerFromJavaOptionalToNonOptional[A, B](implicit
      aToB: PartialTransformer[A, B]
  ): PartialTransformer[java.util.Optional[A], B] =
    (optional, failFast) =>
      optional.map[partial.Result[B]](a => aToB.transform(a, failFast)).orElseGet(() => partial.Result.fromEmpty[B])

  // non-Map collections

  /** @since 0.8.1 */
  implicit def partialTransformerFromJavaCollectionToScalaCollection[
      JColl[A0] <: java.lang.Iterable[A0],
      SColl[A0] <: IterableOnce[A0],
      A,
      B
  ](implicit aToB: PartialTransformer[A, B], factory: Factory[B, SColl[B]]): PartialTransformer[JColl[A], SColl[B]] =
    (collection, failFast) =>
      partial.Result.traverse[SColl[B], (A, Int), B](
        Iterator.unfold(collection.iterator())(it => if (it.hasNext()) Some(it.next() -> it) else None).zipWithIndex,
        ai => aToB.transform(ai._1, failFast).prependErrorPath(partial.PathElement.Index(ai._2)),
        failFast
      )

  /** @since 0.8.1 */
  implicit def partialTransformerFromScalaCollectionToJavaCollection[
      SColl[A0] <: IterableOnce[A0],
      JColl[A0] <: java.lang.Iterable[A0],
      A,
      B
  ](implicit
      aToB: PartialTransformer[A, B],
      factory: JavaFactory[B, JColl[B]]
  ): PartialTransformer[SColl[A], JColl[B]] =
    (collection, failFast) =>
      partial.Result.traverse[JColl[B], (A, Int), B](
        collection.iterator.zipWithIndex,
        ai => aToB.transform(ai._1, failFast).prependErrorPath(partial.PathElement.Index(ai._2)),
        failFast
      )

  /** @since 0.8.1 */
  implicit def partialTransformerFromJavaCollectionToJavaCollection[
      JColl1[A0] <: java.lang.Iterable[A0],
      JColl2[A0] <: java.lang.Iterable[A0],
      A,
      B
  ](implicit
      aToB: PartialTransformer[A, B],
      factory: JavaFactory[B, JColl2[B]]
  ): PartialTransformer[JColl1[A], JColl2[B]] =
    (collection, failFast) =>
      partial.Result.traverse[JColl2[B], (A, Int), B](
        Iterator.unfold(collection.iterator())(it => if (it.hasNext()) Some(it.next() -> it) else None).zipWithIndex,
        ai => aToB.transform(ai._1, failFast).prependErrorPath(partial.PathElement.Index(ai._2)),
        failFast
      )

  // Maps

  /** @since 0.8.1 */
  implicit def partialTransformerFromJavaMapToScalaMap[
      JMap[K0, V0] <: java.util.Map[K0, V0],
      SMap[K0, V0] <: scala.collection.Map[K0, V0],
      K1,
      V1,
      K2,
      V2
  ](implicit
      keys: PartialTransformer[K1, K2],
      values: PartialTransformer[V1, V2],
      factory: Factory[(K2, V2), SMap[K2, V2]]
  ): PartialTransformer[JMap[K1, V1], SMap[K2, V2]] =
    (collection, failFast) =>
      partial.Result.traverse[SMap[K2, V2], java.util.Map.Entry[K1, V1], (K2, V2)](
        Iterator.unfold(collection.entrySet().iterator())(it => if (it.hasNext()) Some(it.next() -> it) else None),
        entry =>
          partial.Result.product(
            keys.transform(entry.getKey).prependErrorPath(partial.PathElement.MapKey(entry.getKey)),
            values.transform(entry.getValue).prependErrorPath(partial.PathElement.MapValue(entry.getKey)),
            failFast
          ),
        failFast
      )

  /** @since 0.8.1 */
  implicit def partialTransformerFromScalaMapToJavaMap[
      SMap[K0, V0] <: scala.collection.Map[K0, V0],
      JMap[K0, V0] <: java.util.Map[K0, V0],
      K1,
      V1,
      K2,
      V2
  ](implicit
      keys: PartialTransformer[K1, K2],
      values: PartialTransformer[V1, V2],
      factory: JavaFactory[(K2, V2), JMap[K2, V2]]
  ): PartialTransformer[SMap[K1, V1], JMap[K2, V2]] =
    (collection, failFast) =>
      partial.Result.traverse[JMap[K2, V2], (K1, V1), (K2, V2)](
        collection.iterator,
        pair =>
          partial.Result.product(
            keys.transform(pair._1).prependErrorPath(partial.PathElement.MapKey(pair._1)),
            values.transform(pair._2).prependErrorPath(partial.PathElement.MapValue(pair._1)),
            failFast
          ),
        failFast
      )

  /** @since 0.8.1 */
  implicit def partialTransformerFromJavaMapToJavaMap[
      JMap1[K0, V0] <: java.util.Map[K0, V0],
      JMap2[K0, V0] <: java.util.Map[K0, V0],
      K1,
      V1,
      K2,
      V2
  ](implicit
      keys: PartialTransformer[K1, K2],
      values: PartialTransformer[V1, V2],
      factory: JavaFactory[(K2, V2), JMap2[K2, V2]]
  ): PartialTransformer[JMap1[K1, V1], JMap2[K2, V2]] =
    (collection, failFast) =>
      partial.Result.traverse[JMap2[K2, V2], java.util.Map.Entry[K1, V1], (K2, V2)](
        Iterator.unfold(collection.entrySet().iterator())(it => if (it.hasNext()) Some(it.next() -> it) else None),
        entry =>
          partial.Result.product(
            keys.transform(entry.getKey).prependErrorPath(partial.PathElement.MapKey(entry.getKey)),
            values.transform(entry.getValue).prependErrorPath(partial.PathElement.MapValue(entry.getKey)),
            failFast
          ),
        failFast
      )
}
