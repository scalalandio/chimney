package io.scalaland.chimney.javacollections

import io.scalaland.chimney.{partial, PartialTransformer}
import io.scalaland.chimney.javacollections.internal.PartialTransformOrUpcast
import io.scalaland.chimney.javacollections.JavaFactory.ConversionToScalaFactory.*

import scala.collection.compat.*

/** @since 0.8.0 */
trait JavaCollectionsPartialTransformerImplicits extends JavaCollectionsPartialTransformerImplicitsLowPriority {

  // from/to java.util.Optional

  /** @since 0.8.0 */
  implicit def partialTransformerFromJavaOptionalToScalaOption[A, B](implicit
      aToB: PartialTransformOrUpcast[A, B]
  ): PartialTransformer[java.util.Optional[A], Option[B]] =
    (optional, failFast) =>
      optional
        .map[partial.Result[Option[B]]](a => aToB.transform(a, failFast).map(Option(_)))
        .orElseGet(() => partial.Result.fromValue(None))

  /** @since 0.8.0 */
  implicit def partialTransformerFromScalaOptionToJavaOptional[A, B](implicit
      aToB: PartialTransformOrUpcast[A, B]
  ): PartialTransformer[Option[A], java.util.Optional[B]] =
    (option, failFast) =>
      option.fold(partial.Result.fromValue(java.util.Optional.empty[B]())) { a =>
        aToB.transform(a, failFast).map(java.util.Optional.of(_))
      }

  /** @since 0.8.0 */
  implicit def partialTransformerFromJavaOptionalToJavaOptional[A, B](implicit
      aToB: PartialTransformOrUpcast[A, B]
  ): PartialTransformer[java.util.Optional[A], java.util.Optional[B]] =
    (optional, failFast) =>
      optional
        .map[partial.Result[java.util.Optional[B]]](a => aToB.transform(a, failFast).map(java.util.Optional.of(_)))
        .orElseGet(() => partial.Result.fromValue(java.util.Optional.empty()))

  /** @since 0.8.0 */
  implicit def partialTransformerFromNonOptionalToJavaOptional[A, B](implicit
      aToB: PartialTransformOrUpcast[A, B]
  ): PartialTransformer[A, java.util.Optional[B]] =
    (a, failFast) => aToB.transform(a, failFast).map(java.util.Optional.of(_))

  /** @since 0.8.0 */
  implicit def partialTransformerFromJavaOptionalToNonOptional[A, B](implicit
      aToB: PartialTransformOrUpcast[A, B]
  ): PartialTransformer[java.util.Optional[A], B] =
    (optional, failFast) =>
      optional.map[partial.Result[B]](a => aToB.transform(a, failFast)).orElseGet(() => partial.Result.fromEmpty[B])

  // from types with JavaIterator/to types with JavaFactory

  /** @since 0.8.0 */
  implicit def partialTransformerFromJavaCollectionToScalaCollection[
      JColl,
      SColl[A0] <: IterableOnce[A0],
      A,
      B
  ](implicit
      iterator: JavaIterator[A, JColl],
      factory: Factory[B, SColl[B]],
      aToB: PartialTransformOrUpcast[A, B]
  ): PartialTransformer[JColl, SColl[B]] =
    (collection, failFast) =>
      partial.Result.traverse[SColl[B], (A, Int), B](
        iterator.iterator(collection).zipWithIndex,
        { case (a, i) => aToB.transform(a, failFast).prependErrorPath(partial.PathElement.Index(i)) },
        failFast
      )

  /** @since 0.8.0 */
  implicit def partialTransformerFromScalaCollectionToJavaCollection[
      SColl[A0] <: IterableOnce[A0],
      JColl,
      A,
      B
  ](implicit
      factory: JavaFactory[B, JColl],
      aToB: PartialTransformOrUpcast[A, B]
  ): PartialTransformer[SColl[A], JColl] =
    (collection, failFast) =>
      partial.Result.traverse[JColl, (A, Int), B](
        collection.iterator.zipWithIndex,
        { case (a, i) => aToB.transform(a, failFast).prependErrorPath(partial.PathElement.Index(i)) },
        failFast
      )

  /** @since 0.8.0 */
  implicit def partialTransformerFromJavaCollectionToJavaCollection[
      JColl1,
      JColl2,
      A,
      B
  ](implicit
      iterator: JavaIterator[A, JColl1],
      factory: JavaFactory[B, JColl2],
      aToB: PartialTransformOrUpcast[A, B]
  ): PartialTransformer[JColl1, JColl2] =
    (collection, failFast) =>
      partial.Result.traverse[JColl2, (A, Int), B](
        iterator.iterator(collection).zipWithIndex,
        { case (a, i) => aToB.transform(a, failFast).prependErrorPath(partial.PathElement.Index(i)) },
        failFast
      )
}

private[javacollections] trait JavaCollectionsPartialTransformerImplicitsLowPriority {

  /** @since 0.8.0 */
  implicit def partialTransformerFromJavaMapToScalaMap[
      JMap,
      SMap[K0, V0] <: IterableOnce[(K0, V0)],
      K1,
      V1,
      K2,
      V2
  ](implicit
      iterator: JavaIterator[(K1, V1), JMap],
      factory: Factory[(K2, V2), SMap[K2, V2]],
      keys: PartialTransformOrUpcast[K1, K2],
      values: PartialTransformOrUpcast[V1, V2]
  ): PartialTransformer[JMap, SMap[K2, V2]] =
    (collection, failFast) =>
      partial.Result.traverse[SMap[K2, V2], (K1, V1), (K2, V2)](
        iterator.iterator(collection),
        { case (k, v) =>
          partial.Result
            .product(
              keys.transform(k, failFast).prependErrorPath(partial.PathElement.MapKey(k)),
              values.transform(v, failFast),
              failFast
            )
            .prependErrorPath(partial.PathElement.MapValue(k))
        },
        failFast
      )

  /** @since 0.8.0 */
  implicit def partialTransformerFromScalaMapToJavaMap[
      SMap[K0, V0] <: IterableOnce[(K0, V0)],
      JMap,
      K1,
      V1,
      K2,
      V2
  ](implicit
      factory: JavaFactory[(K2, V2), JMap],
      keys: PartialTransformOrUpcast[K1, K2],
      values: PartialTransformOrUpcast[V1, V2]
  ): PartialTransformer[SMap[K1, V1], JMap] =
    (collection, failFast) =>
      partial.Result.traverse[JMap, (K1, V1), (K2, V2)](
        collection.iterator,
        { case (k, v) =>
          partial.Result
            .product(
              keys.transform(k, failFast).prependErrorPath(partial.PathElement.MapKey(k)),
              values.transform(v, failFast),
              failFast
            )
            .prependErrorPath(partial.PathElement.MapValue(k))
        },
        failFast
      )

  /** @since 0.8.0 */
  implicit def partialTransformerFromJavaMapToJavaMap[
      JMap1,
      JMap2,
      K1,
      V1,
      K2,
      V2
  ](implicit
      iterator: JavaIterator[(K1, V1), JMap1],
      factory: JavaFactory[(K2, V2), JMap2],
      keys: PartialTransformOrUpcast[K1, K2],
      values: PartialTransformOrUpcast[V1, V2]
  ): PartialTransformer[JMap1, JMap2] =
    (collection, failFast) =>
      partial.Result.traverse[JMap2, (K1, V1), (K2, V2)](
        iterator.iterator(collection),
        { case (k, v) =>
          partial.Result
            .product(
              keys.transform(k, failFast).prependErrorPath(partial.PathElement.MapKey(k)),
              values.transform(v, failFast),
              failFast
            )
            .prependErrorPath(partial.PathElement.MapValue(k))
        },
        failFast
      )
}
