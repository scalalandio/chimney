package io.scalaland.chimney.javacollections

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.javacollections.internal.TransformOrUpcast

import scala.collection.compat.*

/** @since 0.8.1 */
trait JavaCollectionsTotalTransformerImplicits extends JavaCollectionsTotalTransformerImplicitsLowPriority {

  // from/to java.util.Optional

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaOptionalToScalaOption[A, B](implicit
      aToB: TransformOrUpcast[A, B]
  ): Transformer[java.util.Optional[A], Option[B]] =
    optional => optional.map[Option[B]](a => Some(aToB.transform(a))).orElseGet(() => None)

  /** @since 0.8.1 */
  implicit def totalTransformerFromScalaOptionToJavaOptional[A, B](implicit
      aToB: TransformOrUpcast[A, B]
  ): Transformer[Option[A], java.util.Optional[B]] =
    option => option.fold(java.util.Optional.empty[B]())(a => java.util.Optional.of(aToB.transform(a)))

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaOptionalToJavaOptional[A, B](implicit
      aToB: TransformOrUpcast[A, B]
  ): Transformer[java.util.Optional[A], java.util.Optional[B]] =
    optional => optional.map(a => aToB.transform(a))

  /** @since 0.8.1 */
  implicit def totalTransformerFromNonOptionalToJavaOptional[A, B](implicit
      aToB: TransformOrUpcast[A, B]
  ): Transformer[A, java.util.Optional[B]] =
    a => java.util.Optional.of(aToB.transform(a))

  // from types with JavaIterator/to types with JavaFactory

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaCollectionToScalaCollection[
      JColl,
      SColl[A0] <: IterableOnce[A0],
      A,
      B
  ](implicit
      iterator: JavaIterator.Aux[A, JColl],
      factory: Factory[B, SColl[B]],
      aToB: TransformOrUpcast[A, B]
  ): Transformer[JColl, SColl[B]] =
    collection => {
      val builder = factory.newBuilder
      iterator.foreach(collection) { a =>
        builder += aToB.transform(a)
      }
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromScalaCollectionToJavaCollection[
      SColl[A0] <: IterableOnce[A0],
      JColl,
      A,
      B
  ](implicit factory: JavaFactory[B, JColl], aToB: TransformOrUpcast[A, B]): Transformer[SColl[A], JColl] =
    collection => {
      val builder = factory.newBuilder
      collection.iterator.foreach { a =>
        builder.addOne(aToB.transform(a))
      }
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaCollectionToJavaCollection[
      JColl1,
      JColl2,
      A,
      B
  ](implicit
      iterator: JavaIterator.Aux[A, JColl1],
      factory: JavaFactory[B, JColl2],
      aToB: TransformOrUpcast[A, B]
  ): Transformer[JColl1, JColl2] =
    collection => {
      val builder = factory.newBuilder
      iterator.foreach(collection) { a =>
        builder.addOne(aToB.transform(a))
      }
      builder.result()
    }
}

private[javacollections] trait JavaCollectionsTotalTransformerImplicitsLowPriority {

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaMapToScalaMap[
      JMap,
      SMap[K0, V0] <: IterableOnce[(K0, V0)],
      K1,
      V1,
      K2,
      V2
  ](implicit
      iterator: JavaIterator.Aux[(K1, V1), JMap],
      factory: Factory[(K2, V2), SMap[K2, V2]],
      keys: TransformOrUpcast[K1, K2],
      values: TransformOrUpcast[V1, V2]
  ): Transformer[JMap, SMap[K2, V2]] =
    collection => {
      val builder = factory.newBuilder
      iterator.foreach(collection) { case (k, v) =>
        builder += (keys.transform(k) -> values.transform(v))
      }
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromScalaMapToJavaMap[
      SMap[K0, V0] <: IterableOnce[(K0, V0)],
      JMap,
      K1,
      V1,
      K2,
      V2
  ](implicit
      factory: JavaFactory[(K2, V2), JMap],
      keys: TransformOrUpcast[K1, K2],
      values: TransformOrUpcast[V1, V2]
  ): Transformer[SMap[K1, V1], JMap] =
    collection => {
      val builder = factory.newBuilder
      collection.iterator.foreach { case (k, v) =>
        builder.addOne(keys.transform(k) -> values.transform(v))
      }
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaMapToJavaMap[
      JMap1,
      JMap2,
      K1,
      V1,
      K2,
      V2
  ](implicit
      iterator: JavaIterator.Aux[(K1, V1), JMap1],
      factory: JavaFactory[(K2, V2), JMap2],
      keys: TransformOrUpcast[K1, K2],
      values: TransformOrUpcast[V1, V2]
  ): Transformer[JMap1, JMap2] =
    collection => {
      val builder = factory.newBuilder
      iterator.foreach(collection) { case (k, v) =>
        builder.addOne(keys.transform(k) -> values.transform(v))
      }
      builder.result()
    }
}
