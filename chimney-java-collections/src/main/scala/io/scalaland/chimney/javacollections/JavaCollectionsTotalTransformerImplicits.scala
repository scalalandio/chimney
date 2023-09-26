package io.scalaland.chimney.javacollections

import io.scalaland.chimney.Transformer

import scala.collection.compat.*

/** @since 0.8.1 */
trait JavaCollectionsTotalTransformerImplicits {

  // non-Map collections

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaCollectionToScalaCollection[
      JColl[A0] <: java.lang.Iterable[A0],
      SColl[A0] <: IterableOnce[A0],
      A,
      B
  ](implicit aToB: Transformer[A, B], factory: Factory[B, SColl[B]]): Transformer[JColl[A], SColl[B]] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.iterator()
      while (it.hasNext())
        builder += aToB.transform(it.next())
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromScalaCollectionToJavaCollection[
      SColl[A0] <: IterableOnce[A0],
      JColl[A0] <: java.lang.Iterable[A0],
      A,
      B
  ](implicit aToB: Transformer[A, B], factory: JavaFactory[B, JColl[B]]): Transformer[SColl[A], JColl[B]] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.iterator
      while (it.hasNext)
        builder.addOne(aToB.transform(it.next()))
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaCollectionToJavaCollection[
      JColl1[A0] <: java.lang.Iterable[A0],
      JColl2[A0] <: java.lang.Iterable[A0],
      A,
      B
  ](implicit aToB: Transformer[A, B], factory: JavaFactory[B, JColl2[B]]): Transformer[JColl1[A], JColl2[B]] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.iterator()
      while (it.hasNext())
        builder.addOne(aToB.transform(it.next()))
      builder.result()
    }

  // Maps

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaMapToScalaMap[
      JMap[K0, V0] <: java.util.Map[K0, V0],
      SMap[K0, V0] <: scala.collection.Map[K0, V0],
      K1,
      V1,
      K2,
      V2
  ](implicit
      keys: Transformer[K1, K2],
      values: Transformer[V1, V2],
      factory: Factory[(K2, V2), SMap[K2, V2]]
  ): Transformer[JMap[K1, V1], SMap[K2, V2]] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.entrySet().iterator()
      while (it.hasNext()) {
        val entry = it.next()
        builder += (keys.transform(entry.getKey) -> values.transform(entry.getValue))
      }
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromScalaMapToJavaMap[
      SMap[K0, V0] <: scala.collection.Map[K0, V0],
      JMap[K0, V0] <: java.util.Map[K0, V0],
      K1,
      V1,
      K2,
      V2
  ](implicit
      keys: Transformer[K1, K2],
      values: Transformer[V1, V2],
      factory: JavaFactory[(K2, V2), JMap[K2, V2]]
  ): Transformer[SMap[K1, V1], JMap[K2, V2]] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.iterator
      while (it.hasNext) {
        val pair = it.next()
        builder.addOne(keys.transform(pair._1) -> values.transform(pair._2))
      }
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaMapToJavaMap[
      JMap1[K0, V0] <: java.util.Map[K0, V0],
      JMap2[K0, V0] <: java.util.Map[K0, V0],
      K1,
      V1,
      K2,
      V2
  ](implicit
      keys: Transformer[K1, K2],
      values: Transformer[V1, V2],
      factory: JavaFactory[(K2, V2), JMap2[K2, V2]]
  ): Transformer[JMap1[K1, V1], JMap2[K2, V2]] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.entrySet().iterator()
      while (it.hasNext()) {
        val entry = it.next()
        builder.addOne(keys.transform(entry.getKey) -> values.transform(entry.getValue))
      }
      builder.result()
    }

  // TODO: Optionals

  // TODO: Streams ?
}
