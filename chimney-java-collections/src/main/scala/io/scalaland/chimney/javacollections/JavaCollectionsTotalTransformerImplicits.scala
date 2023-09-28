package io.scalaland.chimney.javacollections

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.javacollections.internal.TransformOrUpcast

import scala.collection.compat.*

/** @since 0.8.1 */
trait JavaCollectionsTotalTransformerImplicits extends JavaCollectionsTotalTransformerLowPriorityImplicits1 {

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

  // from Scala type to any Java type with JavaFactory

  /** @since 0.8.1 */
  implicit def totalTransformerFromScalaCollectionToJavaType[
      SColl[A0] <: IterableOnce[A0],
      JColl,
      A,
      B
  ](implicit factory: JavaFactory[B, JColl], aToB: TransformOrUpcast[A, B]): Transformer[SColl[A], JColl] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.iterator
      while (it.hasNext)
        builder.addOne(aToB.transform(it.next()))
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromScalaMapToJavaType[
      SMap[K0, V0] <: scala.collection.Map[K0, V0],
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
      val it = collection.iterator
      while (it.hasNext) {
        val pair = it.next()
        builder.addOne(keys.transform(pair._1) -> values.transform(pair._2))
      }
      builder.result()
    }

  // from java.util.Iterator

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaIteratorToScalaCollection[
      JColl[A0] <: java.util.Iterator[A0],
      SColl[A0] <: IterableOnce[A0],
      A,
      B
  ](implicit factory: Factory[B, SColl[B]], aToB: TransformOrUpcast[A, B]): Transformer[JColl[A], SColl[B]] =
    it => {
      val builder = factory.newBuilder
      while (it.hasNext())
        builder += aToB.transform(it.next())
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaIteratorToJavaType[
      JColl1[A0] <: java.util.Iterator[A0],
      JColl2,
      A,
      B
  ](implicit factory: JavaFactory[B, JColl2], aToB: TransformOrUpcast[A, B]): Transformer[JColl1[A], JColl2] =
    it => {
      val builder = factory.newBuilder
      while (it.hasNext())
        builder.addOne(aToB.transform(it.next()))
      builder.result()
    }

  // from java.util.Enumeration

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaEnumerationToScalaCollection[
      JColl[A0] <: java.util.Enumeration[A0],
      SColl[A0] <: IterableOnce[A0],
      A,
      B
  ](implicit factory: Factory[B, SColl[B]], aToB: TransformOrUpcast[A, B]): Transformer[JColl[A], SColl[B]] =
    it => {
      val builder = factory.newBuilder
      while (it.hasMoreElements())
        builder += aToB.transform(it.nextElement())
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaEnumerationToJavaType[
      JColl1[A0] <: java.util.Enumeration[A0],
      JColl2,
      A,
      B
  ](implicit factory: JavaFactory[B, JColl2], aToB: TransformOrUpcast[A, B]): Transformer[JColl1[A], JColl2] =
    it => {
      val builder = factory.newBuilder
      while (it.hasMoreElements())
        builder.addOne(aToB.transform(it.nextElement()))
      builder.result()
    }

  // from java.util.Iterable (java.util.Collection)

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaCollectionToScalaCollection[
      JColl[A0] <: java.lang.Iterable[A0],
      SColl[A0] <: IterableOnce[A0],
      A,
      B
  ](implicit factory: Factory[B, SColl[B]], aToB: TransformOrUpcast[A, B]): Transformer[JColl[A], SColl[B]] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.iterator()
      while (it.hasNext())
        builder += aToB.transform(it.next())
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaCollectionToJavaType[
      JColl1[A0] <: java.lang.Iterable[A0],
      JColl2,
      A,
      B
  ](implicit factory: JavaFactory[B, JColl2], aToB: TransformOrUpcast[A, B]): Transformer[JColl1[A], JColl2] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.iterator()
      while (it.hasNext())
        builder.addOne(aToB.transform(it.next()))
      builder.result()
    }

  // from java.util.Map

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaMapToScalaMap[
      JMap[K0, V0] <: java.util.Map[K0, V0],
      SMap[K0, V0] <: scala.collection.Map[K0, V0],
      K1,
      V1,
      K2,
      V2
  ](implicit
      keys: TransformOrUpcast[K1, K2],
      values: TransformOrUpcast[V1, V2],
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
  implicit def totalTransformerFromJavaMapToJavaType[
      JMap1[K0, V0] <: java.util.Map[K0, V0],
      JMap2,
      K1,
      V1,
      K2,
      V2
  ](implicit
      factory: JavaFactory[(K2, V2), JMap2],
      keys: TransformOrUpcast[K1, K2],
      values: TransformOrUpcast[V1, V2]
  ): Transformer[JMap1[K1, V1], JMap2] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.entrySet().iterator()
      while (it.hasNext()) {
        val entry = it.next()
        builder.addOne(keys.transform(entry.getKey) -> values.transform(entry.getValue))
      }
      builder.result()
    }

  // from java.util.BitSet

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaBitSetToScalaCollection[
      SColl[A0] <: IterableOnce[A0],
      B
  ](implicit factory: Factory[B, SColl[B]], aToB: TransformOrUpcast[Int, B]): Transformer[java.util.BitSet, SColl[B]] =
    collection => {
      val builder = factory.newBuilder
      (0 until collection.size()).foreach { i =>
        if (collection.get(i))
          builder += aToB.transform(i)
      }
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaBitSetToJavaType[
      JColl2,
      B
  ](implicit
      factory: JavaFactory[B, JColl2],
      aToB: TransformOrUpcast[Int, B]
  ): Transformer[java.util.BitSet, JColl2] =
    collection => {
      val builder = factory.newBuilder
      (0 until collection.size()).foreach { i =>
        if (collection.get(i))
          builder.addOne(aToB.transform(i))
      }
      builder.result()
    }
}

private[javacollections] trait JavaCollectionsTotalTransformerLowPriorityImplicits1 {

  // from java.util.Dictionary

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaDictionaryToScalaMap[
      JMap[K0, V0] <: java.util.Dictionary[K0, V0],
      SMap[K0, V0] <: scala.collection.Map[K0, V0],
      K1,
      V1,
      K2,
      V2
  ](implicit
      keys: TransformOrUpcast[K1, K2],
      values: TransformOrUpcast[V1, V2],
      factory: Factory[(K2, V2), SMap[K2, V2]]
  ): Transformer[JMap[K1, V1], SMap[K2, V2]] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.keys()
      while (it.hasMoreElements()) {
        val key = it.nextElement()
        builder += (keys.transform(key) -> values.transform(collection.get(key)))
      }
      builder.result()
    }

  /** @since 0.8.1 */
  implicit def totalTransformerFromJavaDictionaryToJavaType[
      JMap1[K0, V0] <: java.util.Dictionary[K0, V0],
      JMap2,
      K1,
      V1,
      K2,
      V2
  ](implicit
      factory: JavaFactory[(K2, V2), JMap2],
      keys: TransformOrUpcast[K1, K2],
      values: TransformOrUpcast[V1, V2]
  ): Transformer[JMap1[K1, V1], JMap2] =
    collection => {
      val builder = factory.newBuilder
      val it = collection.keys()
      while (it.hasMoreElements()) {
        val key = it.nextElement()
        builder.addOne(keys.transform(key) -> values.transform(collection.get(key)))
      }
      builder.result()
    }
}
