package io.scalaland.chimney.javacollections

import io.scalaland.chimney.Transformer

import scala.collection.compat.*

/** @since 0.8.1 */
trait JavaCollectionsTotalTransformerImplicits {

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

  // TODO: Maps

  // TODO: Optionals

  // TODO: Streams ?
}
