package io.scalaland.chimney.internal.runtime

import scala.annotation.{implicitNotFound, unused}

@implicitNotFound(
  "Expected collection (type extending scala.Iterable which has scala.collection.compat.Factory instance), got ${C}"
)
sealed trait IsCollection[C] {
  type Item
}
object IsCollection extends IsCollection1 {
  @implicitNotFound(
    "Expected collection (type extending scala.Iterable which has scala.collection.compat.Factory instance), got ${C}"
  )
  type Of[C, A] = IsCollection[C] { type Item = A }

  protected object Impl extends IsCollection[Nothing]

  // build-in Chimney support for Arrays is always provided
  implicit def arrayIsCollection[A]: IsCollection.Of[Array[A], A] = Impl.asInstanceOf[IsCollection.Of[Array[A], A]]
}
private[runtime] trait IsCollection1 { this: IsCollection.type =>

  // build-in Chimney support for collections assumes that they are BOTH Iterable and have a Factory
  implicit def scalaCollectionIsCollection[A, C <: Iterable[A]](implicit
      @unused ev: scala.collection.compat.Factory[A, C]
  ): IsCollection.Of[C, A] = Impl.asInstanceOf[IsCollection.Of[C, A]]
}
