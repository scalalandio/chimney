package io.scalaland.chimney.internal.runtime

import scala.annotation.unused

sealed trait IsCollection[C] {
  type Item
}
object IsCollection {
  type Of[C, A] = IsCollection[C] { type Item = A }

  private object Impl extends IsCollection[Nothing]

  // build-in Chimney support for collections assumes that they are BOTH Iterable and have a Factory
  implicit def scalaCollectionIsCollection[A, C <: Iterable[A]](implicit
      @unused ev: scala.collection.compat.Factory[A, C]
  ): IsCollection.Of[C, A] = Impl.asInstanceOf[IsCollection.Of[C, A]]
}
