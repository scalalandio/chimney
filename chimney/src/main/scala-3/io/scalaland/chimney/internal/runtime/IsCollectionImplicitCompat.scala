package io.scalaland.chimney.internal.runtime

private[runtime] trait IsCollectionImplicitCompat { this: IsCollection.type =>

  // build-in Chimney support for IArrays is always provided - on Scala 3
  implicit def iarrayIsCollection[A]: IsCollection.Of[IArray[A], A] = Impl.asInstanceOf[IsCollection.Of[IArray[A], A]]
}
