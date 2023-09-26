package io.scalaland.chimney.javacollections

import scala.collection.compat.*
import scala.collection.mutable

private[javacollections] trait JavaCollectionsCompat {

  implicit protected def javaFactoryToScalaFactory[A, CC](implicit javaFactory: JavaFactory[A, CC]): Factory[A, CC] =
    new FactoryImpl(javaFactory)

  final private class FactoryImpl[A, CC](javaFactory: JavaFactory[A, CC]) extends scala.collection.Factory[A, CC] {
    override def fromSpecific(it: IterableOnce[A]): CC = javaFactory.fromSpecific(it)
    override def newBuilder: mutable.Builder[A, CC] = new mutable.Builder[A, CC] {
      private var inner = javaFactory.newBuilder
      override def clear(): Unit = inner = javaFactory.newBuilder
      override def result(): CC = inner.result()
      override def addOne(elem: A): this.type = { inner.addOne(elem); this }
    }
  }
}
