package io.scalaland.chimney.javacollections

import scala.collection.compat.*
import scala.collection.mutable

private[javacollections] trait JavaCollectionsCompat {

  implicit protected class IteratorOps(private val it: Iterator.type) {

    def unfold[A, S](init: S)(f: S => Option[(A, S)]): Iterator[A] =
      it
        .iterate(Option(null.asInstanceOf[A] -> init)) {
          case Some((_, s)) => f(s)
          case _            => None
        }
        .drop(1)
        .takeWhile(_.isDefined)
        .collect { case Some((a, _)) =>
          a
        }
  }

  implicit protected def javaFactoryToScalaFactory[A, CC](implicit javaFactory: JavaFactory[A, CC]): Factory[A, CC] =
    new FactoryImpl(javaFactory)

  final private class FactoryImpl[From, A, CC](javaFactory: JavaFactory[A, CC])
      extends scala.collection.generic.CanBuildFrom[From, A, CC] {
    override def apply(from: From): mutable.Builder[A, CC] = apply()
    override def apply(): mutable.Builder[A, CC] = new mutable.Builder[A, CC] {
      private var builder = javaFactory.newBuilder
      override def clear(): Unit = builder = javaFactory.newBuilder
      override def result(): CC = builder.result()
      override def +=(elem: A): this.type = { builder.addOne(elem); this }
    }
  }
}
