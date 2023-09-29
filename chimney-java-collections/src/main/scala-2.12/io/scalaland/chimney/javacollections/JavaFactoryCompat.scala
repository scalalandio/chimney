package io.scalaland.chimney.javacollections

import scala.collection.compat.*
import scala.collection.mutable
import scala.language.implicitConversions

private[javacollections] trait JavaFactoryCompat {

  implicit def convertJavaFactoryToScalaFactory[A, CC](javaFactory: JavaFactory[A, CC]): Factory[A, CC] =
    new FactoryImpl(javaFactory)

  implicit def provideScalaFactoryFromJavaFactory[A, CC](implicit javaFactory: JavaFactory[A, CC]): Factory[A, CC] =
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
