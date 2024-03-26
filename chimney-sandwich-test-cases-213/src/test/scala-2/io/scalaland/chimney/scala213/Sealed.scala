package io.scalaland.chimney.scala213

object Sealed {
  sealed trait Foo extends Product with Serializable
  object Foo {
    case object A extends Foo
    case object B extends Foo
  }
  sealed trait Bar extends Product with Serializable
  object Bar {
    final case class A(a: Int) extends Bar
    final case class B(b: String) extends Bar
  }
  sealed trait Baz extends Product with Serializable
  object Baz {
    case object A extends Baz
    final case class B(b: String) extends Baz
  }
}
