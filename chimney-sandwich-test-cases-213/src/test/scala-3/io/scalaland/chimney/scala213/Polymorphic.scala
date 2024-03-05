package io.scalaland.chimney.scala213

object Polymorphic {
  final case class Foo[A, B](a: A, b: B, c: Double, d: Boolean)
  final case class Bar[A, B](a: A, b: B, c: Double)
  final case class Baz[A, B](a: A, b: B, c0: Double, d: Int)
}
