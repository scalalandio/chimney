package io.scalaland.chimney.scala3

object Defaults {
  final case class Foo(a: Int = 0, b: String = "", c: Double = 0.0, d: Boolean = false)
  final case class Bar(a: Int = 0, b: String = "", c: Double = 0.0)
  final case class Baz(a: Int = 0, b: String = "", c0: Double = 0.0, d: Int = 0)
}
