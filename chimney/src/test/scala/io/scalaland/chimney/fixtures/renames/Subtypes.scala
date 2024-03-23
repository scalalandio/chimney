package io.scalaland.chimney.fixtures.renames

object Subtypes {

  sealed trait Foo
  object Foo {
    case object BAZ extends Foo
  }

  sealed trait Bar
  object Bar {
    case object Baz extends Bar
  }
}
