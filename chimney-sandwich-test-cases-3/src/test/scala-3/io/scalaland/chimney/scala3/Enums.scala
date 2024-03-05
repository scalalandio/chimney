package io.scalaland.chimney.scala3

object Enums {
  enum Foo {
    case A
    case B
  }
  enum Bar {
    case A(a: Int)
    case B(b: String)
  }
  enum Baz {
    case A
    case B(b: String)
  }
}
