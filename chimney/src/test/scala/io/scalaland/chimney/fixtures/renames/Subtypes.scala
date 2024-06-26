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

  sealed trait Foo2
  object Foo2 {
    case object baz extends Foo2
  }

  sealed trait BarAmbiguous
  object BarAmbiguous {
    case object getBaz extends BarAmbiguous
    case object baz extends BarAmbiguous
  }

  sealed trait Foo3
  object Foo3 {
    case object Baz extends Foo3
    case object Bazz extends Foo3
  }
}
