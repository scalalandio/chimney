package io.scalaland.chimney.fixtures.renames

object Subtypes {

  sealed trait Foo
  object Foo {
    case object BAZ_BAZ extends Foo
  }

  sealed trait Bar
  object Bar {
    case object BazBaz extends Bar
  }

  sealed trait Foo2
  object Foo2 {
    case object bazBaz extends Foo2
  }

  sealed trait BarAmbiguous
  object BarAmbiguous {
    case object getBazBaz extends BarAmbiguous
    case object bazBaz extends BarAmbiguous
  }
}
