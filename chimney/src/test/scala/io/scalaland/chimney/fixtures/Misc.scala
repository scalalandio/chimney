package io.scalaland.chimney.fixtures

case class PolyBar[A](value: String, poly: A)

object PolyBar {
  type UnitBar = PolyBar[Unit]
}

class BaseFoo(val x: Int)
case class CaseBar(override val x: Int) extends BaseFoo(x)

object MutualRec {
  case class Baz[A](bar: Option[A])

  case class Bar1(x: Int, foo: Baz[Bar1])

  case class Bar2(foo: Baz[Bar2])

}
