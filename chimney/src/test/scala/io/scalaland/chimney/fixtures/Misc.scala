package io.scalaland.chimney.fixtures

case class PolyBar[T](value: String, poly: T)

object PolyBar {
  type UnitBar = PolyBar[Unit]
}

class BaseFoo(val x: Int)
case class CaseBar(override val x: Int) extends BaseFoo(x)

object MutualRec {
  case class Baz[T](bar: Option[T])

  case class Bar1(x: Int, foo: Baz[Bar1])

  case class Bar2(foo: Baz[Bar2])

}
