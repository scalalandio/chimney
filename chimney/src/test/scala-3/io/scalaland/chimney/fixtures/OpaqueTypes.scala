package io.scalaland.chimney.fixtures

package opaquetypes {

  case class Foo(value: String)

  opaque type Bar = Int
  extension (bar: Bar)
    def value: Int = bar
  object Bar {
    def parse(value: String): Either[String, Bar] =
      scala.util.Try(value.toInt).toEither.left.map(_.getMessage)
  }
}
