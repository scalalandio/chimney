// Top-level opaque types (owned by the synthetic `Issue962$package` object), exactly like in the issue's reproduction.
package io.scalaland.chimney.fixtures.issue962

opaque type MyInt = Int
object MyInt {
  def apply(value: Int): MyInt = value
}

opaque type MyString = String
object MyString {
  def apply(value: String): MyString = value
}

case class Foo(a: MyInt, b: MyString)
case class Bar(a: MyInt, b: MyString)
case class Baz(a: Option[MyInt], b: Option[MyString])
