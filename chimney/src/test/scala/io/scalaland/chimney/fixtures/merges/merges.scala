package io.scalaland.chimney.fixtures.merges

object Disjoint {

  case class Foo[A](a: Int, b: String, c: A)
  case class Bar[A](d: Int, e: String, f: A)

  case class Baz[A](a: Int, b: String, c: A, d: Int, e: String, f: A)
}

object Overlapping {

  case class Foo[A](a: Int, c: A)
  case class Bar[A](b: String, c: A)

  case class Baz[A](a: Int, b: String, c: A)
}

case class Nested[A](value: A)
