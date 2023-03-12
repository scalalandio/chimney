package io.scalaland.chimney.examples

object foo {
  import io.scalaland.chimney.dsl.*

  sealed trait A extends Product with Serializable
  sealed trait AA extends A
  case object A1 extends AA

  object into {
    sealed trait A extends Product with Serializable
    sealed trait AA extends A
    case object A1 extends AA
  }

  def convert(a: A): into.A = a.transformInto[into.A]
}
