package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class TotalMergingSealedHiearchySpec extends ChimneySpec {

  import TotalMergingSealedHiearchySpec.*

  group("setting .withFallback(fallbackValue)") {

    test("should merge each subtype of sealed hierarchy with fallback to fill fields missing in target subtype") {
      import merges.Nested

      (Foo.One(10): Foo).into[Baz].withFallback(Bar(true)).transform ==> Baz.One(10, true)
      (Foo.Two("test"): Foo).into[Baz].withFallback(Bar(true)).transform ==> Baz.Two("test", true)

      Nested(Foo.One(10): Foo)
        .into[Nested[Baz]]
        .withFallback(Nested(Bar(true)))
        .transform ==> Nested(Baz.One(10, true))
      Nested(Foo.Two("test"): Foo)
        .into[Nested[Baz]]
        .withFallback(Nested(Bar(true)))
        .transform ==> Nested(Baz.Two("test", true))
    }
  }

  group("setting .withFallbackFrom(selectorFrom)(fallbackValue)") {

    test("should merge each subtype of sealed hierarchy with fallback to fill fields missing in target subtype") {
      import merges.Nested

      Nested(Foo.One(10): Foo)
        .into[Nested[Baz]]
        .withFallbackFrom(_.value)(Bar(true))
        .transform ==> Nested(Baz.One(10, true))
      Nested(Foo.Two("test"): Foo)
        .into[Nested[Baz]]
        .withFallbackFrom(_.value)(Bar(true))
        .transform ==> Nested(Baz.Two("test", true))

      Nested(Nested(Foo.One(10): Foo))
        .into[Nested[Nested[Baz]]]
        .withFallbackFrom(_.value)(Nested(Bar(true)))
        .transform ==> Nested(Nested(Baz.One(10, true)))
      Nested(Nested(Foo.Two("test"): Foo))
        .into[Nested[Nested[Baz]]]
        .withFallbackFrom(_.value)(Nested(Bar(true)))
        .transform ==> Nested(Nested(Baz.Two("test", true)))
    }
  }
}
object TotalMergingSealedHiearchySpec {

  sealed trait Foo extends Product with Serializable
  object Foo {
    case class One(a: Int) extends Foo
    case class Two(b: String) extends Foo
  }

  case class Bar(c: Boolean)

  sealed trait Baz extends Product with Serializable
  object Baz {
    case class One(a: Int, c: Boolean) extends Baz
    case class Two(b: String, c: Boolean) extends Baz
  }
}
