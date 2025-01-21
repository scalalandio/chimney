package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class PartialMergingSealedHiearchySpec extends ChimneySpec {

  import TotalMergingSealedHiearchySpec.*

  group("setting .withFallback(fallbackValue)") {

    test("should merge each subtype of sealed hierarchy with fallback to fill fields missing in target subtype") {
      import merges.Nested

      (Foo.One(10): Foo)
        .intoPartial[Baz]
        .withFallback(Bar(true))
        .transform
        .asOption ==> Some(Baz.One(10, true))
      (Foo.Two("test"): Foo)
        .intoPartial[Baz]
        .withFallback(Bar(true))
        .transform
        .asOption ==> Some(Baz.Two("test", true))

      Nested(Foo.One(10): Foo)
        .intoPartial[Nested[Baz]]
        .withFallback(Nested(Bar(true)))
        .transform
        .asOption ==> Some(Nested(Baz.One(10, true)))
      Nested(Foo.Two("test"): Foo)
        .intoPartial[Nested[Baz]]
        .withFallback(Nested(Bar(true)))
        .transform
        .asOption ==> Some(Nested(Baz.Two("test", true)))
    }
  }

  group("setting .withFallbackFrom(selectorFrom)(fallbackValue)") {

    test("should merge each subtype of sealed hierarchy with fallback to fill fields missing in target subtype") {
      import merges.Nested

      Nested(Foo.One(10): Foo)
        .intoPartial[Nested[Baz]]
        .withFallbackFrom(_.value)(Bar(true))
        .transform
        .asOption ==> Some(Nested(Baz.One(10, true)))
      Nested(Foo.Two("test"): Foo)
        .intoPartial[Nested[Baz]]
        .withFallbackFrom(_.value)(Bar(true))
        .transform
        .asOption ==> Some(Nested(Baz.Two("test", true)))

      Nested(Nested(Foo.One(10): Foo))
        .intoPartial[Nested[Nested[Baz]]]
        .withFallbackFrom(_.value)(Nested(Bar(true)))
        .transform
        .asOption ==> Some(Nested(Nested(Baz.One(10, true))))
      Nested(Nested(Foo.Two("test"): Foo))
        .intoPartial[Nested[Nested[Baz]]]
        .withFallbackFrom(_.value)(Nested(Bar(true)))
        .transform
        .asOption ==> Some(Nested(Nested(Baz.Two("test", true))))
    }
  }
}
