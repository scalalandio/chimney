package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class TotalTransformerMergingSpec extends ChimneySpec {

  group("setting .withFallback(fallbackValue)") {

    test("should merge 2 case classes with disjoint field names") {
      import merges.Disjoint.*, merges.Nested

      Foo(1, "b", 3.0).into[Baz[Double]].withFallback(Bar(4, "e", 6.0)).transform ==> Baz(1, "b", 3.0, 4, "e", 6.0)

      Nested(Foo(1, "b", 3.0))
        .into[Nested[Baz[Double]]]
        .withFallback(Nested(Bar(4, "e", 6.0)))
        .transform ==> Nested(Baz(1, "b", 3.0, 4, "e", 6.0))
    }

    test("should merge 2 case classes with overlapping field names") {
      import merges.Overlapping.*, merges.Nested

      Foo(1, 2.0).into[Baz[Double]].withFallback(Bar("3", 4.0)).transform ==> Baz(1, "3", 2.0)

      Nested(Foo(1, 2.0))
        .into[Nested[Baz[Double]]]
        .withFallback(Nested(Bar("3", 4.0)))
        .transform ==> Nested(Baz(1, "3", 2.0))
    }

  }

  // TODO: same for withFallbackFrom
}
