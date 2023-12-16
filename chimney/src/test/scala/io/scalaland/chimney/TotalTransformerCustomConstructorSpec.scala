package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class TotalTransformerCustomConstructorSpec extends ChimneySpec {

  // TODO: test rejecting non-constructors

  test("""transformation from a "superset" of fields into a "subset" of fields without modifiers""") {
    import products.{Foo, Bar}

    // TODO: test unary/nulllary - constConstructor
    // TODO: rename customConstructor -> binaryConstructor
    // TODO: add tests with multiple parameter lists - paramListsConstructor
    // TODO: test eta expansion of method with type params - typeParametricConstructor

    def customConstructor(x: Int, z: (Double, Double)): Bar = Bar(x * 2, (z._1 * 2, z._2 * 2))

    // Scala 2.12 :/
//    Foo(3, "pi", (3.14, 3.14))
//      .into[Bar]
//      .withConstructor(customConstructor)
//      .transform ==> Bar(6, (6.28, 6.28))
    Foo(3, "pi", (3.14, 3.14))
      .into[Bar]
      .withConstructor(customConstructor _)
      .transform ==> Bar(6, (6.28, 6.28))
    Foo(3, "pi", (3.14, 3.14))
      .into[Bar]
      .withConstructor({ (x: Int, z: (Double, Double)) =>
        customConstructor(x, z)
      })
      .transform ==> Bar(6, (6.28, 6.28))
    Foo(3, "pi", (3.14, 3.14))
      .into[Bar]
      .withConstructor({ (x: Int) => (z: (Double, Double)) =>
        customConstructor(x, z)
      })
      .transform ==> Bar(6, (6.28, 6.28))
  }
}
