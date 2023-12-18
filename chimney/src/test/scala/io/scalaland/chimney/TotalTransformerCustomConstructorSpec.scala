package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class TotalTransformerCustomConstructorSpec extends ChimneySpec {

  // TODO: test rejecting non-constructors

  test("""transformation from a "superset" of fields into a "subset" of fields without modifiers""") {
    import products.{Foo, Bar, BarParams}

    def nullaryConstructor(): Bar = Bar(0, (0.0, 0.0))

    Foo(3, "pi", (3.14, 3.14))
      .into[Bar]
      .withConstructor(nullaryConstructor _)
      .transform ==> Bar(0, (0.0, 0.0))
    Foo(3, "pi", (3.14, 3.14))
      .into[Bar]
      .withConstructor { () =>
        nullaryConstructor()
      }
      .transform ==> Bar(0, (0.0, 0.0))

    def uncurriedConstructor(x: Int, z: (Double, Double)): Bar = Bar(x * 2, (z._1 * 2, z._2 * 2))

    Foo(3, "pi", (3.14, 3.14))
      .into[Bar]
      .withConstructor(uncurriedConstructor _)
      .transform ==> Bar(6, (6.28, 6.28))
    Foo(3, "pi", (3.14, 3.14))
      .into[Bar]
      .withConstructor { (x: Int, z: (Double, Double)) =>
        uncurriedConstructor(x, z)
      }
      .transform ==> Bar(6, (6.28, 6.28))

    def curriedConstructor(x: Int)(z: (Double, Double)): Bar = Bar(x * 3, (z._1 * 3, z._2 * 3))

    Foo(3, "pi", (3.14, 3.14))
      .into[Bar]
      .withConstructor(curriedConstructor _)
      .transform ==> Bar(9, (9.42, 9.42))
    Foo(3, "pi", (3.14, 3.14))
      .into[Bar]
      .withConstructor { (x: Int) => (z: (Double, Double)) =>
        curriedConstructor(x)(z)
      }
      .transform ==> Bar(9, (9.42, 9.42))

    def typeParametricConstructor[A, B](x: A, z: (B, Double)): BarParams[A, B] = BarParams(x, (z._1, z._2 * 4))

    Foo(3, "pi", (3.14, 3.14))
      .into[BarParams[Int, Double]]
      .withConstructor(typeParametricConstructor[Int, Double] _)
      .transform ==> BarParams(3, (3.14, 12.56))
  }
}
