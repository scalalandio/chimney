package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class TotalTransformerCustomConstructor2_13plusSyntaxSpec extends ChimneySpec {

  // 2.13+ doesn't require explicit Eta-expansion (no need to "method _")
  test("""transformation from a "superset" of fields into a "subset" of fields without modifiers""") {
    import products.{Foo, Bar, BarParams}

    def uncurriedConstructor(x: Int, z: (Double, Double)): Bar = Bar(x * 2, (z._1 * 2, z._2 * 2))

    Foo(3, "pi", (3.14, 3.14))
      .into[Bar]
      .withConstructor(uncurriedConstructor)
      .transform ==> Bar(6, (6.28, 6.28))

    def curriedConstructor(x: Int)(z: (Double, Double)): Bar = Bar(x * 3, (z._1 * 3, z._2 * 3))

    Foo(3, "pi", (3.14, 3.14))
      .into[Bar]
      .withConstructor(curriedConstructor)
      .transform ==> Bar(9, (9.42, 9.42))

    def typeParametricConstructor[A, B](x: A, z: (B, Double)): BarParams[A, B] = BarParams(x, (z._1, z._2 * 4))

    Foo(3, "pi", (3.14, 3.14))
      .into[BarParams[Int, Double]]
      .withConstructor(typeParametricConstructor[Int, Double])
      .transform ==> BarParams(3, (3.14, 12.56))
  }
}
