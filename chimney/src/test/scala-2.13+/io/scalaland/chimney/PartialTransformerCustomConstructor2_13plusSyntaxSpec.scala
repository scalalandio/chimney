package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class PartialTransformerCustomConstructor2_13plusSyntaxSpec extends ChimneySpec {

  // 2.13+ doesn't require explicit Eta-expansion (no need to "method _")
  test("""allow transformation from using Eta-expanded method or lambda""") {
    import products.{Foo, Bar, BarParams}

    val uncurriedExpected = Bar(6, (6.28, 6.28))

    def uncurriedConstructor(x: Int, z: (Double, Double)): Bar = Bar(x * 2, (z._1 * 2, z._2 * 2))

    val result = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructor(uncurriedConstructor)
      .transform
    result.asOption ==> Some(uncurriedExpected)
    result.asEither ==> Right(uncurriedExpected)
    result.asErrorPathMessageStrings ==> Iterable.empty

    def uncurriedConstructorPartial(x: Int, z: (Double, Double)): partial.Result[Bar] =
      partial.Result.fromValue(Bar(x * 2, (z._1 * 2, z._2 * 2)))

    val result2 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructorPartial(uncurriedConstructorPartial)
      .transform
    result2.asOption ==> Some(uncurriedExpected)
    result2.asEither ==> Right(uncurriedExpected)
    result2.asErrorPathMessageStrings ==> Iterable.empty

    val curriedExpected = Bar(9, (9.42, 9.42))

    def curriedConstructor(x: Int)(z: (Double, Double)): Bar = Bar(x * 3, (z._1 * 3, z._2 * 3))

    val result3 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructor(curriedConstructor)
      .transform
    result3.asOption ==> Some(curriedExpected)
    result3.asEither ==> Right(curriedExpected)
    result3.asErrorPathMessageStrings ==> Iterable.empty

    def curriedConstructorPartial(x: Int)(z: (Double, Double)): partial.Result[Bar] =
      partial.Result.fromValue(Bar(x * 3, (z._1 * 3, z._2 * 3)))

    val result4 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructorPartial(curriedConstructorPartial)
      .transform
    result4.asOption ==> Some(curriedExpected)
    result4.asEither ==> Right(curriedExpected)
    result4.asErrorPathMessageStrings ==> Iterable.empty

    val typeParametricExpected = BarParams(3, (3.14, 12.56))

    def typeParametricConstructor[A, B](x: A, z: (B, Double)): BarParams[A, B] = BarParams(x, (z._1, z._2 * 4))

    val result5 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[BarParams[Int, Double]]
      .withConstructor(typeParametricConstructor[Int, Double])
      .transform
    result5.asOption ==> Some(typeParametricExpected)
    result5.asEither ==> Right(typeParametricExpected)
    result5.asErrorPathMessageStrings ==> Iterable.empty

    def typeParametricConstructorPartial[A, B](x: A, z: (B, Double)): partial.Result[BarParams[A, B]] =
      partial.Result.fromValue(BarParams(x, (z._1, z._2 * 4)))

    val result6 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[BarParams[Int, Double]]
      .withConstructorPartial(typeParametricConstructorPartial[Int, Double])
      .transform
    result6.asOption ==> Some(typeParametricExpected)
    result6.asEither ==> Right(typeParametricExpected)
    result6.asErrorPathMessageStrings ==> Iterable.empty
  }
}
