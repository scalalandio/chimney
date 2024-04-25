package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class PartialTransformerCustomConstructorSpec extends ChimneySpec {

  test("""not allow transformation when passed value is not a function/method""") {
    import products.{Foo, Bar}

    compileErrors("""Foo(3, "pi", (3.14, 3.14)).intoPartial[Bar].withConstructor(Bar(4, (5.0, 5.0))).transform""")
      .check(
        "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of ", // difference between Scala 2 and 3
        ", got io.scalaland.chimney.fixtures.products.Bar"
      )

    compileErrors(
      """Foo(3, "pi", (3.14, 3.14)).intoPartial[Bar].withConstructorPartial(partial.Result.fromValue(Bar(4, (5.0, 5.0)))).transform"""
    ).check(
      "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of ", // difference between Scala 2 and 3
      ", got io.scalaland.chimney.partial.Result[io.scalaland.chimney.fixtures.products.Bar]"
    )
  }

  test("""allow transformation from using Eta-expanded method or lambda""") {
    import products.{Foo, Bar, BarParams}

    val nullaryExpected = Bar(0, (0.0, 0.0))

    def nullaryConstructor(): Bar = Bar(0, (0.0, 0.0))

    val result = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructor(nullaryConstructor _)
      .transform
    result.asOption ==> Some(nullaryExpected)
    result.asEither ==> Right(nullaryExpected)
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructor { () =>
        nullaryConstructor()
      }
      .transform
    result2.asOption ==> Some(nullaryExpected)
    result2.asEither ==> Right(nullaryExpected)
    result2.asErrorPathMessageStrings ==> Iterable.empty

    def nullaryConstructorPartial(): partial.Result[Bar] = partial.Result.fromValue(Bar(0, (0.0, 0.0)))

    val result3 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructorPartial(nullaryConstructorPartial _)
      .transform
    result3.asOption ==> Some(nullaryExpected)
    result3.asEither ==> Right(nullaryExpected)
    result3.asErrorPathMessageStrings ==> Iterable.empty

    val result4 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructorPartial { () =>
        nullaryConstructorPartial()
      }
      .transform
    result4.asOption ==> Some(nullaryExpected)
    result4.asEither ==> Right(nullaryExpected)
    result4.asErrorPathMessageStrings ==> Iterable.empty

    val uncurriedExpected = Bar(6, (6.28, 6.28))

    def uncurriedConstructor(x: Int, z: (Double, Double)): Bar = Bar(x * 2, (z._1 * 2, z._2 * 2))

    val result5 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructor(uncurriedConstructor _)
      .transform
    result5.asOption ==> Some(uncurriedExpected)
    result5.asEither ==> Right(uncurriedExpected)
    result5.asErrorPathMessageStrings ==> Iterable.empty

    val result6 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructor { (x: Int, z: (Double, Double)) =>
        uncurriedConstructor(x, z)
      }
      .transform
    result6.asOption ==> Some(uncurriedExpected)
    result6.asEither ==> Right(uncurriedExpected)
    result6.asErrorPathMessageStrings ==> Iterable.empty

    def uncurriedConstructorPartial(x: Int, z: (Double, Double)): partial.Result[Bar] =
      partial.Result.fromValue(Bar(x * 2, (z._1 * 2, z._2 * 2)))

    val result7 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructorPartial(uncurriedConstructorPartial _)
      .transform
    result7.asOption ==> Some(uncurriedExpected)
    result7.asEither ==> Right(uncurriedExpected)
    result7.asErrorPathMessageStrings ==> Iterable.empty

    val result8 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructorPartial { (x: Int, z: (Double, Double)) =>
        uncurriedConstructorPartial(x, z)
      }
      .transform
    result8.asOption ==> Some(uncurriedExpected)
    result8.asEither ==> Right(uncurriedExpected)
    result8.asErrorPathMessageStrings ==> Iterable.empty

    val curriedExpected = Bar(9, (9.42, 9.42))

    def curriedConstructor(x: Int)(z: (Double, Double)): Bar = Bar(x * 3, (z._1 * 3, z._2 * 3))

    val result9 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructor(curriedConstructor _)
      .transform
    result9.asOption ==> Some(curriedExpected)
    result9.asEither ==> Right(curriedExpected)
    result9.asErrorPathMessageStrings ==> Iterable.empty

    val result10 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructor { (x: Int) => (z: (Double, Double)) =>
        curriedConstructor(x)(z)
      }
      .transform
    result10.asOption ==> Some(curriedExpected)
    result10.asEither ==> Right(curriedExpected)
    result10.asErrorPathMessageStrings ==> Iterable.empty

    def curriedConstructorPartial(x: Int)(z: (Double, Double)): partial.Result[Bar] =
      partial.Result.fromValue(Bar(x * 3, (z._1 * 3, z._2 * 3)))

    val result11 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructorPartial(curriedConstructorPartial _)
      .transform
    result11.asOption ==> Some(curriedExpected)
    result11.asEither ==> Right(curriedExpected)
    result11.asErrorPathMessageStrings ==> Iterable.empty

    val result12 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[Bar]
      .withConstructorPartial { (x: Int) => (z: (Double, Double)) =>
        curriedConstructorPartial(x)(z)
      }
      .transform
    result12.asOption ==> Some(curriedExpected)
    result12.asEither ==> Right(curriedExpected)
    result12.asErrorPathMessageStrings ==> Iterable.empty

    val typeParametricExpected = BarParams(3, (3.14, 12.56))

    def typeParametricConstructor[A, B](x: A, z: (B, Double)): BarParams[A, B] = BarParams(x, (z._1, z._2 * 4))

    val result13 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[BarParams[Int, Double]]
      .withConstructor(typeParametricConstructor[Int, Double] _)
      .transform
    result13.asOption ==> Some(typeParametricExpected)
    result13.asEither ==> Right(typeParametricExpected)
    result13.asErrorPathMessageStrings ==> Iterable.empty

    def typeParametricConstructorPartial[A, B](x: A, z: (B, Double)): partial.Result[BarParams[A, B]] =
      partial.Result.fromValue(BarParams(x, (z._1, z._2 * 4)))

    val result14 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[BarParams[Int, Double]]
      .withConstructorPartial(typeParametricConstructorPartial[Int, Double] _)
      .transform
    result14.asOption ==> Some(typeParametricExpected)
    result14.asEither ==> Right(typeParametricExpected)
    result14.asErrorPathMessageStrings ==> Iterable.empty

    def typeParametricConstructorEither[A, B](x: A, z: (B, Double)): Either[String, BarParams[A, B]] =
      Right(BarParams(x, (z._1, z._2 * 4)))

    val result15 = Foo(3, "pi", (3.14, 3.14))
      .intoPartial[BarParams[Int, Double]]
      .withConstructorEither(typeParametricConstructorEither[Int, Double] _)
      .transform
    result15.asOption ==> Some(typeParametricExpected)
    result15.asEither ==> Right(typeParametricExpected)
    result15.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("""allow defining transformers with overrides""") {
    import products.NonCaseDomain.*

    implicit val transformer: PartialTransformer[ClassSource, TraitSource] = PartialTransformer
      .define[ClassSource, TraitSource]
      .withConstructorPartial { (name: String, id: String) =>
        // swap
        partial.Result.fromValue[TraitSource](new TraitSourceImpl(name = id, id = name))
      }
      // another swap
      .withFieldRenamed(_.id, _.name)
      .withFieldRenamed(_.name, _.id)
      .buildTransformer

    val result = (new ClassSource("id", "name")).transformIntoPartial[TraitSource].asOption.get
    result.id ==> "id"
    result.name ==> "name"
  }
}
