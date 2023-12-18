package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class TotalTransformerCustomConstructorSpec extends ChimneySpec {

  test("""not allow transformation when passed value is not a function/method""") {
    import products.{Foo, Bar}

    compileErrorsFixed("""Foo(3, "pi", (3.14, 3.14)).into[Bar].withConstructor(Bar(4, (5.0, 5.0))).transform""").check(
      "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of ", // difference between Scala 2 and 3
      ", got io.scalaland.chimney.fixtures.products.Bar"
    )
  }

  test("""allow transformation from using Eta-expanded method or lambda""") {
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

  test("""allow defining transformers with overrides""") {
    import products.NonCaseDomain.*

    implicit val transformer: Transformer[ClassSource, TraitSource] = Transformer
      .define[ClassSource, TraitSource]
      .withConstructor { (name: String, id: String) =>
        // swap
        new TraitSourceImpl(name = id, id = name): TraitSource
      }
      // another swap
      .withFieldRenamed(_.id, _.name)
      .withFieldRenamed(_.name, _.id)
      .buildTransformer

    val result = (new ClassSource("id", "name")).transformInto[TraitSource]
    result.id ==> "id"
    result.name ==> "name"
  }
}
