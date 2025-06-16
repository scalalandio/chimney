package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.annotation.nowarn

@nowarn("msg=unused import")
class TotalTransformerCustomConstructorSpec extends ChimneySpec {

  group("setting .withConstructor(fn)") {

    test("""should not allow transformation when passed value is not a function/method""") {
      import products.{Foo, Bar}

      compileErrors("""Foo(3, "pi", (3.14, 3.14)).into[Bar].withConstructor(Bar(4, (5.0, 5.0))).transform""").check(
        "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of ", // difference between Scala 2 and 3
        ", got io.scalaland.chimney.fixtures.products.Bar"
      )
    }

    test("""should allow transformation from using Eta-expanded method or lambda""") {
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

    test("""should allow defining transformers with overrides""") {
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

    test("should work with semiautomatic derivation") {
      import products.{Foo, Bar}

      def uncurriedConstructor(x: Int, z: (Double, Double)): Bar = Bar(x * 2, (z._1 * 2, z._2 * 2))

      Transformer
        .define[Foo, Bar]
        .withConstructor(uncurriedConstructor _)
        .buildTransformer
        .transform(Foo(3, "pi", (3.14, 3.14))) ==> Bar(6, (6.28, 6.28))
      Transformer
        .define[Foo, Bar]
        .withConstructor((x: Int, z: (Double, Double)) => uncurriedConstructor(x, z))
        .buildTransformer
        .transform(Foo(3, "pi", (3.14, 3.14))) ==> Bar(6, (6.28, 6.28))
    }
  }

  group("setting .withConstructorTo(_.field)(fn)") {

    test("""should not allow transformation when passed value is not a function/method""") {
      import products.{Foo, Bar}, nestedpath.*

      compileErrors(
        """NestedProduct(Foo(3, "pi", (3.14, 3.14))).into[NestedProduct[Bar]].withConstructorTo(_.value)(Bar(4, (5.0, 5.0))).transform"""
      ).check(
        "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of ", // difference between Scala 2 and 3
        ", got io.scalaland.chimney.fixtures.products.Bar"
      )
    }

    test("""should allow transformation from using Eta-expanded method or lambda""") {
      import products.{Foo, Bar, BarParams}, nestedpath.*

      def nullaryConstructor(): Bar = Bar(0, (0.0, 0.0))

      NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .into[NestedProduct[Bar]]
        .withConstructorTo(_.value)(nullaryConstructor _)
        .transform ==> NestedProduct(Bar(0, (0.0, 0.0)))
      NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .into[NestedProduct[Bar]]
        .withConstructorTo(_.value) { () =>
          nullaryConstructor()
        }
        .transform ==> NestedProduct(Bar(0, (0.0, 0.0)))

      def uncurriedConstructor(x: Int, z: (Double, Double)): Bar = Bar(x * 2, (z._1 * 2, z._2 * 2))

      NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .into[NestedProduct[Bar]]
        .withConstructorTo(_.value)(uncurriedConstructor _)
        .transform ==> NestedProduct(Bar(6, (6.28, 6.28)))
      NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .into[NestedProduct[Bar]]
        .withConstructorTo(_.value) { (x: Int, z: (Double, Double)) =>
          uncurriedConstructor(x, z)
        }
        .transform ==> NestedProduct(Bar(6, (6.28, 6.28)))

      def curriedConstructor(x: Int)(z: (Double, Double)): Bar = Bar(x * 3, (z._1 * 3, z._2 * 3))

      NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .into[NestedProduct[Bar]]
        .withConstructorTo(_.value)(curriedConstructor _)
        .transform ==> NestedProduct(Bar(9, (9.42, 9.42)))
      NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .into[NestedProduct[Bar]]
        .withConstructorTo(_.value) { (x: Int) => (z: (Double, Double)) =>
          curriedConstructor(x)(z)
        }
        .transform ==> NestedProduct(Bar(9, (9.42, 9.42)))

      def typeParametricConstructor[A, B](x: A, z: (B, Double)): BarParams[A, B] = BarParams(x, (z._1, z._2 * 4))

      NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .into[NestedProduct[BarParams[Int, Double]]]
        .withConstructorTo(_.value)(typeParametricConstructor[Int, Double] _)
        .transform ==> NestedProduct(BarParams(3, (3.14, 12.56)))
    }

    test("""should allow defining transformers with overrides""") {
      import products.NonCaseDomain.*, nestedpath.*

      implicit val transformer: Transformer[NestedProduct[ClassSource], NestedProduct[TraitSource]] = Transformer
        .define[NestedProduct[ClassSource], NestedProduct[TraitSource]]
        .withConstructorTo(_.value) { (name: String, id: String) =>
          // swap
          new TraitSourceImpl(name = id, id = name): TraitSource
        }
        // another swap
        .withFieldRenamed(_.value.id, _.value.name)
        .withFieldRenamed(_.value.name, _.value.id)
        .buildTransformer

      val result = NestedProduct(new ClassSource("id", "name")).transformInto[NestedProduct[TraitSource]]
      result.value.id ==> "id"
      result.value.name ==> "name"
    }

    test("should work with semiautomatic derivation") {
      import products.{Foo, Bar}, nestedpath.*

      def uncurriedConstructor(x: Int, z: (Double, Double)): Bar = Bar(x * 2, (z._1 * 2, z._2 * 2))

      Transformer
        .define[NestedProduct[Foo], NestedProduct[Bar]]
        .withConstructorTo(_.value)(uncurriedConstructor _)
        .buildTransformer
        .transform(NestedProduct(Foo(3, "pi", (3.14, 3.14)))) ==> NestedProduct(Bar(6, (6.28, 6.28)))
      Transformer
        .define[NestedProduct[Foo], NestedProduct[Bar]]
        .withConstructorTo(_.value)((x: Int, z: (Double, Double)) => uncurriedConstructor(x, z))
        .buildTransformer
        .transform(NestedProduct(Foo(3, "pi", (3.14, 3.14)))) ==> NestedProduct(Bar(6, (6.28, 6.28)))
    }
  }

  test("""allow transformation from using Eta-expanded method or lambda""") {
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
