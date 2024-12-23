package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class PartialTransformerCustomConstructorSpec extends ChimneySpec {

  group("setting .withConstructor(fn)") {

    test("""should not allow transformation when passed value is not a function/method""") {
      import products.{Foo, Bar}

      compileErrors("""Foo(3, "pi", (3.14, 3.14)).intoPartial[Bar].withConstructor(Bar(4, (5.0, 5.0))).transform""")
        .check(
          "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of ", // difference between Scala 2 and 3
          ", got io.scalaland.chimney.fixtures.products.Bar"
        )
    }

    test("""should allow transformation from using Eta-expanded method or lambda""") {
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

      val uncurriedExpected = Bar(6, (6.28, 6.28))

      def uncurriedConstructor(x: Int, z: (Double, Double)): Bar = Bar(x * 2, (z._1 * 2, z._2 * 2))

      val result3 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructor(uncurriedConstructor _)
        .transform
      result3.asOption ==> Some(uncurriedExpected)
      result3.asEither ==> Right(uncurriedExpected)
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructor { (x: Int, z: (Double, Double)) =>
          uncurriedConstructor(x, z)
        }
        .transform
      result4.asOption ==> Some(uncurriedExpected)
      result4.asEither ==> Right(uncurriedExpected)
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val curriedExpected = Bar(9, (9.42, 9.42))

      def curriedConstructor(x: Int)(z: (Double, Double)): Bar = Bar(x * 3, (z._1 * 3, z._2 * 3))

      val result5 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructor(curriedConstructor _)
        .transform
      result5.asOption ==> Some(curriedExpected)
      result5.asEither ==> Right(curriedExpected)
      result5.asErrorPathMessageStrings ==> Iterable.empty

      val result6 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructor { (x: Int) => (z: (Double, Double)) =>
          curriedConstructor(x)(z)
        }
        .transform
      result6.asOption ==> Some(curriedExpected)
      result6.asEither ==> Right(curriedExpected)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val typeParametricExpected = BarParams(3, (3.14, 12.56))

      def typeParametricConstructor[A, B](x: A, z: (B, Double)): BarParams[A, B] = BarParams(x, (z._1, z._2 * 4))

      val result7 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[BarParams[Int, Double]]
        .withConstructor(typeParametricConstructor[Int, Double] _)
        .transform
      result7.asOption ==> Some(typeParametricExpected)
      result7.asEither ==> Right(typeParametricExpected)
      result7.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("""should allow defining transformers with overrides""") {
      import products.NonCaseDomain.*

      implicit val transformer: PartialTransformer[ClassSource, TraitSource] = PartialTransformer
        .define[ClassSource, TraitSource]
        .withConstructor { (name: String, id: String) =>
          // swap
          new TraitSourceImpl(name = id, id = name): TraitSource
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

  group("setting .withConstructorTo(_.field)(fn)") {

    test("""should not allow transformation when passed value is not a function/method""") {
      import products.{Foo, Bar}, nestedpath.*

      compileErrors(
        """NestedProduct(Foo(3, "pi", (3.14, 3.14))).intoPartial[NestedProduct[Bar]].withConstructorTo(_.value)(Bar(4, (5.0, 5.0))).transform"""
      )
        .check(
          "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of ", // difference between Scala 2 and 3
          ", got io.scalaland.chimney.fixtures.products.Bar"
        )
    }

    test("""should allow transformation from using Eta-expanded method or lambda""") {
      import products.{Foo, Bar, BarParams}, nestedpath.*

      val nullaryExpected = NestedProduct(Bar(0, (0.0, 0.0)))

      def nullaryConstructor(): Bar = Bar(0, (0.0, 0.0))

      val result = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorTo(_.value)(nullaryConstructor _)
        .transform
      result.asOption ==> Some(nullaryExpected)
      result.asEither ==> Right(nullaryExpected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorTo(_.value) { () =>
          nullaryConstructor()
        }
        .transform
      result2.asOption ==> Some(nullaryExpected)
      result2.asEither ==> Right(nullaryExpected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val uncurriedExpected = NestedProduct(Bar(6, (6.28, 6.28)))

      def uncurriedConstructor(x: Int, z: (Double, Double)): Bar = Bar(x * 2, (z._1 * 2, z._2 * 2))

      val result3 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorTo(_.value)(uncurriedConstructor _)
        .transform
      result3.asOption ==> Some(uncurriedExpected)
      result3.asEither ==> Right(uncurriedExpected)
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorTo(_.value) { (x: Int, z: (Double, Double)) =>
          uncurriedConstructor(x, z)
        }
        .transform
      result4.asOption ==> Some(uncurriedExpected)
      result4.asEither ==> Right(uncurriedExpected)
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val curriedExpected = NestedProduct(Bar(9, (9.42, 9.42)))

      def curriedConstructor(x: Int)(z: (Double, Double)): Bar = Bar(x * 3, (z._1 * 3, z._2 * 3))

      val result5 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorTo(_.value)(curriedConstructor _)
        .transform
      result5.asOption ==> Some(curriedExpected)
      result5.asEither ==> Right(curriedExpected)
      result5.asErrorPathMessageStrings ==> Iterable.empty

      val result6 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorTo(_.value) { (x: Int) => (z: (Double, Double)) =>
          curriedConstructor(x)(z)
        }
        .transform
      result6.asOption ==> Some(curriedExpected)
      result6.asEither ==> Right(curriedExpected)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val typeParametricExpected = NestedProduct(BarParams(3, (3.14, 12.56)))

      def typeParametricConstructor[A, B](x: A, z: (B, Double)): BarParams[A, B] = BarParams(x, (z._1, z._2 * 4))

      val result7 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[BarParams[Int, Double]]]
        .withConstructorTo(_.value)(typeParametricConstructor[Int, Double] _)
        .transform
      result7.asOption ==> Some(typeParametricExpected)
      result7.asEither ==> Right(typeParametricExpected)
      result7.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("""should allow defining transformers with overrides""") {
      import products.NonCaseDomain.*, nestedpath.*

      implicit val transformer: PartialTransformer[NestedProduct[ClassSource], NestedProduct[TraitSource]] =
        PartialTransformer
          .define[NestedProduct[ClassSource], NestedProduct[TraitSource]]
          .withConstructorTo(_.value) { (name: String, id: String) =>
            // swap
            new TraitSourceImpl(name = id, id = name): TraitSource
          }
          // another swap
          .withFieldRenamed(_.value.id, _.value.name)
          .withFieldRenamed(_.value.name, _.value.id)
          .buildTransformer

      val result =
        NestedProduct(new ClassSource("id", "name")).transformIntoPartial[NestedProduct[TraitSource]].asOption.get
      result.value.id ==> "id"
      result.value.name ==> "name"
    }
  }

  group("setting .withConstructorPartial(fn)") {

    test("""should not allow transformation when passed value is not a function/method""") {
      import products.{Foo, Bar}

      compileErrors(
        """Foo(3, "pi", (3.14, 3.14)).intoPartial[Bar].withConstructorPartial(partial.Result.fromValue(Bar(4, (5.0, 5.0)))).transform"""
      ).check(
        "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of ", // difference between Scala 2 and 3
        ", got io.scalaland.chimney.partial.Result[io.scalaland.chimney.fixtures.products.Bar]"
      )
    }

    test("""should allow transformation from using Eta-expanded method or lambda""") {
      import products.{Foo, Bar, BarParams}

      val nullaryExpected = Bar(0, (0.0, 0.0))

      def nullaryConstructor(): partial.Result[Bar] = partial.Result.fromValue(Bar(0, (0.0, 0.0)))

      val result = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorPartial(nullaryConstructor _)
        .transform
      result.asOption ==> Some(nullaryExpected)
      result.asEither ==> Right(nullaryExpected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorPartial { () =>
          nullaryConstructor()
        }
        .transform
      result2.asOption ==> Some(nullaryExpected)
      result2.asEither ==> Right(nullaryExpected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val uncurriedExpected = Bar(6, (6.28, 6.28))

      def uncurriedConstructor(x: Int, z: (Double, Double)): partial.Result[Bar] =
        partial.Result.fromValue(Bar(x * 2, (z._1 * 2, z._2 * 2)))

      val result3 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorPartial(uncurriedConstructor _)
        .transform
      result3.asOption ==> Some(uncurriedExpected)
      result3.asEither ==> Right(uncurriedExpected)
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorPartial { (x: Int, z: (Double, Double)) =>
          uncurriedConstructor(x, z)
        }
        .transform
      result4.asOption ==> Some(uncurriedExpected)
      result4.asEither ==> Right(uncurriedExpected)
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val curriedExpected = Bar(9, (9.42, 9.42))

      def curriedConstructor(x: Int)(z: (Double, Double)): partial.Result[Bar] =
        partial.Result.fromValue(Bar(x * 3, (z._1 * 3, z._2 * 3)))

      val result5 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorPartial(curriedConstructor _)
        .transform
      result5.asOption ==> Some(curriedExpected)
      result5.asEither ==> Right(curriedExpected)
      result5.asErrorPathMessageStrings ==> Iterable.empty

      val result6 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorPartial { (x: Int) => (z: (Double, Double)) =>
          curriedConstructor(x)(z)
        }
        .transform
      result6.asOption ==> Some(curriedExpected)
      result6.asEither ==> Right(curriedExpected)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val typeParametricExpected = BarParams(3, (3.14, 12.56))

      def typeParametricConstructor[A, B](x: A, z: (B, Double)): partial.Result[BarParams[A, B]] =
        partial.Result.fromValue(BarParams(x, (z._1, z._2 * 4)))

      val result7 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[BarParams[Int, Double]]
        .withConstructorPartial(typeParametricConstructor[Int, Double] _)
        .transform
      result7.asOption ==> Some(typeParametricExpected)
      result7.asEither ==> Right(typeParametricExpected)
      result7.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("""should allow defining transformers with overrides""") {
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

  group("setting .withConstructorPartialTo(_.field)(fn)") {

    test("""should not allow transformation when passed value is not a function/method""") {
      import products.{Foo, Bar}, nestedpath.*

      compileErrors(
        """NestedProduct(Foo(3, "pi", (3.14, 3.14))).intoPartial[NestedProduct[Bar]].withConstructorPartialTo(_.value)(partial.Result.fromValue(Bar(4, (5.0, 5.0)))).transform"""
      ).check(
        "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of ", // difference between Scala 2 and 3
        ", got io.scalaland.chimney.partial.Result[io.scalaland.chimney.fixtures.products.Bar]"
      )
    }

    test("""should allow transformation from using Eta-expanded method or lambda""") {
      import products.{Foo, Bar, BarParams}, nestedpath.*

      val nullaryExpected = NestedProduct(Bar(0, (0.0, 0.0)))

      def nullaryConstructor(): partial.Result[Bar] = partial.Result.fromValue(Bar(0, (0.0, 0.0)))

      val result = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorPartialTo(_.value)(nullaryConstructor _)
        .transform
      result.asOption ==> Some(nullaryExpected)
      result.asEither ==> Right(nullaryExpected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorPartialTo(_.value) { () =>
          nullaryConstructor()
        }
        .transform
      result2.asOption ==> Some(nullaryExpected)
      result2.asEither ==> Right(nullaryExpected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val uncurriedExpected = NestedProduct(Bar(6, (6.28, 6.28)))

      def uncurriedConstructor(x: Int, z: (Double, Double)): partial.Result[Bar] =
        partial.Result.fromValue(Bar(x * 2, (z._1 * 2, z._2 * 2)))

      val result3 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorPartialTo(_.value)(uncurriedConstructor _)
        .transform
      result3.asOption ==> Some(uncurriedExpected)
      result3.asEither ==> Right(uncurriedExpected)
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorPartialTo(_.value) { (x: Int, z: (Double, Double)) =>
          uncurriedConstructor(x, z)
        }
        .transform
      result4.asOption ==> Some(uncurriedExpected)
      result4.asEither ==> Right(uncurriedExpected)
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val curriedExpected = NestedProduct(Bar(9, (9.42, 9.42)))

      def curriedConstructor(x: Int)(z: (Double, Double)): partial.Result[Bar] =
        partial.Result.fromValue(Bar(x * 3, (z._1 * 3, z._2 * 3)))

      val result5 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorPartialTo(_.value)(curriedConstructor _)
        .transform
      result5.asOption ==> Some(curriedExpected)
      result5.asEither ==> Right(curriedExpected)
      result5.asErrorPathMessageStrings ==> Iterable.empty

      val result6 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorPartialTo(_.value) { (x: Int) => (z: (Double, Double)) =>
          curriedConstructor(x)(z)
        }
        .transform
      result6.asOption ==> Some(curriedExpected)
      result6.asEither ==> Right(curriedExpected)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val typeParametricExpected = NestedProduct(BarParams(3, (3.14, 12.56)))

      def typeParametricConstructor[A, B](x: A, z: (B, Double)): partial.Result[BarParams[A, B]] =
        partial.Result.fromValue(BarParams(x, (z._1, z._2 * 4)))

      val result7 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[BarParams[Int, Double]]]
        .withConstructorPartialTo(_.value)(typeParametricConstructor[Int, Double] _)
        .transform
      result7.asOption ==> Some(typeParametricExpected)
      result7.asEither ==> Right(typeParametricExpected)
      result7.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("""should allow defining transformers with overrides""") {
      import products.NonCaseDomain.*, nestedpath.*

      implicit val transformer: PartialTransformer[NestedProduct[ClassSource], NestedProduct[TraitSource]] =
        PartialTransformer
          .define[NestedProduct[ClassSource], NestedProduct[TraitSource]]
          .withConstructorPartialTo(_.value) { (name: String, id: String) =>
            // swap
            partial.Result.fromValue[TraitSource](new TraitSourceImpl(name = id, id = name))
          }
          // another swap
          .withFieldRenamed(_.value.id, _.value.name)
          .withFieldRenamed(_.value.name, _.value.id)
          .buildTransformer

      val result =
        NestedProduct(new ClassSource("id", "name")).transformIntoPartial[NestedProduct[TraitSource]].asOption.get
      result.value.id ==> "id"
      result.value.name ==> "name"
    }
  }

  group("setting .withConstructorEither(fn)") {

    test("""should not allow transformation when passed value is not a function/method""") {
      import products.{Foo, Bar}

      compileErrors(
        """Foo(3, "pi", (3.14, 3.14)).intoPartial[Bar].withConstructorEither(Right(Bar(4, (5.0, 5.0)))).transform"""
      ).check(
        "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of " // difference between Scala 2 and 3
      )
    }

    test("""should allow transformation from using Eta-expanded method or lambda""") {
      import products.{Foo, Bar, BarParams}

      val nullaryExpected = Bar(0, (0.0, 0.0))

      def nullaryConstructor(): Either[String, Bar] = Right(Bar(0, (0.0, 0.0)))

      val result = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorEither(nullaryConstructor _)
        .transform
      result.asOption ==> Some(nullaryExpected)
      result.asEither ==> Right(nullaryExpected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorEither { () =>
          nullaryConstructor()
        }
        .transform
      result2.asOption ==> Some(nullaryExpected)
      result2.asEither ==> Right(nullaryExpected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val uncurriedExpected = Bar(6, (6.28, 6.28))

      def uncurriedConstructor(x: Int, z: (Double, Double)): Either[String, Bar] =
        Right(Bar(x * 2, (z._1 * 2, z._2 * 2)))

      val result3 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorEither(uncurriedConstructor _)
        .transform
      result3.asOption ==> Some(uncurriedExpected)
      result3.asEither ==> Right(uncurriedExpected)
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorEither { (x: Int, z: (Double, Double)) =>
          uncurriedConstructor(x, z)
        }
        .transform
      result4.asOption ==> Some(uncurriedExpected)
      result4.asEither ==> Right(uncurriedExpected)
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val curriedExpected = Bar(9, (9.42, 9.42))

      def curriedConstructor(x: Int)(z: (Double, Double)): Either[String, Bar] =
        Right(Bar(x * 3, (z._1 * 3, z._2 * 3)))

      val result5 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorEither(curriedConstructor _)
        .transform
      result5.asOption ==> Some(curriedExpected)
      result5.asEither ==> Right(curriedExpected)
      result5.asErrorPathMessageStrings ==> Iterable.empty

      val result6 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[Bar]
        .withConstructorEither { (x: Int) => (z: (Double, Double)) =>
          curriedConstructor(x)(z)
        }
        .transform
      result6.asOption ==> Some(curriedExpected)
      result6.asEither ==> Right(curriedExpected)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val typeParametricExpected = BarParams(3, (3.14, 12.56))

      def typeParametricConstructor[A, B](x: A, z: (B, Double)): Either[String, BarParams[A, B]] =
        Right(BarParams(x, (z._1, z._2 * 4)))

      val result7 = Foo(3, "pi", (3.14, 3.14))
        .intoPartial[BarParams[Int, Double]]
        .withConstructorEither(typeParametricConstructor[Int, Double] _)
        .transform
      result7.asOption ==> Some(typeParametricExpected)
      result7.asEither ==> Right(typeParametricExpected)
      result7.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("""should allow defining transformers with overrides""") {
      import products.NonCaseDomain.*

      implicit val transformer: PartialTransformer[ClassSource, TraitSource] = PartialTransformer
        .define[ClassSource, TraitSource]
        .withConstructorEither { (name: String, id: String) =>
          // swap
          Right[String, TraitSource](new TraitSourceImpl(name = id, id = name)): Either[String, TraitSource]
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

  group("setting .withConstructorEitherTo(_.field)(fn)") {

    test("""should not allow transformation when passed value is not a function/method""") {
      import products.{Foo, Bar}, nestedpath.*

      compileErrors(
        """NestedProduct(Foo(3, "pi", (3.14, 3.14))).intoPartial[NestedProduct[Bar]].withConstructorEitherTo(_.value)(Right(Bar(4, (5.0, 5.0)))).transform"""
      ).check(
        "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of " // difference between Scala 2 and 3
      )
    }

    test("""should allow transformation from using Eta-expanded method or lambda""") {
      import products.{Foo, Bar, BarParams}, nestedpath.*

      val nullaryExpected = NestedProduct(Bar(0, (0.0, 0.0)))

      def nullaryConstructor(): Either[String, Bar] = Right(Bar(0, (0.0, 0.0)))

      val result = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorEitherTo(_.value)(nullaryConstructor _)
        .transform
      result.asOption ==> Some(nullaryExpected)
      result.asEither ==> Right(nullaryExpected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorEitherTo(_.value) { () =>
          nullaryConstructor()
        }
        .transform
      result2.asOption ==> Some(nullaryExpected)
      result2.asEither ==> Right(nullaryExpected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val uncurriedExpected = NestedProduct(Bar(6, (6.28, 6.28)))

      def uncurriedConstructor(x: Int, z: (Double, Double)): Either[String, Bar] =
        Right(Bar(x * 2, (z._1 * 2, z._2 * 2)))

      val result3 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorEitherTo(_.value)(uncurriedConstructor _)
        .transform
      result3.asOption ==> Some(uncurriedExpected)
      result3.asEither ==> Right(uncurriedExpected)
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorEitherTo(_.value) { (x: Int, z: (Double, Double)) =>
          uncurriedConstructor(x, z)
        }
        .transform
      result4.asOption ==> Some(uncurriedExpected)
      result4.asEither ==> Right(uncurriedExpected)
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val curriedExpected = NestedProduct(Bar(9, (9.42, 9.42)))

      def curriedConstructor(x: Int)(z: (Double, Double)): Either[String, Bar] =
        Right(Bar(x * 3, (z._1 * 3, z._2 * 3)))

      val result5 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorEitherTo(_.value)(curriedConstructor _)
        .transform
      result5.asOption ==> Some(curriedExpected)
      result5.asEither ==> Right(curriedExpected)
      result5.asErrorPathMessageStrings ==> Iterable.empty

      val result6 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[Bar]]
        .withConstructorEitherTo(_.value) { (x: Int) => (z: (Double, Double)) =>
          curriedConstructor(x)(z)
        }
        .transform
      result6.asOption ==> Some(curriedExpected)
      result6.asEither ==> Right(curriedExpected)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val typeParametricExpected = NestedProduct(BarParams(3, (3.14, 12.56)))

      def typeParametricConstructor[A, B](x: A, z: (B, Double)): Either[String, BarParams[A, B]] =
        Right(BarParams(x, (z._1, z._2 * 4)))

      val result7 = NestedProduct(Foo(3, "pi", (3.14, 3.14)))
        .intoPartial[NestedProduct[BarParams[Int, Double]]]
        .withConstructorEitherTo(_.value)(typeParametricConstructor[Int, Double] _)
        .transform
      result7.asOption ==> Some(typeParametricExpected)
      result7.asEither ==> Right(typeParametricExpected)
      result7.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("""should allow defining transformers with overrides""") {
      import products.NonCaseDomain.*, nestedpath.*

      implicit val transformer: PartialTransformer[NestedProduct[ClassSource], NestedProduct[TraitSource]] =
        PartialTransformer
          .define[NestedProduct[ClassSource], NestedProduct[TraitSource]]
          .withConstructorEitherTo(_.value) { (name: String, id: String) =>
            // swap
            Right[String, TraitSource](new TraitSourceImpl(name = id, id = name)): Either[String, TraitSource]
          }
          // another swap
          .withFieldRenamed(_.value.id, _.value.name)
          .withFieldRenamed(_.value.name, _.value.id)
          .buildTransformer

      val result =
        NestedProduct(new ClassSource("id", "name")).transformIntoPartial[NestedProduct[TraitSource]].asOption.get
      result.value.id ==> "id"
      result.value.name ==> "name"
    }
  }
}
