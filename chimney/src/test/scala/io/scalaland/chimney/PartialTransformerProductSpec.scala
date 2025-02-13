package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*
import io.scalaland.chimney.partial.syntax.*
import io.scalaland.chimney.utils.OptionUtils.*

import scala.annotation.unused
import scala.collection.immutable.ListMap

class PartialTransformerProductSpec extends ChimneySpec {

  test("transform case classes with the same fields' number, names and types without modifiers") {
    import trip.*

    val expected = User("John", 10, 140)

    val result = Person("John", 10, 140).intoPartial[User].transform
    result.asOption ==> Some(expected)
    result.asEither ==> Right(expected)
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = Person("John", 10, 140).transformIntoPartial[User]
    result2.asOption ==> Some(expected)
    result2.asEither ==> Right(expected)
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test(
    """not allow transformation from a "subset" of fields into a "superset" of fields when missing values are not provided"""
  ) {
    import products.{Foo, Bar}

    compileErrors("Bar(3, (3.14, 3.14)).intoPartial[Foo].transform").check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Bar to io.scalaland.chimney.fixtures.products.Foo",
      "io.scalaland.chimney.fixtures.products.Foo",
      "  y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Bar",
      "Consult https://chimney.readthedocs.io for usage examples."
    )

    compileErrors("Bar(3, (3.14, 3.14)).transformIntoPartial[Foo]").check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Bar to io.scalaland.chimney.fixtures.products.Foo",
      "io.scalaland.chimney.fixtures.products.Foo",
      "  y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Bar",
      "Consult https://chimney.readthedocs.io for usage examples."
    )
  }

  test("""transformation from a "superset" of fields into a "subset" of fields without modifiers""") {
    import products.{Foo, Bar}

    val expected = Bar(3, (3.14, 3.14))

    val result = Foo(3, "pi", (3.14, 3.14)).intoPartial[Bar].transform
    result.asOption ==> Some(expected)
    result.asEither ==> Right(expected)
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = Foo(3, "pi", (3.14, 3.14)).transformIntoPartial[Bar]
    result2.asOption ==> Some(expected)
    result2.asEither ==> Right(expected)
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("""transform from a subtype to a non-abstract supertype without modifiers""") {
    val result = CaseBar(100).transformIntoPartial[BaseFoo]

    result.asOption.map(_.x) ==> Some(100)
    result.asEither.map(_.x) ==> Right(100)
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = CaseBar(100).intoPartial[BaseFoo].transform

    result2.asOption.map(_.x) ==> Some(100)
    result2.asEither.map(_.x) ==> Right(100)
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("""transform from a subtype to a non-abstract supertype with modifiers""") {
    val result = CaseBar(100).intoPartial[BaseFoo].withFieldConst(_.x, 200).transform

    result.asOption.map(_.x) ==> Some(200)
    result.asEither.map(_.x) ==> Right(200)
    result.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("""transformation from a "superset" of fields into a "subset" of vars with .enableBeanSetters flag""") {
    import products.{Foo, BarVars}

    val result = Foo(3, "pi", (3.14, 3.14)).intoPartial[BarVars].enableBeanSetters.transform
    result.asOption ==> Some(BarVars(3, (3.14, 3.14)))
    result.asEither ==> Right(BarVars(3, (3.14, 3.14)))
    result.asErrorPathMessageStrings ==> Iterable.empty
    locally {
      implicit val cfg = TransformerConfiguration.default.enableBeanSetters
      val result2 = Foo(3, "pi", (3.14, 3.14)).transformIntoPartial[BarVars]
      result2.asOption ==> Some(BarVars(3, (3.14, 3.14)))
      result2.asEither ==> Right(BarVars(3, (3.14, 3.14)))
      result2.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("setting .withFieldConst(_.field, value)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      compileErrors("""Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.y + "abc", "pi").transform""").check(
        "The path expression has to be a single chain of calls on the original input, got operation other than value extraction:"
      )

      compileErrors(
        """
        val haveY = HaveY("")
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(cc => haveY.y, "pi").transform
        """
      ).check("The path expression has to be a single chain of calls on the original input, got external identifier:")

      compileErrors("""Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.matching, "pi").transform""")
        .arePresent()
      compileErrors("""Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.matchingSome, "pi").transform""")
        .arePresent()
      compileErrors("""Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.matchingLeft, "pi").transform""")
        .arePresent()
      compileErrors("""Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.matchingRight, "pi").transform""")
        .arePresent()
      compileErrors("""Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.everyItem, "pi").transform""")
        .arePresent()
      compileErrors("""Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.everyMapKey, "pi").transform""")
        .arePresent()
      compileErrors("""Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.everyMapValue, "pi").transform""")
        .arePresent()
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}, nestedpath.*

      implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

      val expected = Foo(3, "pi", (3.14, 3.14))

      val result = Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.y, "pi").transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(cc => cc.y, "pi").transform
      result2.asOption ==> Some(expected)
      result2.asEither ==> Right(expected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val result3 = NestedProduct(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedValueClass[Foo]]
        .withFieldConst(_.value.y, "pi")
        .transform
      result3.asOption ==> Some(NestedValueClass(expected))
      result3.asEither ==> Right(NestedValueClass(expected))
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = NestedValueClass(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedJavaBean[Foo]]
        .withFieldConst(cc => cc.getValue.y, "pi")
        .transform
      result4.asOption ==> Some(NestedJavaBean(expected))
      result4.asEither ==> Right(NestedJavaBean(expected))
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val result5 = NestedJavaBean(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedProduct[Foo]]
        .withFieldConst(cc => cc.value.y, "pi")
        .transform
      result5.asOption ==> Some(NestedProduct(expected))
      result5.asEither ==> Right(NestedProduct(expected))
      result5.asErrorPathMessageStrings ==> Iterable.empty

      import trip.*

      val expected2 = User("John", 20, 140)

      val result6 = Person("John", 10, 140).intoPartial[User].withFieldConst(_.age, 20).transform
      result6.asOption ==> Some(expected2)
      result6.asEither ==> Right(expected2)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val result7 = NestedProduct(Person("John", 10, 140))
        .intoPartial[NestedValueClass[User]]
        .withFieldConst(_.value.age, 20)
        .transform
      result7.asOption ==> Some(NestedValueClass(expected2))
      result7.asEither ==> Right(NestedValueClass(expected2))
      result7.asErrorPathMessageStrings ==> Iterable.empty

      val result8 = NestedValueClass(Person("John", 10, 140))
        .intoPartial[NestedJavaBean[User]]
        .withFieldConst(_.getValue.age, 20)
        .transform
      result8.asOption ==> Some(NestedJavaBean(expected2))
      result8.asEither ==> Right(NestedJavaBean(expected2))
      result8.asErrorPathMessageStrings ==> Iterable.empty

      val result9 = NestedJavaBean(Person("John", 10, 140))
        .intoPartial[NestedProduct[User]]
        .withFieldConst(_.value.age, 20)
        .transform
      result9.asOption ==> Some(NestedProduct(expected2))
      result9.asEither ==> Right(NestedProduct(expected2))
      result9.asErrorPathMessageStrings ==> Iterable.empty

      val expected3 = NestedComplex(
        User("John", 10, 140),
        Some(User("John", 15, 140)),
        Right(User("John", 30, 140)),
        List(User("John", 40, 140)),
        ListMap(User("John", 50, 140) -> User("John", 60, 140))
      )

      val result10 = NestedComplex(
        Person("John", 10, 140),
        Some(Person("John", 10, 140)),
        Right(Person("John", 10, 140)),
        List(Person("John", 10, 140)),
        ListMap(Person("John", 10, 140) -> Person("John", 10, 140))
      ).intoPartial[NestedComplex[User]]
        .withFieldConst(_.option.matchingSome.age, 15)
        .withFieldConst(_.either.matchingLeft.age, 20)
        .withFieldConst(_.either.matchingRight.age, 30)
        .withFieldConst(_.collection.everyItem.age, 40)
        .withFieldConst(_.map.everyMapKey.age, 50)
        .withFieldConst(_.map.everyMapValue.age, 60)
        .transform
      result10.asOption ==> Some(expected3)
      result10.asEither ==> Right(expected3)
      result10.asErrorPathMessageStrings ==> Iterable.empty

      val expected4 = Vector(NestedADT.Foo(User("John", 20, 140)), NestedADT.Bar(User("John", 30, 140)))

      val result11 =
        List[NestedADT[Person]](NestedADT.Foo(Person("John", 10, 140)), NestedADT.Bar(Person("John", 10, 140)))
          .intoPartial[Vector[NestedADT[User]]]
          .withFieldConst(_.everyItem.matching[NestedADT.Foo[User]].foo.age, 20)
          .withFieldConst(_.everyItem.matching[NestedADT.Bar[User]].bar.age, 30)
          .transform
      result11.asOption ==> Some(expected4)
      result11.asEither ==> Right(expected4)
      result11.asErrorPathMessageStrings ==> Iterable.empty

      import fixtures.products.Renames

      val result12 = (NestedADT.Foo(Renames.User(1, "Kuba", Some(28))): NestedADT[Renames.User])
        .intoPartial[NestedADT[Renames.UserPLStd]]
        .withFieldRenamed(
          _.matching[NestedADT.Foo[Renames.User]].foo.name,
          _.matching[NestedADT.Foo[Renames.UserPLStd]].foo.imie
        )
        .withFieldRenamed(
          _.matching[NestedADT.Foo[Renames.User]].foo.age,
          _.matching[NestedADT.Foo[Renames.UserPLStd]].foo.wiek
        )
        .withFieldRenamed(
          _.matching[NestedADT.Bar[Renames.User]].bar.name,
          _.matching[NestedADT.Bar[Renames.UserPLStd]].bar.imie
        )
        .withFieldRenamed(
          _.matching[NestedADT.Bar[Renames.User]].bar.age,
          _.matching[NestedADT.Bar[Renames.UserPLStd]].bar.wiek
        )
        .transform
      result12.asOption ==> Some(NestedADT.Foo(Renames.UserPLStd(1, "Kuba", Some(28))))
      result12.asEither ==> Right(NestedADT.Foo(Renames.UserPLStd(1, "Kuba", Some(28))))
      result12.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should work with semiautomatic derivation") {
      import products.{Foo, Bar}

      val expected = Foo(3, "pi", (3.14, 3.14))

      val result =
        PartialTransformer.define[Bar, Foo].withFieldConst(_.y, "pi").buildTransformer.transform(Bar(3, (3.14, 3.14)))
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("setting .withFieldConstPartial(_.field, result)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      compileErrors(
        """
        Bar(3, (3.14, 3.14))
          .intoPartial[Foo]
          .withFieldConstPartial(_.y + "abc", partial.Result.fromValue("pi"))
          .transform
        """
      ).check(
        "The path expression has to be a single chain of calls on the original input, got operation other than value extraction:"
      )

      compileErrors(
        """
        val haveY = HaveY("")
        Bar(3, (3.14, 3.14))
          .intoPartial[Foo]
          .withFieldConstPartial(cc => haveY.y, partial.Result.fromValue("pi"))
          .transform
        """
      ).check("The path expression has to be a single chain of calls on the original input, got external identifier:")

      compileErrors("""Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConstPartial(_.matching, "pi").transform""")
        .arePresent()
      compileErrors(
        """Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConstPartial(_.matchingSome, "pi").transform"""
      ).arePresent()
      compileErrors(
        """Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConstPartial(_.matchingLeft, "pi").transform"""
      ).arePresent()
      compileErrors(
        """Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConstPartial(_.matchingRight, "pi").transform"""
      ).arePresent()
      compileErrors("""Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConstPartial(_.everyItem, "pi").transform""")
        .arePresent()
      compileErrors(
        """Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConstPartial(_.everyMapKey, "pi").transform"""
      ).arePresent()
      compileErrors(
        """Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConstPartial(_.everyMapValue, "pi").transform"""
      ).arePresent()
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}, nestedpath.*

      implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

      val expected = Foo(3, "pi", (3.14, 3.14))

      val result = Bar(3, (3.14, 3.14))
        .intoPartial[Foo]
        .withFieldConstPartial(_.y, partial.Result.fromValue("pi"))
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Bar(3, (3.14, 3.14))
        .intoPartial[Foo]
        .withFieldConstPartial(cc => cc.y, partial.Result.fromValue("pi"))
        .transform
      result2.asOption ==> Some(expected)
      result2.asEither ==> Right(expected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val result3 = NestedProduct(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedValueClass[Foo]]
        .withFieldConstPartial(_.value.y, partial.Result.fromValue("pi"))
        .transform
      result3.asOption ==> Some(NestedValueClass(expected))
      result3.asEither ==> Right(NestedValueClass(expected))
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = NestedValueClass(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedJavaBean[Foo]]
        .withFieldConstPartial(_.getValue.y, partial.Result.fromValue("pi"))
        .transform
      result4.asOption ==> Some(NestedJavaBean(expected))
      result4.asEither ==> Right(NestedJavaBean(expected))
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val result5 = NestedJavaBean(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedProduct[Foo]]
        .withFieldConstPartial(_.value.y, partial.Result.fromValue("pi"))
        .transform
      result5.asOption ==> Some(NestedProduct(expected))
      result5.asEither ==> Right(NestedProduct(expected))
      result5.asErrorPathMessageStrings ==> Iterable.empty

      import trip.*

      val expected2 = User("John", 20, 140)

      val result6 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldConstPartial(_.age, partial.Result.fromValue(20))
        .transform
      result6.asOption ==> Some(expected2)
      result6.asEither ==> Right(expected2)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val result7 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldConstPartial(_.age, partial.Result.fromEmpty)
        .transform

      result7.asOption ==> None
      result7.asEither.isLeft ==> true
      result7.asErrorPathMessageStrings ==> Iterable(
        "<const for _.age>" -> "empty value"
      )

      val result8 = NestedProduct(Person("John", 10, 140))
        .intoPartial[NestedValueClass[User]]
        .withFieldConstPartial(_.value.age, partial.Result.fromValue(20))
        .transform
      result8.asOption ==> Some(NestedValueClass(expected2))
      result8.asEither ==> Right(NestedValueClass(expected2))
      result8.asErrorPathMessageStrings ==> Iterable.empty

      val result9 = NestedValueClass(Person("John", 10, 140))
        .intoPartial[NestedJavaBean[User]]
        .withFieldConstPartial(_.getValue.age, partial.Result.fromValue(20))
        .transform
      result9.asOption ==> Some(NestedJavaBean(expected2))
      result9.asEither ==> Right(NestedJavaBean(expected2))
      result9.asErrorPathMessageStrings ==> Iterable.empty

      val result10 = NestedJavaBean(Person("John", 10, 140))
        .intoPartial[NestedProduct[User]]
        .withFieldConstPartial(_.value.age, partial.Result.fromValue(20))
        .transform
      result10.asOption ==> Some(NestedProduct(expected2))
      result10.asEither ==> Right(NestedProduct(expected2))
      result10.asErrorPathMessageStrings ==> Iterable.empty

      val expected3 = NestedComplex(
        User("John", 10, 140),
        Some(User("John", 15, 140)),
        Right(User("John", 30, 140)),
        List(User("John", 40, 140)),
        ListMap(User("John", 50, 140) -> User("John", 60, 140))
      )

      val result11 = NestedComplex(
        Person("John", 10, 140),
        Some(Person("John", 10, 140)),
        Right(Person("John", 10, 140)),
        List(Person("John", 10, 140)),
        ListMap(Person("John", 10, 140) -> Person("John", 10, 140))
      ).intoPartial[NestedComplex[User]]
        .withFieldConstPartial(_.option.matchingSome.age, partial.Result.fromValue(15))
        .withFieldConstPartial(_.either.matchingLeft.age, partial.Result.fromValue(20))
        .withFieldConstPartial(_.either.matchingRight.age, partial.Result.fromValue(30))
        .withFieldConstPartial(_.collection.everyItem.age, partial.Result.fromValue(40))
        .withFieldConstPartial(_.map.everyMapKey.age, partial.Result.fromValue(50))
        .withFieldConstPartial(_.map.everyMapValue.age, partial.Result.fromValue(60))
        .transform
      result11.asOption ==> Some(expected3)
      result11.asEither ==> Right(expected3)
      result11.asErrorPathMessageStrings ==> Iterable.empty

      val expected4 = Vector(NestedADT.Foo(User("John", 20, 140)), NestedADT.Bar(User("John", 30, 140)))

      val result12 =
        List[NestedADT[Person]](NestedADT.Foo(Person("John", 10, 140)), NestedADT.Bar(Person("John", 10, 140)))
          .intoPartial[Vector[NestedADT[User]]]
          .withFieldConstPartial(_.everyItem.matching[NestedADT.Foo[User]].foo.age, partial.Result.fromValue(20))
          .withFieldConstPartial(_.everyItem.matching[NestedADT.Bar[User]].bar.age, partial.Result.fromValue(30))
          .transform
      result12.asOption ==> Some(expected4)
      result12.asEither ==> Right(expected4)
      result12.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should work with semiautomatic derivation") {
      import products.{Foo, Bar}

      val expected = Foo(3, "pi", (3.14, 3.14))

      val result = PartialTransformer
        .define[Bar, Foo]
        .withFieldConstPartial(_.y, partial.Result.fromValue("pi"))
        .buildTransformer
        .transform(Bar(3, (3.14, 3.14)))
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("setting .withFieldComputed(_.field, source => value)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(_.y + "abc", _.toString).transform
        """
      ).check(
        "The path expression has to be a single chain of calls on the original input, got operation other than value extraction:"
      )

      compileErrors(
        """
        val haveY = HaveY("")
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(cc => haveY.y, _.toString).transform
        """
      ).check("The path expression has to be a single chain of calls on the original input, got external identifier:")

      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(_.matching, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(_.matchingSome, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(_.matchingLeft, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(_.matchingRight, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(_.everyItem, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(_.everyMapKey, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(_.everyMapValue, _.toString).transform
        """
      ).arePresent()
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}, nestedpath.*

      implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

      val expected = Foo(3, "3", (3.14, 3.14))

      val result = Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(_.y, _.x.toString).transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(cc => cc.y, _.x.toString).transform
      result2.asOption ==> Some(expected)
      result2.asEither ==> Right(expected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val result3 = NestedProduct(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedValueClass[Foo]]
        .withFieldComputed(_.value.y, _.value.x.toString)
        .transform
      result3.asOption ==> Some(NestedValueClass(expected))
      result3.asEither ==> Right(NestedValueClass(expected))
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = NestedValueClass(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedJavaBean[Foo]]
        .withFieldComputed(_.getValue.y, _.value.x.toString)
        .transform
      result4.asOption ==> Some(NestedJavaBean(expected))
      result4.asEither ==> Right(NestedJavaBean(expected))
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val result5 = NestedJavaBean(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedProduct[Foo]]
        .withFieldComputed(_.value.y, _.getValue.x.toString)
        .transform
      result5.asOption ==> Some(NestedProduct(expected))
      result5.asEither ==> Right(NestedProduct(expected))
      result5.asErrorPathMessageStrings ==> Iterable.empty

      import trip.*

      val expected2 = User("John", 20, 140)

      val result6 = Person("John", 10, 140).intoPartial[User].withFieldComputed(_.age, _.age * 2).transform
      result6.asOption ==> Some(expected2)
      result6.asEither ==> Right(expected2)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val result7 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldComputed(_.age, _ => sys.error("error happened"))
        .transform
      result7.asOption ==> None
      result7.asEither.isLeft ==> true
      result7.asErrorPathMessageStrings ==> Iterable(
        "<computed for _.age>" -> "error happened"
      )

      val result8 = NestedProduct(Person("John", 10, 140))
        .intoPartial[NestedValueClass[User]]
        .withFieldComputed(_.value.age, _.value.age * 2)
        .transform
      result8.asOption ==> Some(NestedValueClass(expected2))
      result8.asEither ==> Right(NestedValueClass(expected2))
      result8.asErrorPathMessageStrings ==> Iterable.empty

      val result9 = NestedValueClass(Person("John", 10, 140))
        .intoPartial[NestedJavaBean[User]]
        .withFieldComputed(_.getValue.age, _.value.age * 2)
        .transform
      result9.asOption ==> Some(NestedJavaBean(expected2))
      result9.asEither ==> Right(NestedJavaBean(expected2))
      result9.asErrorPathMessageStrings ==> Iterable.empty

      val result10 = NestedJavaBean(Person("John", 10, 140))
        .intoPartial[NestedProduct[User]]
        .withFieldComputed(_.value.age, _.getValue.age * 2)
        .transform
      result10.asOption ==> Some(NestedProduct(expected2))
      result10.asEither ==> Right(NestedProduct(expected2))
      result10.asErrorPathMessageStrings ==> Iterable.empty

      val expected3 = NestedComplex(
        User("John", 10, 140),
        Some(User("John", 15, 140)),
        Right(User("John", 30, 140)),
        List(User("John", 40, 140)),
        ListMap(User("John", 50, 140) -> User("John", 60, 140))
      )

      val result11 = NestedComplex(
        Person("John", 10, 140),
        Some(Person("John", 10, 140)),
        Right(Person("John", 10, 140)),
        List(Person("John", 10, 140)),
        ListMap(Person("John", 10, 140) -> Person("John", 10, 140))
      ).intoPartial[NestedComplex[User]]
        .withFieldComputed(_.option.matchingSome.age, _.id.age + 5)
        .withFieldComputed(_.either.matchingLeft.age, _.id.age * 2)
        .withFieldComputed(_.either.matchingRight.age, _.id.age * 3)
        .withFieldComputed(_.collection.everyItem.age, _.id.age * 4)
        .withFieldComputed(_.map.everyMapKey.age, _.id.age * 5)
        .withFieldComputed(_.map.everyMapValue.age, _.id.age * 6)
        .transform
      result11.asOption ==> Some(expected3)
      result11.asEither ==> Right(expected3)
      result11.asErrorPathMessageStrings ==> Iterable.empty

      val expected4 = Vector(NestedADT.Foo(User("John", 20, 140)), NestedADT.Bar(User("John", 30, 140)))

      val result12 =
        List[NestedADT[Person]](NestedADT.Foo(Person("John", 10, 140)), NestedADT.Bar(Person("John", 10, 140)))
          .intoPartial[Vector[NestedADT[User]]]
          .withFieldComputed(_.everyItem.matching[NestedADT.Foo[User]].foo.age, _ => 20)
          .withFieldComputed(_.everyItem.matching[NestedADT.Bar[User]].bar.age, _ => 30)
          .transform
      result12.asOption ==> Some(expected4)
      result12.asEither ==> Right(expected4)
      result12.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should work with semiautomatic derivation") {
      import products.{Foo, Bar}

      val expected = Foo(3, "3", (3.14, 3.14))

      val result = PartialTransformer
        .define[Bar, Foo]
        .withFieldComputed(_.y, _.x.toString)
        .buildTransformer
        .transform(Bar(3, (3.14, 3.14)))
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("setting .withFieldComputedFrom(_.field)(_.field, source => value)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedFrom(_.x)(_.y + "abc", _.toString).transform
        """
      ).check(
        "The path expression has to be a single chain of calls on the original input, got operation other than value extraction:"
      )

      compileErrors(
        """
        val haveY = HaveY("")
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedFrom(_.x)(cc => haveY.y, _.toString).transform
        """
      ).check("The path expression has to be a single chain of calls on the original input, got external identifier:")

      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedFrom(_.x)(_.matching, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedFrom(_.x)(_.matchingSome, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedFrom(_.x)(_.matchingLeft, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedFrom(_.x)(_.matchingRight, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedFrom(_.x)(_.everyItem, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedFrom(_.x)(_.everyMapKey, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedFrom(_.x)(_.everyMapValue, _.toString).transform
        """
      ).arePresent()
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}, nestedpath.*

      implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

      val expected = Foo(3, "3", (3.14, 3.14))

      val result = Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedFrom(_.x)(_.y, _.toString).transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedFrom(_.x)(cc => cc.y, _.toString).transform
      result2.asOption ==> Some(expected)
      result2.asEither ==> Right(expected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val result3 = NestedProduct(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedValueClass[Foo]]
        .withFieldComputedFrom(_.value.x)(_.value.y, _.toString)
        .transform
      result3.asOption ==> Some(NestedValueClass(expected))
      result3.asEither ==> Right(NestedValueClass(expected))
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = NestedValueClass(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedJavaBean[Foo]]
        .withFieldComputedFrom(_.value.x)(_.getValue.y, _.toString)
        .transform
      result4.asOption ==> Some(NestedJavaBean(expected))
      result4.asEither ==> Right(NestedJavaBean(expected))
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val result5 = NestedJavaBean(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedProduct[Foo]]
        .withFieldComputedFrom(_.getValue.x)(_.value.y, _.toString)
        .transform
      result5.asOption ==> Some(NestedProduct(expected))
      result5.asEither ==> Right(NestedProduct(expected))
      result5.asErrorPathMessageStrings ==> Iterable.empty

      import trip.*

      val expected2 = User("John", 20, 140)

      val result6 = Person("John", 10, 140).intoPartial[User].withFieldComputedFrom(_.age)(_.age, _ * 2).transform
      result6.asOption ==> Some(expected2)
      result6.asEither ==> Right(expected2)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val result7 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldComputedFrom(_.age)(_.age, _ => sys.error("error happened"))
        .transform
      result7.asOption ==> None
      result7.asEither.isLeft ==> true
      result7.asErrorPathMessageStrings ==> Iterable(
        "age => <computed for _.age>" -> "error happened"
      )

      val result8 = NestedProduct(Person("John", 10, 140))
        .intoPartial[NestedValueClass[User]]
        .withFieldComputedFrom(_.value.age)(_.value.age, _ * 2)
        .transform
      result8.asOption ==> Some(NestedValueClass(expected2))
      result8.asEither ==> Right(NestedValueClass(expected2))
      result8.asErrorPathMessageStrings ==> Iterable.empty

      val result9 = NestedValueClass(Person("John", 10, 140))
        .intoPartial[NestedJavaBean[User]]
        .withFieldComputedFrom(_.value.age)(_.getValue.age, _ * 2)
        .transform
      result9.asOption ==> Some(NestedJavaBean(expected2))
      result9.asEither ==> Right(NestedJavaBean(expected2))
      result9.asErrorPathMessageStrings ==> Iterable.empty

      val result10 = NestedJavaBean(Person("John", 10, 140))
        .intoPartial[NestedProduct[User]]
        .withFieldComputedFrom(_.getValue.age)(_.value.age, _ * 2)
        .transform
      result10.asOption ==> Some(NestedProduct(expected2))
      result10.asEither ==> Right(NestedProduct(expected2))
      result10.asErrorPathMessageStrings ==> Iterable.empty

      val expected3 = NestedComplex(
        User("John", 10, 140),
        Some(User("John", 15, 140)),
        Right(User("John", 30, 140)),
        List(User("John", 40, 140)),
        ListMap(User("John", 50, 140) -> User("John", 60, 140))
      )

      val result11 = NestedComplex(
        Person("John", 10, 140),
        Some(Person("John", 10, 140)),
        Right(Person("John", 10, 140)),
        List(Person("John", 10, 140)),
        ListMap(Person("John", 10, 140) -> Person("John", 10, 140))
      ).intoPartial[NestedComplex[User]]
        .withFieldComputedFrom(_.id.age)(_.option.matchingSome.age, _ + 5)
        .withFieldComputedFrom(_.id.age)(_.either.matchingLeft.age, _ * 2)
        .withFieldComputedFrom(_.id.age)(_.either.matchingRight.age, _ * 3)
        .withFieldComputedFrom(_.id.age)(_.collection.everyItem.age, _ * 4)
        .withFieldComputedFrom(_.id.age)(_.map.everyMapKey.age, _ * 5)
        .withFieldComputedFrom(_.id.age)(_.map.everyMapValue.age, _ * 6)
        .transform
      result11.asOption ==> Some(expected3)
      result11.asEither ==> Right(expected3)
      result11.asErrorPathMessageStrings ==> Iterable.empty

      val expected4 = Vector(NestedADT.Foo(User("John", 20, 140)), NestedADT.Bar(User("John", 30, 140)))

      val result12 =
        List[NestedADT[Person]](NestedADT.Foo(Person("John", 10, 140)), NestedADT.Bar(Person("John", 10, 140)))
          .intoPartial[Vector[NestedADT[User]]]
          .withFieldComputedFrom(_.everyItem.matching[NestedADT.Foo[Person]].foo.age)(
            _.everyItem.matching[NestedADT.Foo[User]].foo.age,
            _ * 2
          )
          .withFieldComputedFrom(_.everyItem.matching[NestedADT.Bar[Person]].bar.age)(
            _.everyItem.matching[NestedADT.Bar[User]].bar.age,
            _ * 3
          )
          .transform
      result12.asOption ==> Some(expected4)
      result12.asEither ==> Right(expected4)
      result12.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should work with semiautomatic derivation") {
      import products.{Foo, Bar}

      val expected = Foo(3, "3", (3.14, 3.14))

      val result = PartialTransformer
        .define[Bar, Foo]
        .withFieldComputedFrom(_.x)(_.y, _.toString)
        .buildTransformer
        .transform(Bar(3, (3.14, 3.14)))
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("setting .withFieldComputedPartial(_.field, source => value)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartial(_.y + "abc", _ => ???).transform
        """
      ).check(
        "The path expression has to be a single chain of calls on the original input, got operation other than value extraction:"
      )

      compileErrors(
        """
        val haveY = HaveY("")
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartial(cc => haveY.y, _ => ???).transform
        """
      ).check("The path expression has to be a single chain of calls on the original input, got external identifier:")

      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartial(_.matching, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartial(_.matchingSome, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartial(_.matchingLeft, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartial(_.matchingRight, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartial(_.everyItem, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartial(_.everyMapKey, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartial(_.everyMapValue, _ => ???).transform
        """
      ).arePresent()
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}, nestedpath.*

      implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

      val expected = Foo(3, "3", (3.14, 3.14))

      val result = Bar(3, (3.14, 3.14))
        .intoPartial[Foo]
        .withFieldComputedPartial(_.y, bar => partial.Result.fromValue(bar.x.toString))
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Bar(3, (3.14, 3.14))
        .intoPartial[Foo]
        .withFieldComputedPartial(cc => cc.y, bar => partial.Result.fromValue(bar.x.toString))
        .transform
      result2.asOption ==> Some(expected)
      result2.asEither ==> Right(expected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val result3 = NestedProduct(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedValueClass[Foo]]
        .withFieldComputedPartial(_.value.y, bar => partial.Result.fromValue(bar.value.x.toString))
        .transform
      result3.asOption ==> Some(NestedValueClass(expected))
      result3.asEither ==> Right(NestedValueClass(expected))
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = NestedValueClass(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedJavaBean[Foo]]
        .withFieldComputedPartial(_.getValue.y, bar => partial.Result.fromValue(bar.value.x.toString))
        .transform
      result4.asOption ==> Some(NestedJavaBean(expected))
      result4.asEither ==> Right(NestedJavaBean(expected))
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val result5 = NestedJavaBean(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedProduct[Foo]]
        .withFieldComputedPartial(_.value.y, bar => partial.Result.fromValue(bar.getValue.x.toString))
        .transform
      result5.asOption ==> Some(NestedProduct(expected))
      result5.asEither ==> Right(NestedProduct(expected))
      result5.asErrorPathMessageStrings ==> Iterable.empty

      import trip.*

      val expected2 = User("John", 20, 140)

      val result6 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldComputedPartial(_.age, bar => partial.Result.fromValue(bar.age * 2))
        .transform
      result6.asOption ==> Some(expected2)
      result6.asEither ==> Right(expected2)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val result7 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldComputedPartial(_.age, _ => partial.Result.fromEmpty)
        .transform
      result7.asOption ==> None
      result7.asEither.isLeft ==> true
      result7.asErrorPathMessageStrings ==> Iterable(
        "<computed for _.age>" -> "empty value"
      )

      val result8 = NestedProduct(Person("John", 10, 140))
        .intoPartial[NestedValueClass[User]]
        .withFieldComputedPartial(_.value.age, nested => partial.Result.fromValue(nested.value.age * 2))
        .transform
      result8.asOption ==> Some(NestedValueClass(expected2))
      result8.asEither ==> Right(NestedValueClass(expected2))
      result8.asErrorPathMessageStrings ==> Iterable.empty

      val result9 = NestedValueClass(Person("John", 10, 140))
        .intoPartial[NestedJavaBean[User]]
        .withFieldComputedPartial(_.getValue.age, nested => partial.Result.fromValue(nested.value.age * 2))
        .transform
      result9.asOption ==> Some(NestedJavaBean(expected2))
      result9.asEither ==> Right(NestedJavaBean(expected2))
      result9.asErrorPathMessageStrings ==> Iterable.empty

      val result10 = NestedJavaBean(Person("John", 10, 140))
        .intoPartial[NestedProduct[User]]
        .withFieldComputedPartial(_.value.age, nested => partial.Result.fromValue(nested.getValue.age * 2))
        .transform
      result10.asOption ==> Some(NestedProduct(expected2))
      result10.asEither ==> Right(NestedProduct(expected2))
      result10.asErrorPathMessageStrings ==> Iterable.empty

      val expected3 = NestedComplex(
        User("John", 10, 140),
        Some(User("John", 15, 140)),
        Right(User("John", 30, 140)),
        List(User("John", 40, 140)),
        ListMap(User("John", 50, 140) -> User("John", 60, 140))
      )

      val result11 = NestedComplex(
        Person("John", 10, 140),
        Some(Person("John", 10, 140)),
        Right(Person("John", 10, 140)),
        List(Person("John", 10, 140)),
        ListMap(Person("John", 10, 140) -> Person("John", 10, 140))
      ).intoPartial[NestedComplex[User]]
        .withFieldComputedPartial(_.option.matchingSome.age, c => partial.Result.fromValue(c.id.age + 5))
        .withFieldComputedPartial(_.either.matchingLeft.age, c => partial.Result.fromValue(c.id.age * 2))
        .withFieldComputedPartial(_.either.matchingRight.age, c => partial.Result.fromValue(c.id.age * 3))
        .withFieldComputedPartial(_.collection.everyItem.age, c => partial.Result.fromValue(c.id.age * 4))
        .withFieldComputedPartial(_.map.everyMapKey.age, c => partial.Result.fromValue(c.id.age * 5))
        .withFieldComputedPartial(_.map.everyMapValue.age, c => partial.Result.fromValue(c.id.age * 6))
        .transform
      result11.asOption ==> Some(expected3)
      result11.asEither ==> Right(expected3)
      result11.asErrorPathMessageStrings ==> Iterable.empty

      val expected4 = Vector(NestedADT.Foo(User("John", 20, 140)), NestedADT.Bar(User("John", 30, 140)))

      val result12 =
        List[NestedADT[Person]](NestedADT.Foo(Person("John", 10, 140)), NestedADT.Bar(Person("John", 10, 140)))
          .intoPartial[Vector[NestedADT[User]]]
          .withFieldComputedPartial(
            _.everyItem.matching[NestedADT.Foo[User]].foo.age,
            _ => partial.Result.fromValue(20)
          )
          .withFieldComputedPartial(
            _.everyItem.matching[NestedADT.Bar[User]].bar.age,
            _ => partial.Result.fromValue(30)
          )
          .transform
      result12.asOption ==> Some(expected4)
      result12.asEither ==> Right(expected4)
      result12.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should work with semiautomatic derivation") {
      import products.{Foo, Bar}

      val expected = Foo(3, "3", (3.14, 3.14))

      val result = PartialTransformer
        .define[Bar, Foo]
        .withFieldComputedPartial(_.y, bar => partial.Result.fromValue(bar.x.toString))
        .buildTransformer
        .transform(Bar(3, (3.14, 3.14)))
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("setting .withFieldComputedPartialFrom(_.field)(_.field, source => value)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartialFrom(_.x)(_.y + "abc", _ => ???).transform
        """
      ).check(
        "The path expression has to be a single chain of calls on the original input, got operation other than value extraction:"
      )

      compileErrors(
        """
        val haveY = HaveY("")
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartialFrom(_.x)(cc => haveY.y, _ => ???).transform
        """
      ).check("The path expression has to be a single chain of calls on the original input, got external identifier:")

      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartialFrom(_.x)(_.matching, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartialFrom(_.x)(_.matchingSome, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartialFrom(_.x)(_.matchingLeft, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartialFrom(_.x)(_.matchingRight, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartialFrom(_.x)(_.everyItem, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartialFrom(_.x)(_.everyMapKey, _ => ???).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputedPartialFrom(_.x)(_.everyMapValue, _ => ???).transform
        """
      ).arePresent()
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}, nestedpath.*

      implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

      val expected = Foo(3, "3", (3.14, 3.14))

      val result = Bar(3, (3.14, 3.14))
        .intoPartial[Foo]
        .withFieldComputedPartialFrom(_.x)(_.y, x => partial.Result.fromValue(x.toString))
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Bar(3, (3.14, 3.14))
        .intoPartial[Foo]
        .withFieldComputedPartialFrom(_.x)(cc => cc.y, x => partial.Result.fromValue(x.toString))
        .transform
      result2.asOption ==> Some(expected)
      result2.asEither ==> Right(expected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val result3 = NestedProduct(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedValueClass[Foo]]
        .withFieldComputedPartialFrom(_.value.x)(_.value.y, x => partial.Result.fromValue(x.toString))
        .transform
      result3.asOption ==> Some(NestedValueClass(expected))
      result3.asEither ==> Right(NestedValueClass(expected))
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = NestedValueClass(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedJavaBean[Foo]]
        .withFieldComputedPartialFrom(_.value.x)(_.getValue.y, x => partial.Result.fromValue(x.toString))
        .transform
      result4.asOption ==> Some(NestedJavaBean(expected))
      result4.asEither ==> Right(NestedJavaBean(expected))
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val result5 = NestedJavaBean(Bar(3, (3.14, 3.14)))
        .intoPartial[NestedProduct[Foo]]
        .withFieldComputedPartialFrom(_.getValue.x)(_.value.y, x => partial.Result.fromValue(x.toString))
        .transform
      result5.asOption ==> Some(NestedProduct(expected))
      result5.asEither ==> Right(NestedProduct(expected))
      result5.asErrorPathMessageStrings ==> Iterable.empty

      import trip.*

      val expected2 = User("John", 20, 140)

      val result6 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldComputedPartialFrom(_.age)(_.age, age => partial.Result.fromValue(age * 2))
        .transform
      result6.asOption ==> Some(expected2)
      result6.asEither ==> Right(expected2)
      result6.asErrorPathMessageStrings ==> Iterable.empty

      val result7 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldComputedPartialFrom(_.age)(_.age, _ => partial.Result.fromEmpty)
        .transform
      result7.asOption ==> None
      result7.asEither.isLeft ==> true
      result7.asErrorPathMessageStrings ==> Iterable(
        "age => <computed for _.age>" -> "empty value"
      )

      val result8 = NestedProduct(Person("John", 10, 140))
        .intoPartial[NestedValueClass[User]]
        .withFieldComputedPartialFrom(_.value.age)(_.value.age, age => partial.Result.fromValue(age * 2))
        .transform
      result8.asOption ==> Some(NestedValueClass(expected2))
      result8.asEither ==> Right(NestedValueClass(expected2))
      result8.asErrorPathMessageStrings ==> Iterable.empty

      val result9 = NestedValueClass(Person("John", 10, 140))
        .intoPartial[NestedJavaBean[User]]
        .withFieldComputedPartialFrom(_.value.age)(_.getValue.age, age => partial.Result.fromValue(age * 2))
        .transform
      result9.asOption ==> Some(NestedJavaBean(expected2))
      result9.asEither ==> Right(NestedJavaBean(expected2))
      result9.asErrorPathMessageStrings ==> Iterable.empty

      val result10 = NestedJavaBean(Person("John", 10, 140))
        .intoPartial[NestedProduct[User]]
        .withFieldComputedPartialFrom(_.getValue.age)(_.value.age, age => partial.Result.fromValue(age * 2))
        .transform
      result10.asOption ==> Some(NestedProduct(expected2))
      result10.asEither ==> Right(NestedProduct(expected2))
      result10.asErrorPathMessageStrings ==> Iterable.empty

      val expected3 = NestedComplex(
        User("John", 10, 140),
        Some(User("John", 15, 140)),
        Right(User("John", 30, 140)),
        List(User("John", 40, 140)),
        ListMap(User("John", 50, 140) -> User("John", 60, 140))
      )

      val result11 = NestedComplex(
        Person("John", 10, 140),
        Some(Person("John", 10, 140)),
        Right(Person("John", 10, 140)),
        List(Person("John", 10, 140)),
        ListMap(Person("John", 10, 140) -> Person("John", 10, 140))
      ).intoPartial[NestedComplex[User]]
        .withFieldComputedPartialFrom(_.id.age)(_.option.matchingSome.age, age => partial.Result.fromValue(age + 5))
        .withFieldComputedPartialFrom(_.id.age)(_.either.matchingLeft.age, age => partial.Result.fromValue(age * 2))
        .withFieldComputedPartialFrom(_.id.age)(_.either.matchingRight.age, age => partial.Result.fromValue(age * 3))
        .withFieldComputedPartialFrom(_.id.age)(_.collection.everyItem.age, age => partial.Result.fromValue(age * 4))
        .withFieldComputedPartialFrom(_.id.age)(_.map.everyMapKey.age, age => partial.Result.fromValue(age * 5))
        .withFieldComputedPartialFrom(_.id.age)(_.map.everyMapValue.age, age => partial.Result.fromValue(age * 6))
        .transform
      result11.asOption ==> Some(expected3)
      result11.asEither ==> Right(expected3)
      result11.asErrorPathMessageStrings ==> Iterable.empty

      val expected4 = Vector(NestedADT.Foo(User("John", 20, 140)), NestedADT.Bar(User("John", 30, 140)))

      val result12 =
        List[NestedADT[Person]](NestedADT.Foo(Person("John", 10, 140)), NestedADT.Bar(Person("John", 10, 140)))
          .intoPartial[Vector[NestedADT[User]]]
          .withFieldComputedPartialFrom(_.everyItem.matching[NestedADT.Foo[Person]].foo.age)(
            _.everyItem.matching[NestedADT.Foo[User]].foo.age,
            age => partial.Result.fromValue(age * 2)
          )
          .withFieldComputedPartialFrom(_.everyItem.matching[NestedADT.Bar[Person]].bar.age)(
            _.everyItem.matching[NestedADT.Bar[User]].bar.age,
            age => partial.Result.fromValue(age * 3)
          )
          .transform
      result12.asOption ==> Some(expected4)
      result12.asEither ==> Right(expected4)
      result12.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should work with semiautomatic derivation") {
      import products.{Foo, Bar}

      val expected = Foo(3, "3", (3.14, 3.14))

      val result = PartialTransformer
        .define[Bar, Foo]
        .withFieldComputedPartialFrom(_.x)(_.y, x => partial.Result.fromValue(x.toString))
        .buildTransformer
        .transform(Bar(3, (3.14, 3.14)))
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("""setting .withFieldRenamed(_.from, _.to)""") {

    test("should not be enabled by default") {
      import products.Renames.*

      compileErrors("""User(1, "Kuba", Some(28)).transformInto[UserPL]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Renames.User to io.scalaland.chimney.fixtures.products.Renames.UserPL",
        "io.scalaland.chimney.fixtures.products.Renames.UserPL",
        "  imie: java.lang.String - no accessor named imie in source type io.scalaland.chimney.fixtures.products.Renames.User",
        "  wiek: scala.util.Either[scala.Unit, scala.Int] - no accessor named wiek in source type io.scalaland.chimney.fixtures.products.Renames.User",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""User(1, "Kuba", Some(28)).into[UserPL].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Renames.User to io.scalaland.chimney.fixtures.products.Renames.UserPL",
        "io.scalaland.chimney.fixtures.products.Renames.UserPL",
        "  imie: java.lang.String - no accessor named imie in source type io.scalaland.chimney.fixtures.products.Renames.User",
        "  wiek: scala.util.Either[scala.Unit, scala.Int] - no accessor named wiek in source type io.scalaland.chimney.fixtures.products.Renames.User",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should not compile when selector is invalid") {
      import products.Renames.*

      compileErrors(
        """
        User(1, "Kuba", Some(28)).intoPartial[UserPL].withFieldRenamed(_.age + "ABC", _.toString).transform
        """
      ).check(
        "The path expression has to be a single chain of calls on the original input, got operation other than value extraction:"
      )

      compileErrors(
        """
        val str = "string"
        User(1, "Kuba", Some(28)).intoPartial[UserPL].withFieldRenamed(u => str, _.toString).transform
        """
      ).check("The path expression has to be a single chain of calls on the original input, got external identifier:")

      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldRenamed(_.matching, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldRenamed(_.matchingSome, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldRenamed(_.matchingLeft, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldRenamed(_.matchingRight, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldRenamed(_.everyItem, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldRenamed(_.everyMapKey, _.toString).transform
        """
      ).arePresent()
      compileErrors(
        """
        Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldRenamed(_.everyMapValue, _.toString).transform
        """
      ).arePresent()
    }

    test(
      "should provide a value to a selected target field from a selected source field when there is no same-named source field"
    ) {
      import products.Renames.*, nestedpath.*

      val expected = UserPLStd(1, "Kuba", Some(28))

      val result = User(1, "Kuba", Some(28))
        .intoPartial[UserPLStd]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

      val result2 = NestedProduct(User(1, "Kuba", Some(28)))
        .intoPartial[NestedValueClass[UserPLStd]]
        .withFieldRenamed(_.value.name, _.value.imie)
        .withFieldRenamed(_.value.age, _.value.wiek)
        .transform
      result2.asOption ==> Some(NestedValueClass(expected))
      result2.asEither ==> Right(NestedValueClass(expected))
      result2.asErrorPathMessageStrings ==> Iterable.empty

      val result3 = NestedValueClass(User(1, "Kuba", Some(28)))
        .intoPartial[NestedJavaBean[UserPLStd]]
        .withFieldRenamed(_.value.name, _.getValue.imie)
        .withFieldRenamed(_.value.age, _.getValue.wiek)
        .transform
      result3.asOption ==> Some(NestedJavaBean(expected))
      result3.asEither ==> Right(NestedJavaBean(expected))
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = NestedJavaBean(User(1, "Kuba", Some(28)))
        .intoPartial[NestedProduct[UserPLStd]]
        .withFieldRenamed(_.getValue.name, _.value.imie)
        .withFieldRenamed(_.getValue.age, _.value.wiek)
        .transform
      result4.asOption ==> Some(NestedProduct(expected))
      result4.asEither ==> Right(NestedProduct(expected))
      result4.asErrorPathMessageStrings ==> Iterable.empty

      val expected2 = NestedComplex(
        UserPLStrict(1, "Kuba", 28),
        Some(UserPLStrict(2, "Kuba", 28)),
        Right(UserPLStrict(3, "Kuba", 28)),
        List(UserPLStrict(4, "Kuba", 28)),
        ListMap(UserPLStrict(5, "Kuba", 28) -> UserPLStrict(6, "Kuba", 28))
      )

      val result5 = NestedComplex(
        UserStrict(1, "Kuba", 28),
        Some(UserStrict(2, "Kuba", 28)),
        Right(UserStrict(3, "Kuba", 28)),
        List(UserStrict(4, "Kuba", 28)),
        ListMap(UserStrict(5, "Kuba", 28) -> UserStrict(6, "Kuba", 28))
      ).intoPartial[NestedComplex[UserPLStrict]]
        .withFieldRenamed(_.id.name, _.id.imie)
        .withFieldRenamed(_.id.age, _.id.wiek)
        .withFieldRenamed(_.id.name, _.option.matchingSome.imie)
        .withFieldRenamed(_.id.age, _.option.matchingSome.wiek)
        .withFieldRenamed(_.id.name, _.either.matchingLeft.imie)
        .withFieldRenamed(_.id.age, _.either.matchingLeft.wiek)
        .withFieldRenamed(_.id.name, _.either.matchingRight.imie)
        .withFieldRenamed(_.id.age, _.either.matchingRight.wiek)
        .withFieldRenamed(_.id.name, _.collection.everyItem.imie)
        .withFieldRenamed(_.id.age, _.collection.everyItem.wiek)
        .withFieldRenamed(_.id.name, _.map.everyMapKey.imie)
        .withFieldRenamed(_.id.age, _.map.everyMapKey.wiek)
        .withFieldRenamed(_.id.name, _.map.everyMapValue.imie)
        .withFieldRenamed(_.id.age, _.map.everyMapValue.wiek)
        .transform
      result5.asOption ==> Some(expected2)
      result5.asEither ==> Right(expected2)
      result5.asErrorPathMessageStrings ==> Iterable.empty
    }

    test(
      "should provide a value to a selected target field from a selected source field despite an existing same-named source field"
    ) {
      import products.Renames.*

      val expected = User(666, "Kuba", Some(28))

      val result = User2ID(1, "Kuba", Some(28), 666)
        .intoPartial[User]
        .withFieldRenamed(_.extraID, _.id)
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should not compile if renamed value change type but an there is no Transformer available") {
      import products.Renames.*

      compileErrors(
        """
        User(1, "Kuba", Some(28))
          .intoPartial[UserPL]
          .withFieldRenamed(_.name, _.imie)
          .withFieldRenamed(_.age, _.wiek)
          .transform
        """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Renames.User to io.scalaland.chimney.fixtures.products.Renames.UserPL",
        "io.scalaland.chimney.fixtures.products.Renames.UserPL",
        "  wiek: scala.util.Either[scala.Unit, scala.Int] - can't derive transformation from wiek: scala.Option[scala.Int] in source type io.scalaland.chimney.fixtures.products.Renames.User",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should convert renamed value if types differ but an implicit Total Transformer exists") {
      import products.Renames.*
      implicit val convert: Transformer[Option[Int], Either[Unit, Int]] = ageToWiekTransformer

      val expected = UserPL(1, "Kuba", Right(28))
      val result = User(1, "Kuba", Some(28))
        .intoPartial[UserPL]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val expected2 = UserPL(1, "Kuba", Left(()))
      val result2 = User(1, "Kuba", None)
        .intoPartial[UserPL]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform
      result2.asOption ==> Some(expected2)
      result2.asEither ==> Right(expected2)
      result2.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should convert renamed value if types differ but an implicit Partial Transformer exists") {
      import products.Renames.*
      implicit val convert: PartialTransformer[Option[Int], Int] = new PartialTransformer[Option[Int], Int] {
        override def transform(src: Option[Int], failFast: Boolean): partial.Result[Int] =
          partial.Result.fromOption(src)
      }

      val expected = UserPLStrict(1, "Kuba", 28)
      val result = User(1, "Kuba", Some(28))
        .intoPartial[UserPLStrict]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val expected2 = partial.Result.Errors.single(
        partial.Error.fromEmptyValue.prependErrorPath(partial.PathElement.Accessor("age"))
      )
      val result2 = User(1, "Kuba", None)
        .intoPartial[UserPLStrict]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform
      result2.asOption ==> None
      result2.asEither ==> Left(expected2)
      result2.asErrorPathMessageStrings ==> expected2.asErrorPathMessageStrings
    }

    test("should work with semiautomatic derivation") {
      import products.Renames.*

      val expected = UserPLStd(1, "Kuba", Some(28))

      val result = PartialTransformer
        .define[User, UserPLStd]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .buildTransformer
        .transform(User(1, "Kuba", Some(28)))
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("setting .withFieldUnused(_.from)") {
    import products.{Foo, Bar}

    test("should fail derivarion if the field is required") {
      compileErrors("""Foo(10, "test", (1,2)).intoPartial[Bar].withFieldUnused(_.x).transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Foo to io.scalaland.chimney.fixtures.products.Bar",
        "io.scalaland.chimney.fixtures.products.Bar",
        "  x: scala.Int - no accessor named x in source type io.scalaland.chimney.fixtures.products.Foo",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should suppress error if UnusedFieldPolicy is used") {
      compileErrors(
        """Foo(10, "test", (1,2)).intoPartial[Bar].enableUnusedFieldPolicyCheck(FailOnIgnoredSourceVal).transform"""
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Foo to io.scalaland.chimney.fixtures.products.Bar",
        "io.scalaland.chimney.fixtures.products.Bar",
        "  FailOnIgnoredSourceVal policy check failed at _, offenders: y!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      Foo(10, "test", (1, 2))
        .intoPartial[Bar]
        // FIXME: if we swap these 2 it's assertion error in -Xcheck-macros on Scala 3 o_0
        .withFieldUnused(_.y)
        .enableUnusedFieldPolicyCheck(FailOnIgnoredSourceVal)
        .transform
        .asOption ==> Some(Bar(10, (1, 2)))
    }

    test("should work with semiautomatic derivation") {

      PartialTransformer
        .define[Foo, Bar]
        // FIXME: if we swap these 2 it's assertion error in -Xcheck-macros on Scala 3 o_0
        .withFieldUnused(_.y)
        .enableUnusedFieldPolicyCheck(FailOnIgnoredSourceVal)
        .buildTransformer
        .transform(Foo(10, "test", (1, 2)))
        .asOption ==> Some(Bar(10, (1, 2)))
    }
  }

  group("flag .enableDefaultValues") {

    test("should be disabled by default") {
      import products.Defaults.*

      compileErrors("""Source(1, "yy", 1.0).transformIntoPartial[Target]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target",
        "io.scalaland.chimney.fixtures.products.Defaults.Target",
        "  x: scala.Int - no accessor named x in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "  y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "There are default values for x, y, constructor arguments/setters in io.scalaland.chimney.fixtures.products.Defaults.Target. Consider using .enableDefaultValues or .enableDefaultValueForType.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""Source(1, "yy", 1.0).intoPartial[Target].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target",
        "io.scalaland.chimney.fixtures.products.Defaults.Target",
        "  x: scala.Int - no accessor named x in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "  y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "There are default values for x, y, constructor arguments/setters in io.scalaland.chimney.fixtures.products.Defaults.Target. Consider using .enableDefaultValues or .enableDefaultValueForType.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      import products.Renames.*
      @unused implicit val defaultInt: integrations.DefaultValue[Int] = () => 0

      compileErrors("""User(1, "Adam", None).transformIntoPartial[User2ID]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Renames.User to io.scalaland.chimney.fixtures.products.Renames.User2ID",
        "io.scalaland.chimney.fixtures.products.Renames.User2ID",
        "  extraID: scala.Int - no accessor named extraID in source type io.scalaland.chimney.fixtures.products.Renames.User",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""User(1, "Adam", None).intoPartial[User2ID].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Renames.User to io.scalaland.chimney.fixtures.products.Renames.User2ID",
        "io.scalaland.chimney.fixtures.products.Renames.User2ID",
        "  extraID: scala.Int - no accessor named extraID in source type io.scalaland.chimney.fixtures.products.Renames.User",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should not be needed if all target fields with default values have their values provided in other way") {
      import products.Defaults.*

      val expected = Target(30, "yy2", 1.0)

      val result = Source(1, "yy", 1.0)
        .intoPartial[Target]
        .withFieldConst(_.x, 30)
        .withFieldComputed(_.y, _.yy + "2")
        .transform

      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val expected2 = new Target3(30L, "yy2", 1.0)

      val result2 = Source(1, "yy", 1.0)
        .intoPartial[Target3]
        .withFieldConst(_.xx, 30L)
        .withFieldComputed(_.yy, _.yy + "2")
        .transform

      result2.asOption ==> Some(expected2)
      result2.asEither ==> Right(expected2)
      result2.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should enable using default values when no source value can be resolved in flat transformation") {
      import products.Defaults.*

      val expected = Target(10, "y", 1.0)

      val result = Source(1, "yy", 1.0).intoPartial[Target].enableDefaultValues.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default.enableDefaultValues

        val result2 = Source(1, "yy", 1.0).transformIntoPartial[Target]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = Source(1, "yy", 1.0).intoPartial[Target].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should enable using default values when no source value can be resolved in nested transformation") {
      import products.Defaults.*

      val expected = Nested(Target(10, "y", 1.0))

      val result = Nested(Source(1, "yy", 1.0)).intoPartial[Nested[Target]].enableDefaultValues.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default.enableDefaultValues

        val result2 = Nested(Source(1, "yy", 1.0)).transformIntoPartial[Nested[Target]]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = Nested(Source(1, "yy", 1.0)).intoPartial[Nested[Target]].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should ignore default value if other setting provides it or source field exists") {
      import products.Defaults.*

      val expected = Target(30, "yy2", 1.0)

      val result = Source(1, "yy", 1.0)
        .intoPartial[Target]
        .enableDefaultValues
        .withFieldConst(_.x, 30)
        .withFieldComputed(_.y, _.yy + "2")
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default.enableDefaultValues

        val result2 = Source(1, "yy", 1.0)
          .intoPartial[Target]
          .withFieldConst(_.x, 30)
          .withFieldComputed(_.y, _.yy + "2")
          .transform
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test(
      "should ignore default value and fail compilation if source fields with different type but no Transformer exists"
    ) {
      import products.Defaults.*

      compileErrors("""Source(1, "yy", 1.0).intoPartial[Target2].enableDefaultValues.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target2",
        "io.scalaland.chimney.fixtures.products.Defaults.Target2",
        "  xx: scala.Long - can't derive transformation from xx: scala.Int in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      locally {
        @unused implicit val config = TransformerConfiguration.default.enableDefaultValues

        compileErrors("""Source(1, "yy", 1.0).transformIntoPartial[Target2]""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "  xx: scala.Long - can't derive transformation from xx: scala.Int in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
        compileErrors("""Source(1, "yy", 1.0).intoPartial[Target2].transform""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "  xx: scala.Long - can't derive transformation from xx: scala.Int in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
      }
    }

    test("should ignore default value if source fields with different type but Total Transformer for it exists") {
      import products.Defaults.*
      implicit val converter: Transformer[Int, Long] = _.toLong

      val expected = Target2(1L, "yy", 1.0)

      val result = Source(1, "yy", 1.0).intoPartial[Target2].enableDefaultValues.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default.enableDefaultValues

        val result2 = Source(1, "yy", 1.0).transformIntoPartial[Target2]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = Source(1, "yy", 1.0).intoPartial[Target2].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should ignore default value if source fields with different type but Partial Transformer for it exists") {
      import products.Defaults.*
      implicit val converter: PartialTransformer[Int, Long] = (i, _) => partial.Result.fromValue(i.toLong)

      val expected = Target2(1L, "yy", 1.0)

      val result = Source(1, "yy", 1.0).intoPartial[Target2].enableDefaultValues.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default.enableDefaultValues

        val result2 = Source(1, "yy", 1.0).transformIntoPartial[Target2]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = Source(1, "yy", 1.0).intoPartial[Target2].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should use default value provided with DefaultValue[A] when the constructor is missing one") {
      import products.Renames.*

      implicit val defaultInt: integrations.DefaultValue[Int] = () => 0

      val expected = User2ID(1, "Adam", None, 0)

      val result = User(1, "Adam", None).intoPartial[User2ID].enableDefaultValues.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default.enableDefaultValues

        val result2 = User(1, "Adam", None).transformIntoPartial[User2ID]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = User(1, "Adam", None).intoPartial[User2ID].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test(
      "should use default value provided with DefaultValue[A] only for a single field when scoped using .withTargetFlag(_.field)"
    ) {
      import products.Renames.*

      implicit val defaultInt: integrations.DefaultValue[Int] = () => 0

      val expected = User2ID(1, "Adam", None, 0)

      val result = User(1, "Adam", None).intoPartial[User2ID].withTargetFlag(_.extraID).enableDefaultValues.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("flag .disableDefaultValues") {

    test("should disable globally enabled .enableDefaultValues") {
      import products.Defaults.*

      @unused implicit val config = TransformerConfiguration.default.enableDefaultValues

      compileErrors("""Source(1, "yy", 1.0).intoPartial[Target].disableDefaultValues.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target",
        "io.scalaland.chimney.fixtures.products.Defaults.Target",
        "  x: scala.Int - no accessor named x in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "  y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "There are default values for x, y, constructor arguments/setters in io.scalaland.chimney.fixtures.products.Defaults.Target. Consider using .enableDefaultValues or .enableDefaultValueForType.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

  group("flag .enableDefaultValueForType[A]") {

    // "should be disabled by default" - done in enableDefaultValues

    // "should not be needed if all target fields with default values have their values provided in other way" - done in enableDefaultValues

    test("should not enable default values for other types") {
      import products.Defaults.*

      compileErrors("""Source(1, "yy", 1.0).into[Target].enableDefaultValueOfType[Long].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target",
        "io.scalaland.chimney.fixtures.products.Defaults.Target",
        "  x: scala.Int - no accessor named x in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "  y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "There are default values for x, y, constructor arguments/setters in io.scalaland.chimney.fixtures.products.Defaults.Target. Consider using .enableDefaultValues or .enableDefaultValueForType.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should enable using default values when no source value can be resolved in flat transformation") {
      import products.Defaults.*

      val expected = Target(10, "y", 1.0)

      val result = Source(1, "yy", 1.0)
        .intoPartial[Target]
        .enableDefaultValueOfType[Int]
        .enableDefaultValueOfType[String]
        .transform

      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default
          .enableDefaultValueOfType[Int]
          .enableDefaultValueOfType[String]

        val result2 = Source(1, "yy", 1.0).transformIntoPartial[Target]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = Source(1, "yy", 1.0).intoPartial[Target].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should enable using default values when no source value can be resolved in nested transformation") {
      import products.Defaults.*

      val expected = Nested(Target(10, "y", 1.0))

      val result = Nested(Source(1, "yy", 1.0))
        .intoPartial[Nested[Target]]
        .enableDefaultValueOfType[Int]
        .enableDefaultValueOfType[String]
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default
          .enableDefaultValueOfType[Int]
          .enableDefaultValueOfType[String]

        val result2 = Nested(Source(1, "yy", 1.0)).transformIntoPartial[Nested[Target]]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = Nested(Source(1, "yy", 1.0)).intoPartial[Nested[Target]].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should ignore default value if other setting provides it or source field exists") {
      import products.Defaults.*

      val expected = Target(30, "yy2", 1.0)

      val result = Source(1, "yy", 1.0)
        .intoPartial[Target]
        .enableDefaultValueOfType[Int]
        .enableDefaultValueOfType[String]
        .withFieldConst(_.x, 30)
        .withFieldComputed(_.y, _.yy + "2")
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default
          .enableDefaultValueOfType[Int]
          .enableDefaultValueOfType[String]

        val result2 = Source(1, "yy", 1.0)
          .intoPartial[Target]
          .withFieldConst(_.x, 30)
          .withFieldComputed(_.y, _.yy + "2")
          .transform
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test(
      "should ignore default value and fail compilation if source fields with different type but no Transformer exists"
    ) {
      import products.Defaults.*

      compileErrors(
        """
        Source(1, "yy", 1.0).intoPartial[Target2]
          .enableDefaultValueOfType[Long]
          .enableDefaultValueOfType[String]
          .transform
        """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target2",
        "io.scalaland.chimney.fixtures.products.Defaults.Target2",
        "  xx: scala.Long - can't derive transformation from xx: scala.Int in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      locally {
        @unused implicit val config = TransformerConfiguration.default
          .enableDefaultValueOfType[Long]
          .enableDefaultValueOfType[String]

        compileErrors("""Source(1, "yy", 1.0).transformIntoPartial[Target2]""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "  xx: scala.Long - can't derive transformation from xx: scala.Int in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
        compileErrors("""Source(1, "yy", 1.0).intoPartial[Target2].transform""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "  xx: scala.Long - can't derive transformation from xx: scala.Int in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
      }
    }

    test("should ignore default value if source fields with different type but Total Transformer for it exists") {
      import products.Defaults.*
      implicit val converter: Transformer[Int, Long] = _.toLong

      val expected = Target2(1L, "yy", 1.0)

      val result = Source(1, "yy", 1.0)
        .intoPartial[Target2]
        .enableDefaultValueOfType[Long]
        .enableDefaultValueOfType[String]
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default
          .enableDefaultValueOfType[Long]
          .enableDefaultValueOfType[String]

        val result2 = Source(1, "yy", 1.0).transformIntoPartial[Target2]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = Source(1, "yy", 1.0).intoPartial[Target2].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should use default value provided with DefaultValue[A] when the constructor is missing one") {
      import products.Renames.*

      implicit val defaultInt: integrations.DefaultValue[Int] = () => 0

      val expected = User2ID(1, "Adam", None, 0)

      val result = User(1, "Adam", None).intoPartial[User2ID].enableDefaultValueOfType[Int].transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default.enableDefaultValueOfType[Int]

        val result2 = User(1, "Adam", None).transformIntoPartial[User2ID]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = User(1, "Adam", None).intoPartial[User2ID].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test(
      "should use default value provided with DefaultValue[A] only for a single field when scoped using .withTargetFlag(_.field)"
    ) {
      import products.Renames.*

      implicit val defaultInt: integrations.DefaultValue[Int] = () => 0

      val expected = User2ID(1, "Adam", None, 0)

      val result =
        User(1, "Adam", None).intoPartial[User2ID].withTargetFlag(_.extraID).enableDefaultValueOfType[Int].transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("flag .disableDefaultValueForType[A]") {

    test("should disable globally enabled .enableDefaultValues") {
      import products.Defaults.*

      @unused implicit val config = TransformerConfiguration.default
        .enableDefaultValueOfType[Long]
        .enableDefaultValueOfType[String]

      compileErrors(
        """
        Source(1, "yy", 1.0).intoPartial[Target]
          .disableDefaultValueOfType[Long]
          .disableDefaultValueOfType[String]
          .transform
        """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target",
        "io.scalaland.chimney.fixtures.products.Defaults.Target",
        "  x: scala.Int - no accessor named x in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "  y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "There are default values for x, y, constructor arguments/setters in io.scalaland.chimney.fixtures.products.Defaults.Target. Consider using .enableDefaultValues or .enableDefaultValueForType.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

  group("flag .enableInheritedAccessors") {

    test("should be disabled by default") {
      import products.Inherited.*

      compileErrors("(new Source).transformIntoPartial[Target]").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Inherited.Source to io.scalaland.chimney.fixtures.products.Inherited.Target",
        "io.scalaland.chimney.fixtures.products.Inherited.Target",
        "  value: java.lang.String - no accessor named value in source type io.scalaland.chimney.fixtures.products.Inherited.Source",
        "There are inherited definitions in io.scalaland.chimney.fixtures.products.Inherited.Source that might be used as accessors for value (e.g. value), the constructor argument/setter in io.scalaland.chimney.fixtures.products.Inherited.Target. Consider using .enableInheritedAccessors.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("(new Source).intoPartial[Target].transform").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Inherited.Source to io.scalaland.chimney.fixtures.products.Inherited.Target",
        "io.scalaland.chimney.fixtures.products.Inherited.Target",
        "  value: java.lang.String - no accessor named value in source type io.scalaland.chimney.fixtures.products.Inherited.Source",
        "There are inherited definitions in io.scalaland.chimney.fixtures.products.Inherited.Source that might be used as accessors for value (e.g. value), the constructor argument/setter in io.scalaland.chimney.fixtures.products.Inherited.Target. Consider using .enableInheritedAccessors.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should enable using inherited accessors") {
      import products.Inherited.*

      val expected = Target("value")

      val result = (new Source).intoPartial[Target].enableInheritedAccessors.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val cfg = TransformerConfiguration.default.enableInheritedAccessors

        val result2 = (new Source).transformIntoPartial[Target]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should enable using inherited accessors only for a single field when scoped using .withTargetFlag(_.field)") {
      import products.Inherited.*

      val expected = Target("value")

      val result = (new Source).intoPartial[Target].withTargetFlag(_.value).enableInheritedAccessors.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("flag .disableInheritedAccessors") {

    test("should disable globally enabled .enableInheritedAccessors") {
      import products.Inherited.*

      @unused implicit val cfg = TransformerConfiguration.default.enableInheritedAccessors

      compileErrors("(new Source).intoPartial[Target].disableInheritedAccessors.transform").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Inherited.Source to io.scalaland.chimney.fixtures.products.Inherited.Target",
        "io.scalaland.chimney.fixtures.products.Inherited.Target",
        "  value: java.lang.String - no accessor named value in source type io.scalaland.chimney.fixtures.products.Inherited.Source",
        "There are inherited definitions in io.scalaland.chimney.fixtures.products.Inherited.Source that might be used as accessors for value (e.g. value), the constructor argument/setter in io.scalaland.chimney.fixtures.products.Inherited.Target. Consider using .enableInheritedAccessors.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

  group("flag .enableMethodAccessors") {

    test("should be disabled by default") {
      import products.Accessors.*

      compileErrors("Source(10).transformIntoPartial[Target2]").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Accessors.Source to io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "  z: scala.Double - no accessor named z in source type io.scalaland.chimney.fixtures.products.Accessors.Source",
        "There are methods in io.scalaland.chimney.fixtures.products.Accessors.Source that might be used as accessors for z (e.g. z), the constructor argument/setter in io.scalaland.chimney.fixtures.products.Accessors.Target2. Consider using .enableMethodAccessors.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("Source(10).intoPartial[Target2].transform").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Accessors.Source to io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "  z: scala.Double - no accessor named z in source type io.scalaland.chimney.fixtures.products.Accessors.Source",
        "There are methods in io.scalaland.chimney.fixtures.products.Accessors.Source that might be used as accessors for z (e.g. z), the constructor argument/setter in io.scalaland.chimney.fixtures.products.Accessors.Target2. Consider using .enableMethodAccessors.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should not be needed if all target fields with default values have their values provided in other way") {
      import products.Accessors.*

      val expected = Target2(10, 20.0)

      val result = Source(10).intoPartial[Target2].withFieldConst(_.z, 20.0).transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Source(10).intoPartial[Target2].withFieldComputed(_.z, a => a.z * 2).transform
      result2.asOption ==> Some(expected)
      result2.asEither ==> Right(expected)
      result2.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should not be needed to providing values from vals defined in body") {
      import products.Accessors.*

      val expected = Target(10, "10")

      val result = Source(10).transformIntoPartial[Target]
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Source(10).intoPartial[Target].transform
      result2.asOption ==> Some(expected)
      result2.asEither ==> Right(expected)
      result2.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should enable using accessors in flat transformers") {
      import products.Accessors.*

      val expected = Target2(10, 10.0)

      val result = Source(10).intoPartial[Target2].enableMethodAccessors.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default.enableMethodAccessors

        val result2 = Source(10).transformIntoPartial[Target2]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = Source(10).intoPartial[Target2].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should enable using accessors in nested transformers") {
      import products.Accessors.*

      val expected = Nested(Target2(10, 10.0))

      val result = Nested(Source(10)).intoPartial[Nested[Target2]].enableMethodAccessors.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default.enableMethodAccessors

        val result2 = Nested(Source(10)).transformIntoPartial[Nested[Target2]]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = Nested(Source(10)).intoPartial[Nested[Target2]].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should ignore accessors if other setting provides it or source field exists") {
      import products.Accessors.*

      val expected = Target2(10, 20.0)

      val result =
        Source(10).intoPartial[Target2].enableMethodAccessors.withFieldConst(_.z, 20.0).transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Source(10).intoPartial[Target2].enableMethodAccessors.withFieldComputed(_.z, a => a.z * 2).transform
      result2.asOption ==> Some(expected)
      result2.asEither ==> Right(expected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val config = TransformerConfiguration.default.enableMethodAccessors

        val result3 = Source(10).intoPartial[Target2].withFieldConst(_.z, 20.0).transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty

        val result4 = Source(10).intoPartial[Target2].withFieldComputed(_.z, a => a.z * 2).transform
        result4.asOption ==> Some(expected)
        result4.asEither ==> Right(expected)
        result4.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should enable using accessors only for a single field when scoped using .withTargetFlag(_.field)") {
      import products.Accessors.*

      val expected = Target2(10, 10.0)

      val result = Source(10).intoPartial[Target2].withTargetFlag(_.z).enableMethodAccessors.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("flag .disableMethodAccessors") {

    test("should disable globally enabled .enableDefaultValues") {
      import products.Accessors.*

      @unused implicit val config = TransformerConfiguration.default.enableMethodAccessors

      compileErrors("""Source(10).into[Target2].disableMethodAccessors.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Accessors.Source to io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "  z: scala.Double - no accessor named z in source type io.scalaland.chimney.fixtures.products.Accessors.Source",
        "There are methods in io.scalaland.chimney.fixtures.products.Accessors.Source that might be used as accessors for z (e.g. z), the constructor argument/setter in io.scalaland.chimney.fixtures.products.Accessors.Target2. Consider using .enableMethodAccessors.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

  group("flag .enableNonAnyValWrappers") {

    test("should be disabled by default") {
      import fixtures.nestedpath.NestedProduct
      import fixtures.valuetypes.*

      compileErrors("""UserWithName("value").transformIntoPartial[String]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.valuetypes.UserWithName to java.lang.String",
        "java.lang.String",
        "  derivation from userwithname: io.scalaland.chimney.fixtures.valuetypes.UserWithName to java.lang.String is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      compileErrors(""""value".transformIntoPartial[UserWithName]""").check(
        "Chimney can't derive transformation from java.lang.String to io.scalaland.chimney.fixtures.valuetypes.UserWithName",
        "io.scalaland.chimney.fixtures.valuetypes.UserWithName",
        "  id: java.lang.String - no accessor named id in source type java.lang.String",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      compileErrors("""UserWithName("value").transformIntoPartial[NestedProduct[String]]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.valuetypes.UserWithName to io.scalaland.chimney.fixtures.nestedpath.NestedProduct[java.lang.String]",
        "io.scalaland.chimney.fixtures.nestedpath.NestedProduct[java.lang.String]",
        "  value: java.lang.String - no accessor named value in source type io.scalaland.chimney.fixtures.valuetypes.UserWithName",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should allow unwrapping non-AnyVal wrappers") {
      import fixtures.valuetypes.*

      val expected = "value"

      val result = UserWithName("value").intoPartial[String].enableNonAnyValWrappers.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers

        val result2 = UserWithName("value").transformIntoPartial[String]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = UserWithName("value").intoPartial[String].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should allow wrapping non-AnyVal wrappers") {
      import fixtures.valuetypes.*

      val expected = UserWithName("value")

      val result = "value".intoPartial[UserWithName].enableNonAnyValWrappers.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers

        val result2 = "value".transformIntoPartial[UserWithName]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = "value".intoPartial[UserWithName].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should allow rewrapping non-AnyVal wrappers") {
      import fixtures.nestedpath.NestedProduct
      import fixtures.valuetypes.*

      val expected = NestedProduct[String]("value")

      val result = UserWithName("value").intoPartial[NestedProduct[String]].enableNonAnyValWrappers.transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      locally {
        implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers

        val result2 = UserWithName("value").transformIntoPartial[NestedProduct[String]]
        result2.asOption ==> Some(expected)
        result2.asEither ==> Right(expected)
        result2.asErrorPathMessageStrings ==> Iterable.empty

        val result3 = UserWithName("value").intoPartial[NestedProduct[String]].transform
        result3.asOption ==> Some(expected)
        result3.asEither ==> Right(expected)
        result3.asErrorPathMessageStrings ==> Iterable.empty
      }
    }

    test("should allow (re)wrapping only for a single field when scoped using .withTargetFlag(_.field)") {
      import fixtures.nestedpath.NestedProduct
      import fixtures.valuetypes.*

      val expected = NestedProduct(NestedProduct[String]("value"))

      val result = NestedProduct(UserWithName("value"))
        .intoPartial[NestedProduct[NestedProduct[String]]]
        .withTargetFlag(_.value)
        .enableNonAnyValWrappers
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("flag .disableNonAnyValWrappers") {

    test("should disable globally enabled .enableNonAnyValWrappers") {
      import fixtures.nestedpath.NestedProduct
      import fixtures.valuetypes.*

      @unused implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers

      compileErrors("""UserWithName("value").intoPartial[String].disableNonAnyValWrappers.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.valuetypes.UserWithName to java.lang.String",
        "java.lang.String",
        "  derivation from userwithname: io.scalaland.chimney.fixtures.valuetypes.UserWithName to java.lang.String is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      compileErrors(""""value".intoPartial[UserWithName].disableNonAnyValWrappers.transform""").check(
        "Chimney can't derive transformation from java.lang.String to io.scalaland.chimney.fixtures.valuetypes.UserWithName",
        "io.scalaland.chimney.fixtures.valuetypes.UserWithName",
        "  id: java.lang.String - no accessor named id in source type java.lang.String",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      compileErrors("""UserWithName("value").intoPartial[NestedProduct[String]].disableNonAnyValWrappers.transform""")
        .check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.valuetypes.UserWithName to io.scalaland.chimney.fixtures.nestedpath.NestedProduct[java.lang.String]",
          "io.scalaland.chimney.fixtures.nestedpath.NestedProduct[java.lang.String]",
          "  value: java.lang.String - no accessor named value in source type io.scalaland.chimney.fixtures.valuetypes.UserWithName",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
    }
  }

  group("flag .enableCustomFieldNameComparison") {
    case class Foo(Baz: Foo.Baz, A: Int)
    object Foo {
      case class Baz(S: String)
    }

    case class Bar(baz: Bar.Baz, a: Int)
    object Bar {
      case class Baz(s: String)
    }

    test("should be disabled by default") {

      compileErrors("""Foo(Foo.Baz("test"), 1024).transformIntoPartial[Bar]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerProductSpec.Foo to io.scalaland.chimney.PartialTransformerProductSpec.Bar",
        "io.scalaland.chimney.PartialTransformerProductSpec.Bar",
        "  baz: io.scalaland.chimney.PartialTransformerProductSpec.Bar.Baz - no accessor named baz in source type io.scalaland.chimney.PartialTransformerProductSpec.Foo",
        "  a: scala.Int - no accessor named a in source type io.scalaland.chimney.PartialTransformerProductSpec.Foo",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""Foo(Foo.Baz("test"), 1024).intoPartial[Bar].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerProductSpec.Foo to io.scalaland.chimney.PartialTransformerProductSpec.Bar",
        "io.scalaland.chimney.PartialTransformerProductSpec.Bar",
        "  baz: io.scalaland.chimney.PartialTransformerProductSpec.Bar.Baz - no accessor named baz in source type io.scalaland.chimney.PartialTransformerProductSpec.Foo",
        "  a: scala.Int - no accessor named a in source type io.scalaland.chimney.PartialTransformerProductSpec.Foo",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""Bar(Bar.Baz("test"), 1024).transformIntoPartial[Foo]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerProductSpec.Bar to io.scalaland.chimney.PartialTransformerProductSpec.Foo",
        "io.scalaland.chimney.PartialTransformerProductSpec.Foo",
        "  Baz: io.scalaland.chimney.PartialTransformerProductSpec.Foo.Baz - no accessor named Baz in source type io.scalaland.chimney.PartialTransformerProductSpec.Bar",
        "  A: scala.Int - no accessor named A in source type io.scalaland.chimney.PartialTransformerProductSpec.Bar",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""Bar(Bar.Baz("test"), 1024).intoPartial[Foo].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerProductSpec.Bar to io.scalaland.chimney.PartialTransformerProductSpec.Foo",
        "io.scalaland.chimney.PartialTransformerProductSpec.Foo",
        "  Baz: io.scalaland.chimney.PartialTransformerProductSpec.Foo.Baz - no accessor named Baz in source type io.scalaland.chimney.PartialTransformerProductSpec.Bar",
        "  A: scala.Int - no accessor named A in source type io.scalaland.chimney.PartialTransformerProductSpec.Bar",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should inform user if and why the setting cannot be read") {
      @unused object BadNameComparison extends TransformedNamesComparison {

        def namesMatch(fromName: String, toName: String): Boolean = fromName.equalsIgnoreCase(toName)
      }

      compileErrors(
        """Foo(Foo.Baz("test"), 1024).intoPartial[Bar].enableCustomFieldNameComparison(BadNameComparison).transform"""
      )
        .check(
          "Invalid TransformerNamesComparison type - only (case) objects are allowed, and only the ones defined as top-level or in top-level objects, got: io.scalaland.chimney.PartialTransformerProductSpec.BadNameComparison!!!"
        )
    }

    test("should inform user when the matcher they provided results in ambiguities") {
      case class FooAmbiguous(baz: FooAmbiguous.Baz, a: Int, A: String)
      object FooAmbiguous {
        case class Baz(s: String, S: Int)
      }

      FooAmbiguous(FooAmbiguous.Baz("test", 10), 100, "test2").transformIntoPartial[Bar].asOption ==> Some(
        Bar(Bar.Baz("test"), 100)
      )
      FooAmbiguous(FooAmbiguous.Baz("test", 10), 100, "test2").intoPartial[Bar].transform.asOption ==> Some(
        Bar(Bar.Baz("test"), 100)
      )

      compileErrors(
        """
        FooAmbiguous(FooAmbiguous.Baz("test", 10), 100, "test2").intoPartial[Bar]
          .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
          .transform
        """
      )
        .check(
          "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerProductSpec.FooAmbiguous to io.scalaland.chimney.PartialTransformerProductSpec.Bar",
          "io.scalaland.chimney.PartialTransformerProductSpec.Bar",
          "  baz: io.scalaland.chimney.PartialTransformerProductSpec.Bar.Baz - can't derive transformation from baz: io.scalaland.chimney.PartialTransformerProductSpec.FooAmbiguous.Baz in source type io.scalaland.chimney.PartialTransformerProductSpec.FooAmbiguous",
          "  field a: io.scalaland.chimney.PartialTransformerProductSpec.Bar has ambiguous matches in io.scalaland.chimney.PartialTransformerProductSpec.FooAmbiguous: A, a",
          "io.scalaland.chimney.PartialTransformerProductSpec.Bar.Baz (transforming from: baz into: baz)",
          "  field s: io.scalaland.chimney.PartialTransformerProductSpec.Bar.Baz has ambiguous matches in io.scalaland.chimney.PartialTransformerProductSpec.FooAmbiguous.Baz: S, s",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
    }

    test("should allow fields to be matched using user-provided predicate") {

      val result = Foo(Foo.Baz("test"), 1024)
        .intoPartial[Bar]
        .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
      result.asOption ==> Some(Bar(Bar.Baz("test"), 1024))
      result.asEither ==> Right(Bar(Bar.Baz("test"), 1024))
      result.asErrorPathMessageStrings ==> Iterable()

      val result2 = Bar(Bar.Baz("test"), 1024)
        .intoPartial[Foo]
        .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
      result2.asOption ==> Some(Foo(Foo.Baz("test"), 1024))
      result2.asEither ==> Right(Foo(Foo.Baz("test"), 1024))
      result2.asErrorPathMessageStrings ==> Iterable()

      locally {
        implicit val config = TransformerConfiguration.default.enableCustomFieldNameComparison(
          TransformedNamesComparison.CaseInsensitiveEquality
        )

        val result3 = Foo(Foo.Baz("test"), 1024).transformIntoPartial[Bar]
        result3.asOption ==> Some(Bar(Bar.Baz("test"), 1024))
        result3.asEither ==> Right(Bar(Bar.Baz("test"), 1024))
        result3.asErrorPathMessageStrings ==> Iterable()
        val result4 = Foo(Foo.Baz("test"), 1024).intoPartial[Bar].transform
        result4.asOption ==> Some(Bar(Bar.Baz("test"), 1024))
        result4.asEither ==> Right(Bar(Bar.Baz("test"), 1024))
        result4.asErrorPathMessageStrings ==> Iterable()

        val result5 = Bar(Bar.Baz("test"), 1024).transformIntoPartial[Foo]
        result5.asOption ==> Some(Foo(Foo.Baz("test"), 1024))
        result5.asEither ==> Right(Foo(Foo.Baz("test"), 1024))
        result5.asErrorPathMessageStrings ==> Iterable()
        val result6 = Bar(Bar.Baz("test"), 1024).intoPartial[Foo].transform
        result6.asOption ==> Some(Foo(Foo.Baz("test"), 1024))
        result6.asEither ==> Right(Foo(Foo.Baz("test"), 1024))
        result6.asErrorPathMessageStrings ==> Iterable()
      }
    }

    test("should allow fields to be matched using user-provided predicate") {

      val result = Foo(Foo.Baz("test"), 1024)
        .intoPartial[Bar]
        .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
      result.asOption ==> Some(Bar(Bar.Baz("test"), 1024))
      result.asEither ==> Right(Bar(Bar.Baz("test"), 1024))
      result.asErrorPathMessageStrings ==> Iterable()

      val result2 = Bar(Bar.Baz("test"), 1024)
        .intoPartial[Foo]
        .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
      result2.asOption ==> Some(Foo(Foo.Baz("test"), 1024))
      result2.asEither ==> Right(Foo(Foo.Baz("test"), 1024))
      result2.asErrorPathMessageStrings ==> Iterable()

      locally {
        implicit val config = TransformerConfiguration.default.enableCustomFieldNameComparison(
          TransformedNamesComparison.CaseInsensitiveEquality
        )

        val result3 = Foo(Foo.Baz("test"), 1024).transformIntoPartial[Bar]
        result3.asOption ==> Some(Bar(Bar.Baz("test"), 1024))
        result3.asEither ==> Right(Bar(Bar.Baz("test"), 1024))
        result3.asErrorPathMessageStrings ==> Iterable()
        val result4 = Foo(Foo.Baz("test"), 1024).intoPartial[Bar].transform
        result4.asOption ==> Some(Bar(Bar.Baz("test"), 1024))
        result4.asEither ==> Right(Bar(Bar.Baz("test"), 1024))
        result4.asErrorPathMessageStrings ==> Iterable()

        val result5 = Bar(Bar.Baz("test"), 1024).transformIntoPartial[Foo]
        result5.asOption ==> Some(Foo(Foo.Baz("test"), 1024))
        result5.asEither ==> Right(Foo(Foo.Baz("test"), 1024))
        result5.asErrorPathMessageStrings ==> Iterable()
        val result6 = Bar(Bar.Baz("test"), 1024).intoPartial[Foo].transform
        result6.asOption ==> Some(Foo(Foo.Baz("test"), 1024))
        result6.asEither ==> Right(Foo(Foo.Baz("test"), 1024))
        result6.asErrorPathMessageStrings ==> Iterable()
      }
    }

    test("should use user-provided predicate only for a single field when scoped using .withTargetFlag(_.field)") {
      import fixtures.nestedpath.NestedProduct

      val result = NestedProduct(Foo(Foo.Baz("test"), 1024))
        .intoPartial[NestedProduct[Bar]]
        .withTargetFlag(_.value)
        .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
      result.asOption ==> Some(NestedProduct(Bar(Bar.Baz("test"), 1024)))
      result.asEither ==> Right(NestedProduct(Bar(Bar.Baz("test"), 1024)))
      result.asErrorPathMessageStrings ==> Iterable()

      val result2 = NestedProduct(Bar(Bar.Baz("test"), 1024))
        .intoPartial[NestedProduct[Foo]]
        .withTargetFlag(_.value)
        .enableCustomFieldNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
      result2.asOption ==> Some(NestedProduct(Foo(Foo.Baz("test"), 1024)))
      result2.asEither ==> Right(NestedProduct(Foo(Foo.Baz("test"), 1024)))
      result2.asErrorPathMessageStrings ==> Iterable()
    }
  }

  group("flag .disableCustomFieldNameComparison") {
    @unused case class Foo(Baz: Foo.Baz, A: Int)
    object Foo {
      case class Baz(S: String)
    }

    @unused case class Bar(baz: Bar.Baz, a: Int)
    object Bar {
      case class Baz(s: String)
    }

    test("should disable globally enabled .enableCustomFieldNameComparison") {
      @unused implicit val config = TransformerConfiguration.default.enableCustomFieldNameComparison(
        TransformedNamesComparison.CaseInsensitiveEquality
      )

      compileErrors("""Foo(Foo.Baz("test"), 1024).intoPartial[Bar].disableCustomFieldNameComparison.transform""")
        .check(
          "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerProductSpec.Foo to io.scalaland.chimney.PartialTransformerProductSpec.Bar",
          "io.scalaland.chimney.PartialTransformerProductSpec.Bar",
          "  baz: io.scalaland.chimney.PartialTransformerProductSpec.Bar.Baz - no accessor named baz in source type io.scalaland.chimney.PartialTransformerProductSpec.Foo",
          "  a: scala.Int - no accessor named a in source type io.scalaland.chimney.PartialTransformerProductSpec.Foo",
          "Consult https://chimney.readthedocs.io for usage examples."
        )

      compileErrors("""Bar(Bar.Baz("test"), 1024).intoPartial[Foo].disableCustomFieldNameComparison.transform""")
        .check(
          "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerProductSpec.Bar to io.scalaland.chimney.PartialTransformerProductSpec.Foo",
          "io.scalaland.chimney.PartialTransformerProductSpec.Foo",
          "  Baz: io.scalaland.chimney.PartialTransformerProductSpec.Foo.Baz - no accessor named Baz in source type io.scalaland.chimney.PartialTransformerProductSpec.Bar",
          "  A: scala.Int - no accessor named A in source type io.scalaland.chimney.PartialTransformerProductSpec.Bar",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
    }
  }

  group("flag .enableUnusedFieldPolicyCheck(policyName)") {

    import products.{Foo, Bar}

    test("should be disabled by default") {
      val result = Foo(10, "unused", (1.0, 2.0)).transformIntoPartial[Bar]
      result.asOption ==> Some(Bar(10, (1.0, 2.0)))
      result.asEither ==> Right(Bar(10, (1.0, 2.0)))
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Foo(10, "unused", (1.0, 2.0)).intoPartial[Bar].transform
      result2.asOption ==> Some(Bar(10, (1.0, 2.0)))
      result2.asEither ==> Right(Bar(10, (1.0, 2.0)))
      result2.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("should fail compilation when policy is violated") {
      compileErrors(
        """Foo(10, "unused", (1.0, 2.0)).intoPartial[Bar].enableUnusedFieldPolicyCheck(FailOnIgnoredSourceVal).transform"""
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Foo to io.scalaland.chimney.fixtures.products.Bar",
        "io.scalaland.chimney.fixtures.products.Bar",
        "  FailOnIgnoredSourceVal policy check failed at _, offenders: y!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      locally {
        @unused implicit val config =
          TransformerConfiguration.default.enableUnusedFieldPolicyCheck(FailOnIgnoredSourceVal)

        compileErrors("""Foo(10, "unused", (1.0, 2.0)).transformIntoPartial[Bar]""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Foo to io.scalaland.chimney.fixtures.products.Bar",
          "io.scalaland.chimney.fixtures.products.Bar",
          "  FailOnIgnoredSourceVal policy check failed at _, offenders: y!",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
      }
    }
  }

  group("flag .disableUnusedFieldPolicyCheck") {

    import products.{Foo, Bar}

    test("should disable globally enabled .enableUnusedFieldPolicyCheck") {
      @unused implicit val config =
        TransformerConfiguration.default.enableUnusedFieldPolicyCheck(FailOnIgnoredSourceVal)

      val result = Foo(10, "unused", (1.0, 2.0)).intoPartial[Bar].disableUnusedFieldPolicyCheck.transform
      result.asOption ==> Some(Bar(10, (1.0, 2.0)))
      result.asEither ==> Right(Bar(10, (1.0, 2.0)))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  // old tests, which could be rewritten into something more structured and better named, but are valuable nonetheless

  group("transform always fails") {

    import trip.*

    test("empty value") {
      val result = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldConstPartial(_.height, partial.Result.fromEmpty)
        .transform

      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result.Errors
          .single(partial.Error.fromEmptyValue)
          .prependErrorPath(partial.PathElement.Const("_.height"))
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "<const for _.height>" -> "empty value"
      )
    }

    test("not defined at") {
      val person = Person("John", 10, 140.0)
      val result = person
        .intoPartial[User]
        .withFieldComputedPartial(
          _.height,
          partial.Result.fromPartialFunction {
            case Person(_, age, _) if age > 18 => 2.0 * age
          }
        )
        .transform

      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result.Errors
          .single(partial.Error.fromNotDefinedAt(person))
          .prependErrorPath(partial.PathElement.Computed("_.height"))
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "<computed for _.height>" -> s"not defined at $person"
      )
    }

    test("custom string errors") {
      val result = Person("John", 10, 140.0)
        .intoPartial[User]
        .withFieldConstPartial(_.height, partial.Result.fromErrorStrings("abc", "def"))
        .transform

      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result
          .Errors(
            partial.Error.fromString("abc"),
            partial.Error.fromString("def")
          )
          .prependErrorPath(partial.PathElement.Const("_.height"))
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "<const for _.height>" -> "abc",
        "<const for _.height>" -> "def"
      )
    }

    test("throwable error") {
      import PartialTransformerProductSpec.MyException
      val result = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldConstPartial(_.height, partial.Result.fromErrorThrowable(MyException))
        .transform

      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result.Errors
          .single(
            partial.Error.fromThrowable(MyException)
          )
          .prependErrorPath(partial.PathElement.Const("_.height"))
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "<const for _.height>" -> "my exception"
      )
    }
  }

  group("partial transform validation") {

    import trip.*

    test("success") {
      val okForm = PersonForm("John", "10", "140")
      val expected = Person("JOHN", 10, 140)

      val result = okForm
        .intoPartial[Person]
        .withFieldComputedPartial(
          _.name,
          pf =>
            if (pf.name.isEmpty) partial.Result.fromEmpty
            else partial.Result.fromValue(pf.name.toUpperCase())
        )
        .withFieldComputed(_.age, _.age.toInt) // must catch exceptions
        .withFieldComputedPartial(
          _.height,
          pf => partial.Result.fromOption(pf.height.parseDouble)
        )
        .transform

      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("failure with error handling") {
      val invalidForm = PersonForm("", "foo", "bar")

      val result = invalidForm
        .intoPartial[Person]
        .withFieldComputedPartial(
          _.name,
          pf =>
            if (pf.name.isEmpty) partial.Result.fromEmpty
            else partial.Result.fromValue(pf.name.toUpperCase())
        )
        .withFieldComputed(_.age, _.age.toInt) // must catch exceptions
        .withFieldComputedPartial(
          _.height,
          pf => partial.Result.fromOption(pf.height.parseDouble)
        )
        .transform

      result.asOption ==> None
      result.asErrorPathMessageStrings ==> Iterable(
        "<computed for _.name>" -> "empty value",
        "<computed for _.age>" -> "For input string: \"foo\"",
        "<computed for _.height>" -> "empty value"
      )
    }
  }

  group("recursive partial transform with nested validation") {

    import trip.*

    @unused // for now
    implicit val personPartialTransformer: PartialTransformer[PersonForm, Person] =
      Transformer
        .definePartial[PersonForm, Person]
        .withFieldComputedPartial(_.age, _.age.parseInt.orStringAsResult("bad age value"))
        .withFieldComputedPartial(
          _.height,
          _.height.parseDouble.orStringAsResult("bad height value")
        )
        .buildTransformer

    test("success") {

      val okTripForm = TripForm("100", List(PersonForm("John", "10", "140"), PersonForm("Caroline", "12", "155")))

      val result = okTripForm
        .intoPartial[Trip]
        .withFieldComputedPartial(_.id, _.tripId.parseInt.orStringAsResult("bad trip id"))
        .transform

      result.asOption ==> Some(Trip(100, Vector(Person("John", 10, 140), Person("Caroline", 12, 155))))
    }

    test("failure with error handling") {

      val badTripForm =
        TripForm("100xyz", List(PersonForm("John", "10", "foo"), PersonForm("Caroline", "bar", "155")))

      val result = badTripForm
        .intoPartial[Trip]
        .withFieldComputedPartial(_.id, _.tripId.parseInt.orStringAsResult("bad trip id"))
        .transform

      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result.Errors(
          partial.Error
            .fromString("bad trip id")
            .prependErrorPath(partial.PathElement.Computed("_.id")),
          partial.Error
            .fromString("bad height value")
            .prependErrorPath(partial.PathElement.Computed("_.height"))
            .unsealErrorPath()
            .prependErrorPath(partial.PathElement.Index(0))
            .prependErrorPath(partial.PathElement.Accessor("people")),
          partial.Error
            .fromString("bad age value")
            .prependErrorPath(partial.PathElement.Computed("_.age"))
            .unsealErrorPath()
            .prependErrorPath(partial.PathElement.Index(1))
            .prependErrorPath(partial.PathElement.Accessor("people"))
        )
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "<computed for _.id>" -> "bad trip id",
        "people(0) => <computed for _.height>" -> "bad height value",
        "people(1) => <computed for _.age>" -> "bad age value"
      )
    }
  }

  group("support scoped transformer configuration passed implicitly") {

    class Source {
      def field1: Int = 100
    }
    case class Target(field1: Int = 200, field2: Option[String] = Some("foo"))

    implicit val transformerConfiguration =
      TransformerConfiguration.default.enableOptionDefaultsToNone.enableMethodAccessors.disableDefaultValues

    test("scoped config only") {

      (new Source).transformIntoPartial[Target].asOption ==> Some(Target(100, None))
      (new Source).intoPartial[Target].transform.asOption ==> Some(Target(100, None))
    }

    test("scoped config overridden by instance flag") {

      (new Source)
        .intoPartial[Target]
        .disableMethodAccessors
        .enableDefaultValues
        .transform
        .asOption ==> Some(Target(200, Some("foo")))

      (new Source)
        .intoPartial[Target]
        .enableDefaultValues
        .transform
        .asOption ==> Some(Target(100, Some("foo")))

      (new Source)
        .intoPartial[Target]
        .disableOptionDefaultsToNone
        .withFieldConst(_.field2, Some("abc"))
        .transform
        .asOption ==> Some(Target(100, Some("abc")))
    }

    test("compile error when optionDefaultsToNone were disabled locally") {

      compileErrors("""(new Source).intoPartial[Target].disableOptionDefaultsToNone.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerProductSpec.Source to io.scalaland.chimney.PartialTransformerProductSpec.Target"
      )
    }
  }

  group("implicit conflict resolution") {

    case class Foo(value: String)
    case class Bar(value: Int)

    implicit val totalInner: Transformer[String, Int] = _.toInt

    implicit val partialInner: PartialTransformer[String, Int] =
      PartialTransformer[String, Int](str => partial.Result.fromCatching(str.toInt).map(_ * 2))

    test("ambiguous error when not resolved") {

      compileErrors("""Foo("100").transformIntoPartial[Bar]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerProductSpec.Foo to io.scalaland.chimney.PartialTransformerProductSpec.Bar",
        "io.scalaland.chimney.PartialTransformerProductSpec.Bar",
        "  value: scala.Int - can't derive transformation from value: java.lang.String in source type io.scalaland.chimney.PartialTransformerProductSpec.Foo",
        "scala.Int (transforming from: value into: value)",
        "  ambiguous implicits while resolving Chimney recursive transformation!",
        "    PartialTransformer[java.lang.String, scala.Int]: partialInner",
        "    Transformer[java.lang.String, scala.Int]: totalInner",
        "  Please eliminate total/partial ambiguity from implicit scope or use enableImplicitConflictResolution/withFieldComputed/withFieldComputedPartial to decide which one should be used.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    group("resolve conflict using total transformer implicit preference") {

      test("using dsl operation") {
        Foo("100")
          .intoPartial[Bar]
          .enableImplicitConflictResolution(PreferTotalTransformer)
          .transform
          .asOption ==> Some(Bar(100))
      }

      test("using scoped configuration") {
        implicit val transformerConfiguration = TransformerConfiguration.default
          .enableImplicitConflictResolution(PreferTotalTransformer)

        Foo("100").transformIntoPartial[Bar].asOption ==> Some(Bar(100))

        test("disabled again should not compile") {
          compileErrors(
            """
            Foo("100").intoPartial[Bar]
              .disableImplicitConflictResolution
              .transform
            """
          ).check(
            "Ambiguous implicits while resolving Chimney recursive transformation"
          )
        }
      }

      test("using .withTargetFlag(_.field)") {
        Foo("100")
          .intoPartial[Bar]
          .withTargetFlag(_.value)
          .enableImplicitConflictResolution(PreferTotalTransformer)
          .transform
          .asOption ==> Some(Bar(100))
      }
    }

    group("resolve conflict using partial transformer implicit preference") {

      test("using dsl operation") {
        Foo("100")
          .intoPartial[Bar]
          .enableImplicitConflictResolution(PreferPartialTransformer)
          .transform
          .asOption ==> Some(Bar(200))
      }

      test("using scoped configuration") {
        implicit val transformerConfiguration = TransformerConfiguration.default
          .enableImplicitConflictResolution(PreferPartialTransformer)

        Foo("100").transformIntoPartial[Bar].asOption ==> Some(Bar(200))

        test("disabled again should not compile") {

          compileErrors(
            """
            Foo("100").intoPartial[Bar]
              .disableImplicitConflictResolution
              .transform
            """
          ).check(
            "Ambiguous implicits while resolving Chimney recursive transformation"
          )
        }
      }

      test("using .withTargetFlag(_.field)") {
        Foo("100")
          .intoPartial[Bar]
          .withTargetFlag(_.value)
          .enableImplicitConflictResolution(PreferPartialTransformer)
          .transform
          .asOption ==> Some(Bar(200))
      }
    }

    test("resolve conflict explicitly using .withFieldComputedPartial") {
      Foo("100")
        .intoPartial[Bar]
        .withFieldComputedPartial(_.value, v => partialInner.transform(v.value))
        .transform
        .asOption ==> Some(Bar(200))
    }

    test("resolve conflict explicitly prioritizing: last wins") {
      Foo("100")
        .intoPartial[Bar]
        .withFieldComputed(_.value, v => totalInner.transform(v.value))
        .withFieldComputedPartial(_.value, v => partialInner.transform(v.value))
        .transform
        .asOption ==> Some(Bar(200))

      Foo("100")
        .intoPartial[Bar]
        .withFieldComputedPartial(_.value, v => partialInner.transform(v.value))
        .withFieldComputed(_.value, v => totalInner.transform(v.value))
        .transform
        .asOption ==> Some(Bar(100))
    }

    test("support deriving partial transformer from pure") {
      case class Foo(str: String)

      case class Bar(str: String, other: String)

      implicit val fooToBar: Transformer[Foo, Bar] =
        Transformer
          .define[Foo, Bar]
          .withFieldConst(_.other, "other")
          .buildTransformer

      val result = Foo("str").transformIntoPartial[Bar]

      result.asOption ==> Some(Bar("str", "other"))
      result.asEither ==> Right(Bar("str", "other"))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }
}
object PartialTransformerProductSpec {
  case object MyException extends Exception("my exception")
}
