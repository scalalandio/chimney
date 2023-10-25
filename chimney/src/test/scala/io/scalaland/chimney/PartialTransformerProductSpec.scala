package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*
import io.scalaland.chimney.utils.OptionUtils.*

import scala.annotation.unused

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

    compileErrorsFixed("Bar(3, (3.14, 3.14)).intoPartial[Foo].transform").check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Bar to io.scalaland.chimney.fixtures.products.Foo",
      "io.scalaland.chimney.fixtures.products.Foo",
      "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Bar",
      "Consult https://chimney.readthedocs.io for usage examples."
    )

    compileErrorsFixed("Bar(3, (3.14, 3.14)).transformIntoPartial[Foo]").check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Bar to io.scalaland.chimney.fixtures.products.Foo",
      "io.scalaland.chimney.fixtures.products.Foo",
      "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Bar",
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

  group("setting .withFieldConst(_.field, value)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      compileErrorsFixed(
        """
          Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.y, "pi").withFieldConst(_.z._1, 0.0).transform
         """
      ) check ("Invalid selector expression")

      compileErrorsFixed("""
          Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.y + "abc", "pi").transform
        """) check ("Invalid selector expression")

      compileErrorsFixed("""
          val haveY = HaveY("")
          Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(cc => haveY.y, "pi").transform
        """) check ("Invalid selector expression")
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}
      val expected = Foo(3, "pi", (3.14, 3.14))

      val result = Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(_.y, "pi").transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldConst(cc => cc.y, "pi").transform
      result2.asOption ==> Some(expected)
      result2.asEither ==> Right(expected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      import trip.*
      val expected2 = User("John", 20, 140)

      val result3 = Person("John", 10, 140).intoPartial[User].withFieldConst(_.age, 20).transform
      result3.asOption ==> Some(expected2)
      result3.asEither ==> Right(expected2)
      result3.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("setting .withFieldConstPartial(_.field, result)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      compileErrorsFixed(
        """
          Bar(3, (3.14, 3.14))
            .intoPartial[Foo]
            .withFieldConstPartial(_.y, partial.Result.fromValue("pi"))
            .withFieldConstPartial(_.z._1, partial.Result.fromValue(0.0))
            .transform
          """
      ) check ("Invalid selector expression")

      compileErrorsFixed(
        """
          Bar(3, (3.14, 3.14))
            .intoPartial[Foo]
            .withFieldConstPartial(_.y + "abc", partial.Result.fromValue("pi"))
            .transform
          """
      ) check ("Invalid selector expression")

      compileErrorsFixed(
        """
          val haveY = HaveY("")
          Bar(3, (3.14, 3.14))
            .intoPartial[Foo]
            .withFieldConstPartial(cc => haveY.y, partial.Result.fromValue("pi"))
            .transform
          """
      ) check ("Invalid selector expression")
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}
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

      import trip.*
      val expected2 = User("John", 20, 140)

      val result3 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldConstPartial(_.age, partial.Result.fromValue(20))
        .transform
      result3.asOption ==> Some(expected2)
      result3.asEither ==> Right(expected2)
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldConstPartial(_.age, partial.Result.fromEmpty)
        .transform

      result4.asOption ==> None
      result4.asEither.isLeft ==> true
      result4.asErrorPathMessageStrings ==> Iterable(
        "age" -> "empty value"
      )
    }
  }

  group("setting .withFieldComputed(_.field, source => value)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      compileErrorsFixed(
        """
          Bar(3, (3.14, 3.14))
            .intoPartial[Foo]
            .withFieldComputed(_.y, _.x.toString)
            .withFieldComputed(_.z._1, _.x.toDouble)
            .transform
          """
      ) check ("Invalid selector expression")

      compileErrorsFixed(
        """
          Bar(3, (3.14, 3.14))
            .intoPartial[Foo]
            .withFieldComputed(_.y + "abc", _.toString)
            .transform
          """
      ) check ("Invalid selector expression")

      compileErrorsFixed(
        """
          val haveY = HaveY("")
          Bar(3, (3.14, 3.14))
            .intoPartial[Foo]
            .withFieldComputed(cc => haveY.y, _.toString)
            .transform
          """
      ) check ("Invalid selector expression")
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}
      val expected = Foo(3, "3", (3.14, 3.14))

      val result = Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(_.y, _.x.toString).transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty

      val result2 = Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(cc => cc.y, _.x.toString).transform
      result2.asOption ==> Some(expected)
      result2.asEither ==> Right(expected)
      result2.asErrorPathMessageStrings ==> Iterable.empty

      import trip.*
      val expected2 = User("John", 20, 140)

      val result3 = Person("John", 10, 140).intoPartial[User].withFieldComputed(_.age, _.age * 2).transform
      result3.asOption ==> Some(expected2)
      result3.asEither ==> Right(expected2)
      result3.asErrorPathMessageStrings ==> Iterable.empty

      val result4 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldComputedPartial(_.age, _ => partial.Result.fromEmpty)
        .transform

      result4.asOption ==> None
      result4.asEither.isLeft ==> true
      result4.asErrorPathMessageStrings ==> Iterable(
        "age" -> "empty value"
      )
    }
  }

  group("setting .withFieldComputedPartial(_.field, source => value)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      compileErrorsFixed(
        """
          Bar(3, (3.14, 3.14))
            .intoPartial[Foo]
            .withFieldComputed(_.y, _.x.toString)
            .withFieldComputed(_.z._1, _.x.toDouble)
            .transform
          """
      ) check ("Invalid selector expression")

      compileErrorsFixed("""
          Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(_.y + "abc", _.toString).transform
        """) check ("Invalid selector expression")

      compileErrorsFixed("""
          val haveY = HaveY("")
          Bar(3, (3.14, 3.14)).intoPartial[Foo].withFieldComputed(cc => haveY.y, _.toString).transform
        """) check ("Invalid selector expression")
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}
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

      import trip.*
      val expected2 = User("John", 20, 140)

      val result3 = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldComputedPartial(_.age, bar => partial.Result.fromValue(bar.age * 2))
        .transform
      result3.asOption ==> Some(expected2)
      result3.asEither ==> Right(expected2)
      result3.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("""setting .withFieldRenamed(_.from, _.to)""") {

    test("should not be enabled by default") {
      import products.Renames.*

      compileErrorsFixed("""User(1, "Kuba", Some(28)).transformInto[UserPL]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Renames.User to io.scalaland.chimney.fixtures.products.Renames.UserPL",
        "io.scalaland.chimney.fixtures.products.Renames.UserPL",
        "imie: java.lang.String - no accessor named imie in source type io.scalaland.chimney.fixtures.products.Renames.User",
        "wiek: scala.util.Either[scala.Unit, scala.Int] - no accessor named wiek in source type io.scalaland.chimney.fixtures.products.Renames.User",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrorsFixed("""User(1, "Kuba", Some(28)).into[UserPL].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Renames.User to io.scalaland.chimney.fixtures.products.Renames.UserPL",
        "io.scalaland.chimney.fixtures.products.Renames.UserPL",
        "imie: java.lang.String - no accessor named imie in source type io.scalaland.chimney.fixtures.products.Renames.User",
        "wiek: scala.util.Either[scala.Unit, scala.Int] - no accessor named wiek in source type io.scalaland.chimney.fixtures.products.Renames.User",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should not compile when selector is invalid") {
      import products.Renames.*

      compileErrorsFixed(
        """
          User(1, "Kuba", Some(28)).intoPartial[UserPL].withFieldRenamed(_.age.get, _.wiek.right.get).transform
        """
      ).check(
        "Invalid selector expression"
      )

      compileErrorsFixed("""
          User(1, "Kuba", Some(28)).intoPartial[UserPL].withFieldRenamed(_.age + "ABC", _.toString).transform
        """).arePresent()

      compileErrorsFixed("""
          val str = "string"
          User(1, "Kuba", Some(28)).intoPartial[UserPL].withFieldRenamed(u => str, _.toString).transform
        """).check(
        "Invalid selector expression"
      )
    }

    test(
      "should provide a value to a selected target field from a selected source field when there is no same-named source field"
    ) {
      import products.Renames.*

      val expected = UserPLStd(1, "Kuba", Some(28))

      val result = User(1, "Kuba", Some(28))
        .intoPartial[UserPLStd]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform
      result.asOption ==> Some(expected)
      result.asEither ==> Right(expected)
      result.asErrorPathMessageStrings ==> Iterable.empty
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

    test("should not compile if renamed value change type but an there is no transformer available") {
      import products.Renames.*

      compileErrorsFixed(
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
        "wiek: scala.util.Either[scala.Unit, scala.Int] - can't derive transformation from wiek: scala.Option[scala.Int] in source type io.scalaland.chimney.fixtures.products.Renames.User",
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
  }

  group("flag .enableDefaultValues") {

    test("should be disabled by default") {
      import products.Defaults.*

      compileErrorsFixed("""Source(1, "yy", 1.0).transformIntoPartial[Target]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target",
        "io.scalaland.chimney.fixtures.products.Defaults.Target",
        "x: scala.Int - no accessor named x in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrorsFixed("""Source(1, "yy", 1.0).intoPartial[Target].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target",
        "io.scalaland.chimney.fixtures.products.Defaults.Target",
        "x: scala.Int - no accessor named x in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
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

      compileErrorsFixed("""Source(1, "yy", 1.0).intoPartial[Target2].enableDefaultValues.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target2",
        "io.scalaland.chimney.fixtures.products.Defaults.Target2",
        "xx: scala.Long - can't derive transformation from xx: scala.Int in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      locally {
        @unused implicit val config = TransformerConfiguration.default.enableDefaultValues

        compileErrorsFixed("""Source(1, "yy", 1.0).transformIntoPartial[Target2]""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "xx: scala.Long - can't derive transformation from xx: scala.Int in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
        compileErrorsFixed("""Source(1, "yy", 1.0).intoPartial[Target2].transform""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "xx: scala.Long - can't derive transformation from xx: scala.Int in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
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
  }

  group("flag .disableDefaultValues") {

    test("should disable globally enabled .enableDefaultValues") {
      import products.Defaults.*

      @unused implicit val config = TransformerConfiguration.default.enableDefaultValues

      compileErrorsFixed("""Source(1, "yy", 1.0).intoPartial[Target].disableDefaultValues.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target",
        "io.scalaland.chimney.fixtures.products.Defaults.Target",
        "x: scala.Int - no accessor named x in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

  group("flag .enableInheritedAccessors") {

    test("should be disabled by default") {
      import products.Inherited.*

      compileErrorsFixed("(new Source).transformIntoPartial[Target]").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Inherited.Source to io.scalaland.chimney.fixtures.products.Inherited.Target",
        "io.scalaland.chimney.fixtures.products.Inherited.Target",
        "value: java.lang.String - no accessor named value in source type io.scalaland.chimney.fixtures.products.Inherited.Source",
        "There are methods in io.scalaland.chimney.fixtures.products.Inherited.Source that might be used as accessors for `value` fields in io.scalaland.chimney.fixtures.products.Inherited.Target. Consider using `.enableMethodAccessors`.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrorsFixed("(new Source).intoPartial[Target].transform").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Inherited.Source to io.scalaland.chimney.fixtures.products.Inherited.Target",
        "io.scalaland.chimney.fixtures.products.Inherited.Target",
        "value: java.lang.String - no accessor named value in source type io.scalaland.chimney.fixtures.products.Inherited.Source",
        "There are methods in io.scalaland.chimney.fixtures.products.Inherited.Source that might be used as accessors for `value` fields in io.scalaland.chimney.fixtures.products.Inherited.Target. Consider using `.enableMethodAccessors`.",
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
  }

  group("flag .disableInheritedAccessors") {

    test("should disable globally enabled .enableInheritedAccessors") {
      import products.Inherited.*

      @unused implicit val cfg = TransformerConfiguration.default.enableInheritedAccessors

      compileErrorsFixed("(new Source).intoPartial[Target].disableInheritedAccessors.transform").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Inherited.Source to io.scalaland.chimney.fixtures.products.Inherited.Target",
        "io.scalaland.chimney.fixtures.products.Inherited.Target",
        "value: java.lang.String - no accessor named value in source type io.scalaland.chimney.fixtures.products.Inherited.Source",
        "There are methods in io.scalaland.chimney.fixtures.products.Inherited.Source that might be used as accessors for `value` fields in io.scalaland.chimney.fixtures.products.Inherited.Target. Consider using `.enableMethodAccessors`.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

  group("flag .enableMethodAccessors") {

    test("should be disabled by default") {
      import products.Accessors.*

      compileErrorsFixed("Source(10).transformIntoPartial[Target2]").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Accessors.Source to io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "z: scala.Double - no accessor named z in source type io.scalaland.chimney.fixtures.products.Accessors.Source",
        "There are methods in io.scalaland.chimney.fixtures.products.Accessors.Source that might be used as accessors for `z` fields in io.scalaland.chimney.fixtures.products.Accessors.Target2. Consider using `.enableMethodAccessors`.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrorsFixed("Source(10).intoPartial[Target2].transform").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Accessors.Source to io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "z: scala.Double - no accessor named z in source type io.scalaland.chimney.fixtures.products.Accessors.Source",
        "There are methods in io.scalaland.chimney.fixtures.products.Accessors.Source that might be used as accessors for `z` fields in io.scalaland.chimney.fixtures.products.Accessors.Target2. Consider using `.enableMethodAccessors`.",
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
  }

  group("flag .disableMethodAccessors") {

    test("should disable globally enabled .enableDefaultValues") {
      import products.Accessors.*

      @unused implicit val config = TransformerConfiguration.default.enableMethodAccessors

      compileErrorsFixed("""Source(10).into[Target2].disableMethodAccessors.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Accessors.Source to io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "z: scala.Double - no accessor named z in source type io.scalaland.chimney.fixtures.products.Accessors.Source",
        "There are methods in io.scalaland.chimney.fixtures.products.Accessors.Source that might be used as accessors for `z` fields in io.scalaland.chimney.fixtures.products.Accessors.Target2. Consider using `.enableMethodAccessors`.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

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
          .prependErrorPath(partial.PathElement.Accessor("height"))
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "height" -> "empty value"
      )
    }

    test("not defined at") {
      val person = Person("John", 10, 140)
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
          .prependErrorPath(partial.PathElement.Accessor("height"))
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "height" -> s"not defined at $person"
      )
    }

    test("custom string errors") {
      val result = Person("John", 10, 140)
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
          .prependErrorPath(partial.PathElement.Accessor("height"))
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "height" -> "abc",
        "height" -> "def"
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
          .prependErrorPath(partial.PathElement.Accessor("height"))
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "height" -> "my exception"
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
        "name" -> "empty value",
        "age" -> "For input string: \"foo\"",
        "height" -> "empty value"
      )
    }
  }

  group("recursive partial transform with nested validation") {

    import trip.*

    @unused // for now
    implicit val personPartialTransformer: PartialTransformer[PersonForm, Person] =
      Transformer
        .definePartial[PersonForm, Person]
        .withFieldComputedPartial(_.age, _.age.parseInt.toPartialResultOrString("bad age value"))
        .withFieldComputedPartial(
          _.height,
          _.height.parseDouble.toPartialResultOrString("bad height value")
        )
        .buildTransformer

    test("success") {

      val okTripForm = TripForm("100", List(PersonForm("John", "10", "140"), PersonForm("Caroline", "12", "155")))

      val result = okTripForm
        .intoPartial[Trip]
        .withFieldComputedPartial(_.id, _.tripId.parseInt.toPartialResultOrString("bad trip id"))
        .transform

      result.asOption ==> Some(Trip(100, Vector(Person("John", 10, 140), Person("Caroline", 12, 155))))
    }

    test("failure with error handling") {

      val badTripForm =
        TripForm("100xyz", List(PersonForm("John", "10", "foo"), PersonForm("Caroline", "bar", "155")))

      val result = badTripForm
        .intoPartial[Trip]
        .withFieldComputedPartial(_.id, _.tripId.parseInt.toPartialResultOrString("bad trip id"))
        .transform

      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result.Errors(
          partial.Error
            .fromString("bad trip id")
            .prependErrorPath(partial.PathElement.Accessor("id")),
          partial.Error
            .fromString("bad height value")
            .prependErrorPath(partial.PathElement.Accessor("height"))
            .prependErrorPath(partial.PathElement.Index(0))
            .prependErrorPath(partial.PathElement.Accessor("people")),
          partial.Error
            .fromString("bad age value")
            .prependErrorPath(partial.PathElement.Accessor("age"))
            .prependErrorPath(partial.PathElement.Index(1))
            .prependErrorPath(partial.PathElement.Accessor("people"))
        )
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "id" -> "bad trip id",
        "people(0).height" -> "bad height value",
        "people(1).age" -> "bad age value"
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

      compileErrorsFixed("""
          (new Source).intoPartial[Target].disableOptionDefaultsToNone.transform
        """)
        .check(
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

      compileErrorsFixed(
        """Foo("100").transformIntoPartial[Bar]"""
      ).check(
        "Ambiguous implicits while resolving Chimney recursive transformation",
        "Please eliminate ambiguity from implicit scope or use enableImplicitConflictResolution/withFieldComputed/withFieldComputedPartial to decide which one should be used"
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
          compileErrors("""
               Foo("100").intoPartial[Bar]
                .disableImplicitConflictResolution
                .transform
            """).check(
            "Ambiguous implicits while resolving Chimney recursive transformation"
          )
        }
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

        test("disabled again shoult not compile") {
          compileErrors("""
              Foo("100").intoPartial[Bar]
                .disableImplicitConflictResolution
                .transform
            """).check(
            "Ambiguous implicits while resolving Chimney recursive transformation"
          )
        }
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
