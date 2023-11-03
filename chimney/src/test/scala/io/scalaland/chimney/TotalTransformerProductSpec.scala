package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.annotation.unused

class TotalTransformerProductSpec extends ChimneySpec {

  test(
    """not allow transformation from a "subset" of fields into a "superset" of fields when missing values are not provided"""
  ) {
    import products.{Foo, Bar}

    compileErrorsFixed("Bar(3, (3.14, 3.14)).into[Foo].transform").check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Bar to io.scalaland.chimney.fixtures.products.Foo",
      "io.scalaland.chimney.fixtures.products.Foo",
      "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Bar",
      "Consult https://chimney.readthedocs.io for usage examples."
    )

    compileErrorsFixed("Bar(3, (3.14, 3.14)).transformInto[Foo]").check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Bar to io.scalaland.chimney.fixtures.products.Foo",
      "io.scalaland.chimney.fixtures.products.Foo",
      "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Bar",
      "Consult https://chimney.readthedocs.io for usage examples."
    )
  }

  test("""transformation from a "superset" of fields into a "subset" of fields without modifiers""") {
    import products.{Foo, Bar}

    Foo(3, "pi", (3.14, 3.14)).into[Bar].transform ==> Bar(3, (3.14, 3.14))
    Foo(3, "pi", (3.14, 3.14)).transformInto[Bar] ==> Bar(3, (3.14, 3.14))
  }

  test("""transform from a subtype to a non-abstract supertype without modifiers""") {
    CaseBar(100).transformInto[BaseFoo].x ==> 100
    CaseBar(100).into[BaseFoo].transform.x ==> 100
  }

  test("""transform from a subtype to a non-abstract supertype without modifiers""") {
    CaseBar(100).into[BaseFoo].withFieldConst(_.x, 200).transform.x ==> 200
  }

  group("setting .withFieldConst(_.field, value)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      /*compileErrorsFixed("""
          Bar(3, (3.14, 3.14)).into[Foo].withFieldConst(_.y, "pi").withFieldConst(_.z._1, 0.0).transform
         """) check ("Invalid selector expression")*/

      compileErrorsFixed("""
          Bar(3, (3.14, 3.14)).into[Foo].withFieldConst(_.y + "abc", "pi").transform
        """) check ("Invalid selector expression")

      compileErrorsFixed("""
          val haveY = HaveY("")
          Bar(3, (3.14, 3.14)).into[Foo].withFieldConst(cc => haveY.y, "pi").transform
        """) check ("Invalid selector expression")
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}, nestedpath.*

      implicit val cfg = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

      Bar(3, (3.14, 3.14)).into[Foo].withFieldConst(_.y, "pi").transform ==> Foo(3, "pi", (3.14, 3.14))
      Bar(3, (3.14, 3.14)).into[Foo].withFieldConst(cc => cc.y, "pi").transform ==> Foo(3, "pi", (3.14, 3.14))

      NestedProduct(Bar(3, (3.14, 3.14)))
        .into[NestedValueClass[Foo]]
        .withFieldConst(_.value.y, "pi")
        .transform ==> NestedValueClass(Foo(3, "pi", (3.14, 3.14)))
      NestedValueClass(Bar(3, (3.14, 3.14)))
        .into[NestedJavaBean[Foo]]
        .withFieldConst(cc => cc.getValue.y, "pi")
        .transform ==> NestedJavaBean(Foo(3, "pi", (3.14, 3.14)))
      NestedJavaBean(Bar(3, (3.14, 3.14)))
        .into[NestedProduct[Foo]]
        .withFieldConst(cc => cc.value.y, "pi")
        .transform ==> NestedProduct(Foo(3, "pi", (3.14, 3.14)))

      import trip.*

      Person("John", 10, 140).into[User].withFieldConst(_.age, 20).transform ==> User("John", 20, 140)

      NestedProduct(Person("John", 10, 140))
        .into[NestedValueClass[User]]
        .withFieldConst(_.value.age, 20)
        .transform ==> NestedValueClass(User("John", 20, 140))
      NestedValueClass(Person("John", 10, 140))
        .into[NestedJavaBean[User]]
        .withFieldConst(_.getValue.age, 20)
        .transform ==> NestedJavaBean(User("John", 20, 140))
      NestedJavaBean(Person("John", 10, 140))
        .into[NestedProduct[User]]
        .withFieldConst(_.value.age, 20)
        .transform ==> NestedProduct(User("John", 20, 140))
    }
  }

  group("setting .withFieldComputed(_.field, source => value)") {

    test("should not compile when selector is invalid") {
      import products.{Foo, Bar, HaveY}

      /*compileErrorsFixed(
        """
          Bar(3, (3.14, 3.14))
            .into[Foo]
            .withFieldComputed(_.y, _.x.toString)
            .withFieldComputed(_.z._1, _.x.toDouble)
            .transform
          """
      ) check ("Invalid selector expression")*/

      compileErrorsFixed("""
          Bar(3, (3.14, 3.14)).into[Foo].withFieldComputed(_.y + "abc", _.toString).transform
        """) check ("Invalid selector expression")

      compileErrorsFixed("""
          val haveY = HaveY("")
          Bar(3, (3.14, 3.14)).into[Foo].withFieldComputed(cc => haveY.y, _.toString).transform
        """) check ("Invalid selector expression")
    }

    test("should provide a value for selected target case class field when selector is valid") {
      import products.{Foo, Bar}

      Bar(3, (3.14, 3.14)).into[Foo].withFieldComputed(_.y, _.x.toString).transform ==> Foo(3, "3", (3.14, 3.14))
      Bar(3, (3.14, 3.14)).into[Foo].withFieldComputed(cc => cc.y, _.x.toString).transform ==> Foo(
        3,
        "3",
        (3.14, 3.14)
      )

      import trip.*

      Person("John", 10, 140).into[User].withFieldComputed(_.age, _.age * 2).transform ==> User("John", 20, 140)
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

      /*compileErrorsFixed("""
          User(1, "Kuba", Some(28)).into[UserPL].withFieldRenamed(_.age.get, _.wiek.right.get).transform
        """).check(
        "Invalid selector expression"
      )*/

      compileErrorsFixed("""
          User(1, "Kuba", Some(28)).into[UserPL].withFieldRenamed(_.age + "ABC", _.toString).transform
        """).arePresent()

      compileErrorsFixed("""
          val str = "string"
          User(1, "Kuba", Some(28)).into[UserPL].withFieldRenamed(u => str, _.toString).transform
        """).check(
        "Invalid selector expression"
      )
    }

    test(
      "should provide a value to a selected target field from a selected source field when there is no same-named source field"
    ) {
      import products.Renames.*

      User(1, "Kuba", Some(28))
        .into[UserPLStd]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform ==> UserPLStd(1, "Kuba", Some(28))
    }

    test(
      "should provide a value to a selected target field from a selected source field despite an existing same-named source field"
    ) {
      import products.Renames.*

      User2ID(1, "Kuba", Some(28), 666)
        .into[User]
        .withFieldRenamed(_.extraID, _.id)
        .transform ==> User(666, "Kuba", Some(28))
    }

    test("should not compile if renamed value change type but an there is no transformer available") {
      import products.Renames.*

      compileErrorsFixed(
        """
          User(1, "Kuba", Some(28))
            .into[UserPL]
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

      User(1, "Kuba", Some(28))
        .into[UserPL]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform ==> UserPL(1, "Kuba", Right(28))
      User(1, "Kuba", None)
        .into[UserPL]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform ==> UserPL(1, "Kuba", Left(()))
    }
  }

  group("flag .enableDefaultValues") {

    test("should be disabled by default") {
      import products.Defaults.*

      compileErrorsFixed("""Source(1, "yy", 1.0).transformInto[Target]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target",
        "io.scalaland.chimney.fixtures.products.Defaults.Target",
        "x: scala.Int - no accessor named x in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrorsFixed("""Source(1, "yy", 1.0).into[Target].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target",
        "io.scalaland.chimney.fixtures.products.Defaults.Target",
        "x: scala.Int - no accessor named x in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should not be needed if all target fields with default values have their values provided in other way") {
      import products.Defaults.*

      Source(1, "yy", 1.0)
        .into[Target]
        .withFieldConst(_.x, 30)
        .withFieldComputed(_.y, _.yy + "2")
        .transform ==> Target(30, "yy2", 1.0)
    }

    test("should enable using default values when no source value can be resolved in flat transformation") {
      import products.Defaults.*

      Source(1, "yy", 1.0).into[Target].enableDefaultValues.transform ==> Target(10, "y", 1.0)

      locally {
        implicit val config = TransformerConfiguration.default.enableDefaultValues

        Source(1, "yy", 1.0).transformInto[Target] ==> Target(10, "y", 1.0)
        Source(1, "yy", 1.0).into[Target].transform ==> Target(10, "y", 1.0)
      }
    }

    test("should enable using default values when no source value can be resolved in nested transformation") {
      import products.Defaults.*

      Nested(Source(1, "yy", 1.0)).into[Nested[Target]].enableDefaultValues.transform ==> Nested(Target(10, "y", 1.0))

      locally {
        implicit val config = TransformerConfiguration.default.enableDefaultValues

        Nested(Source(1, "yy", 1.0)).transformInto[Nested[Target]] ==> Nested(Target(10, "y", 1.0))
        Nested(Source(1, "yy", 1.0)).into[Nested[Target]].transform ==> Nested(Target(10, "y", 1.0))
      }
    }

    test("should ignore default value if other setting provides it or source field exists") {
      import products.Defaults.*

      Source(1, "yy", 1.0)
        .into[Target]
        .enableDefaultValues
        .withFieldConst(_.x, 30)
        .withFieldComputed(_.y, _.yy + "2")
        .transform ==> Target(30, "yy2", 1.0)

      locally {
        implicit val config = TransformerConfiguration.default.enableDefaultValues

        Source(1, "yy", 1.0)
          .into[Target]
          .withFieldConst(_.x, 30)
          .withFieldComputed(_.y, _.yy + "2")
          .transform ==> Target(30, "yy2", 1.0)
      }
    }

    test(
      "should ignore default value and fail compilation if source fields with different type but no Transformer exists"
    ) {
      import products.Defaults.*

      compileErrorsFixed("""Source(1, "yy", 1.0).into[Target2].enableDefaultValues.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target2",
        "io.scalaland.chimney.fixtures.products.Defaults.Target2",
        "xx: scala.Long - can't derive transformation from xx: scala.Int in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      locally {
        @unused implicit val config = TransformerConfiguration.default.enableDefaultValues

        compileErrorsFixed("""Source(1, "yy", 1.0).transformInto[Target2]""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Defaults.Source to io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "io.scalaland.chimney.fixtures.products.Defaults.Target2",
          "xx: scala.Long - can't derive transformation from xx: scala.Int in source type io.scalaland.chimney.fixtures.products.Defaults.Source",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
        compileErrorsFixed("""Source(1, "yy", 1.0).into[Target2].transform""").check(
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

      Source(1, "yy", 1.0)
        .into[Target2]
        .enableDefaultValues
        .transform ==> Target2(1L, "yy", 1.0)

      locally {
        implicit val config = TransformerConfiguration.default.enableDefaultValues

        Source(1, "yy", 1.0).transformInto[Target2] ==> Target2(1L, "yy", 1.0)
        Source(1, "yy", 1.0).into[Target2].transform ==> Target2(1L, "yy", 1.0)
      }
    }
  }

  group("flag .disableDefaultValues") {

    test("should disable globally enabled .enableDefaultValues") {
      import products.Defaults.*

      @unused implicit val config = TransformerConfiguration.default.enableDefaultValues

      compileErrorsFixed("""Source(1, "yy", 1.0).into[Target].disableDefaultValues.transform""").check(
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

      compileErrorsFixed("(new Source).transformInto[Target]").check(
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

      (new Source).into[Target].enableInheritedAccessors.transform ==> Target("value")

      locally {
        implicit val cfg = TransformerConfiguration.default.enableInheritedAccessors

        (new Source).transformInto[Target] ==> Target("value")
      }
    }
  }

  group("flag .disableInheritedAccessors") {

    test("should disable globally enabled .enableInheritedAccessors") {
      import products.Inherited.*

      @unused implicit val cfg = TransformerConfiguration.default.enableInheritedAccessors

      compileErrorsFixed("(new Source).into[Target].disableInheritedAccessors.transform").check(
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

      compileErrorsFixed("Source(10).transformInto[Target2]").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Accessors.Source to io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "z: scala.Double - no accessor named z in source type io.scalaland.chimney.fixtures.products.Accessors.Source",
        "There are methods in io.scalaland.chimney.fixtures.products.Accessors.Source that might be used as accessors for `z` fields in io.scalaland.chimney.fixtures.products.Accessors.Target2. Consider using `.enableMethodAccessors`.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrorsFixed("Source(10).into[Target2].transform").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.products.Accessors.Source to io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "io.scalaland.chimney.fixtures.products.Accessors.Target2",
        "z: scala.Double - no accessor named z in source type io.scalaland.chimney.fixtures.products.Accessors.Source",
        "There are methods in io.scalaland.chimney.fixtures.products.Accessors.Source that might be used as accessors for `z` fields in io.scalaland.chimney.fixtures.products.Accessors.Target2. Consider using `.enableMethodAccessors`.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should not be needed if all target fields with default values have their values provided in other way") {
      import products.Accessors.*

      Source(10).into[Target2].withFieldConst(_.z, 20.0).transform ==> Target2(10, 20.0)
      Source(10).into[Target2].withFieldComputed(_.z, a => a.z * 3).transform ==> Target2(10, 30.0)
    }

    test("should not be needed to providing values from vals defined in body") {
      import products.Accessors.*

      Source(10).transformInto[Target] ==> Target(10, "10")
      Source(10).into[Target].transform ==> Target(10, "10")
    }

    test("should enable using accessors in flat transformers") {
      import products.Accessors.*

      Source(10).into[Target2].enableMethodAccessors.transform ==> Target2(10, 10.0)

      locally {
        implicit val config = TransformerConfiguration.default.enableMethodAccessors

        Source(10).transformInto[Target2] ==> Target2(10, 10.0)
        Source(10).into[Target2].transform ==> Target2(10, 10.0)
      }
    }

    test("should enable using accessors in nested transformers") {
      import products.Accessors.*

      Nested(Source(10)).into[Nested[Target2]].enableMethodAccessors.transform ==> Nested(Target2(10, 10.0))

      locally {
        implicit val config = TransformerConfiguration.default.enableMethodAccessors

        Nested(Source(10)).transformInto[Nested[Target2]] ==> Nested(Target2(10, 10.0))
        Nested(Source(10)).into[Nested[Target2]].transform ==> Nested(Target2(10, 10.0))
      }
    }

    test("should ignore accessors if other setting provides it or source field exists") {
      import products.Accessors.*

      Source(10).into[Target2].enableMethodAccessors.withFieldConst(_.z, 20.0).transform ==> Target2(10, 20.0)
      Source(10).into[Target2].enableMethodAccessors.withFieldComputed(_.z, a => a.z * 3).transform ==> Target2(
        10,
        30.0
      )

      locally {
        implicit val config = TransformerConfiguration.default.enableMethodAccessors

        Source(10).into[Target2].withFieldConst(_.z, 20.0).transform ==> Target2(10, 20.0)
        Source(10).into[Target2].withFieldComputed(_.z, a => a.z * 3).transform ==> Target2(10, 30.0)
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

  group("support using method calls to fill values from target type") {
    case class Foobar(param: String) {
      val valField: String = "valField"
      lazy val lazyValField: String = "lazyValField"

      def method1: String = "method1"

      def method2: String = "method2"

      def method3: String = "method3"

      def method5: String = "method5"

      def method4: String = "method4"

      protected def protect: String = "protect"

      private[chimney] def priv: String = "priv"
    }

    case class Foobar2(param: String, valField: String, lazyValField: String)
    case class Foobar3(param: String, valField: String, lazyValField: String, method1: String)

    test("val and lazy vals work") {
      Foobar("param").into[Foobar2].transform ==> Foobar2("param", "valField", "lazyValField")
    }

    test("works with rename") {
      case class FooBar4(p: String, v: String, lv: String, m: String)

      val res = Foobar("param")
        .into[FooBar4]
        .withFieldRenamed(_.param, _.p)
        .withFieldRenamed(_.valField, _.v)
        .withFieldRenamed(_.lazyValField, _.lv)
        .withFieldRenamed(_.method1, _.m)
        .enableMethodAccessors
        .transform

      res ==> FooBar4(p = "param", v = "valField", lv = "lazyValField", m = "method1")
    }

    test("method is disabled by default") {
      @unused case class Foobar5(
          param: String,
          valField: String,
          lazyValField: String,
          method1: String,
          method2: String,
          method3: String,
          method4: String,
          method5: String
      )
      compileErrorsFixed("""Foobar("param").into[Foobar5].transform""").check(
        "method1: java.lang.String - no accessor named method1 in source type io.scalaland.chimney.TotalTransformerProductSpec.Foobar",
        "method2: java.lang.String - no accessor named method2 in source type io.scalaland.chimney.TotalTransformerProductSpec.Foobar",
        "method3: java.lang.String - no accessor named method3 in source type io.scalaland.chimney.TotalTransformerProductSpec.Foobar",
        "method4: java.lang.String - no accessor named method4 in source type io.scalaland.chimney.TotalTransformerProductSpec.Foobar",
        "method5: java.lang.String - no accessor named method5 in source type io.scalaland.chimney.TotalTransformerProductSpec.Foobar",
        "There are methods in io.scalaland.chimney.TotalTransformerProductSpec.Foobar that might be used as accessors for `method1`, `method2`, `method3` and 2 other methods fields in io.scalaland.chimney.TotalTransformerProductSpec.Foobar5. Consider using `.enableMethodAccessors`."
      )
    }

    test("works if transform is configured with .enableMethodAccessors") {
      Foobar("param").into[Foobar3].enableMethodAccessors.transform ==> Foobar3(
        param = "param",
        valField = "valField",
        lazyValField = "lazyValField",
        method1 = "method1"
      )
    }

    test("protected and private methods are not considered (even if accessible)") {
      @unused case class Foo2(param: String, protect: String, priv: String)

      compileErrorsFixed("""Foobar("param").into[Foo2].enableMethodAccessors.transform""").check(
        "protect: java.lang.String - no accessor named protect in source type io.scalaland.chimney.TotalTransformerProductSpec.Foobar",
        "priv: java.lang.String - no accessor named priv in source type io.scalaland.chimney.TotalTransformerProductSpec.Foobar"
      )
    }
  }

  group("support polymorphic source/target objects and modifiers") {

    import products.Poly.*

    test("monomorphic source to polymorphic target") {

      monoSource.transformInto[PolyTarget[String]] ==> polyTarget

      def transform[T]: (String => T) => MonoSource => PolyTarget[T] =
        fun => _.into[PolyTarget[T]].withFieldComputed(_.poly, src => fun(src.poly)).transform

      transform[String](identity)(monoSource) ==> polyTarget
    }

    test("polymorphic source to monomorphic target") {

      def transform[T]: PolySource[T] => MonoTarget =
        _.into[MonoTarget].withFieldComputed(_.poly, _.poly.toString).transform

      transform[String](polySource) ==> monoTarget
    }

    test("polymorphic source to polymorphic target") {

      def transform[T]: PolySource[T] => PolyTarget[T] =
        _.transformInto[PolyTarget[T]]

      transform[String](polySource) ==> polyTarget
    }

    test("handle type-inference for polymorphic computation") {

      def fun[T]: PolySource[T] => String = _.poly.toString

      def transform[T]: PolySource[T] => MonoTarget =
        _.into[MonoTarget].withFieldComputed(_.poly, fun).transform

      transform[String](polySource) ==> monoTarget
    }

    test("automatically fill Unit parameters") {
      case class Foo(value: String)

      Foo("test").transformInto[PolyBar.UnitBar] ==> PolyBar("test", ())
      Foo("test").transformInto[PolyBar[Unit]] ==> PolyBar("test", ())
    }
  }

  test("support abstracting over a value in dsl operations") {

    case class Foo(x: String)
    case class Bar(z: Double, y: Int, x: String)

    val partialTransformer = Foo("abc")
      .into[Bar]
      .withFieldComputed(_.y, _.x.length)

    val transformer1 = partialTransformer.withFieldConst(_.z, 1.0)
    val transformer2 = partialTransformer.withFieldComputed(_.z, _.x.length * 2.0)

    transformer1.transform ==> Bar(1.0, 3, "abc")
    transformer2.transform ==> Bar(6.0, 3, "abc")
  }

  test("transform from non-case class to case class") {
    import products.NonCaseDomain.*
    import javabeans.CaseClassNoFlag

    test("support non-case classes inputs") {
      val source = new ClassSource("test-id", "test-name")
      val target = source.transformInto[CaseClassNoFlag]

      target.id ==> source.id
      target.name ==> source.name
    }

    test("support trait inputs") {
      val source: TraitSource = new TraitSourceImpl("test-id", "test-name")
      val target = source.transformInto[CaseClassNoFlag]

      target.id ==> source.id
      target.name ==> source.name
    }
  }

  group("transform between case classes and tuples") {

    case class Foo(field1: Int, field2: Double, field3: String)

    test("in simple case") {
      val expected = (0, 3.14, "pi")

      Foo(0, 3.14, "pi")
        .transformInto[(Int, Double, String)] ==> expected

      (0, 3.14, "pi").transformInto[Foo]
    }

    test("even recursively") {

      case class Bar(foo: Foo, baz: Boolean)

      val expected = ((100, 2.71, "e"), false)

      Bar(Foo(100, 2.71, "e"), baz = false)
        .transformInto[((Int, Double, String), Boolean)] ==> expected

      ((100, 2.71, "e"), true).transformInto[Bar] ==>
        Bar(Foo(100, 2.71, "e"), baz = true)
    }

    test("handle tuple transformation errors") {

      compileErrorsFixed("""
          (0, "test").transformInto[Foo]
        """)
        .check(
          "source tuple scala.Tuple2[scala.Int, java.lang.String] is of arity 2, while target type io.scalaland.chimney.TotalTransformerProductSpec.Foo is of arity 3; they need to be equal!"
        )

      compileErrorsFixed("""
          (10.5, "abc", 6).transformInto[Foo]
        """)
        .check("can't derive transformation")

      compileErrorsFixed("""
          Foo(10, 36.6, "test").transformInto[(Double, String, Int, Float, Boolean)]
        """)
        .check(
          "source tuple io.scalaland.chimney.TotalTransformerProductSpec.Foo is of arity 3, while target type scala.Tuple5[scala.Double, java.lang.String, scala.Int, scala.Float, scala.Boolean] is of arity 5; they need to be equal!"
        )

      compileErrorsFixed("""
          Foo(10, 36.6, "test").transformInto[(Int, Double, Boolean)]
        """)
        .check("can't derive transformation")
    }
  }

  group("support recursive data structures") {

    case class Foo(x: Option[Foo])
    case class Bar(x: Option[Bar])

    test("defined by hand") {
      implicit def fooToBarTransformer: Transformer[Foo, Bar] =
        (foo: Foo) => Bar(foo.x.map(fooToBarTransformer.transform))

      Foo(Some(Foo(None))).transformInto[Bar] ==> Bar(Some(Bar(None)))
    }

    test("generated automatically") {
      implicit def fooToBarTransformer: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar]

      Foo(Some(Foo(None))).transformInto[Bar] ==> Bar(Some(Bar(None)))
    }

    test("support mutual recursion") {
      import MutualRec.*

      implicit def bar1ToBar2Transformer: Transformer[Bar1, Bar2] = Transformer.derive[Bar1, Bar2]

      Bar1(1, Baz(Some(Bar1(2, Baz(None))))).transformInto[Bar2] ==> Bar2(Baz(Some(Bar2(Baz(None)))))
    }
  }

  test("support macro dependent transformers") {

    test("Option[List[A]] -> List[B]") {
      // FIXME: this isn't exactly intuitive :/
      implicit def optListT[A, B](implicit
          underlying: Transformer.AutoDerived[A, B]
      ): Transformer[Option[List[A]], List[B]] =
        _.toList.flatten.map(underlying.transform)

      case class ClassA(a: Option[List[ClassAA]])
      case class ClassB(a: List[ClassBB])
      case class ClassC(a: List[ClassBB], other: String)
      case class ClassD(a: List[ClassBB], other: String)

      case class ClassAA(s: String)

      case class ClassBB(s: String)

      ClassA(None).transformInto[ClassB] ==> ClassB(Nil)

      ClassA(Some(List.empty)).transformInto[ClassB] ==> ClassB(Nil)

      ClassA(Some(List(ClassAA("l")))).transformInto[ClassB] ==> ClassB(List(ClassBB("l")))

      ClassA(Some(List(ClassAA("l")))).into[ClassC].withFieldConst(_.other, "other").transform ==> ClassC(
        List(ClassBB("l")),
        "other"
      )

      implicit val defined: Transformer[ClassA, ClassD] =
        Transformer.define[ClassA, ClassD].withFieldConst(_.other, "another").buildTransformer

      ClassA(Some(List(ClassAA("l")))).transformInto[ClassD] ==> ClassD(List(ClassBB("l")), "another")
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

      (new Source).transformInto[Target] ==> Target(100, None)
      (new Source).into[Target].transform ==> Target(100, None)
    }

    test("scoped config overridden by instance flag") {

      (new Source)
        .into[Target]
        .disableMethodAccessors
        .enableDefaultValues
        .transform ==> Target(200, Some("foo"))

      (new Source)
        .into[Target]
        .enableDefaultValues
        .transform ==> Target(100, Some("foo"))

      (new Source)
        .into[Target]
        .disableOptionDefaultsToNone
        .withFieldConst(_.field2, Some("abc"))
        .transform ==> Target(100, Some("abc"))
    }
  }
}
