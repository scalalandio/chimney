package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.examples.*
import io.scalaland.chimney.utils.EitherUtils.*
import io.scalaland.chimney.utils.OptionUtils.*
import utest.*

object LiftedTransformerProductSpec extends TestSuite {

  val tests = Tests {

    test("transform case classes with the same fields' number, names and types without modifiers") {

      import trip.*

      test("when F = Option") {
        Person("John", 10, 140).intoF[Option, User].transform ==> Some(User("John", 10, 140))
        Person("John", 10, 140).transformIntoF[Option, User] ==> Some(User("John", 10, 140))
      }

      test("when F = Either[List[String], +*]") {
        Person("John", 10, 140).intoF[Either[Vector[String], +*], User].transform ==> Right(User("John", 10, 140))
        Person("John", 10, 140).transformIntoF[Either[Vector[String], +*], User] ==> Right(User("John", 10, 140))
      }
    }

    test("transform case classes with the same fields' number, names and types without modifiers") {
      import trip.*

      test("when F = Option") {
        Person("John", 10, 140).intoF[Option, User].transform ==> Some(User("John", 10, 140))
        Person("John", 10, 140).transformIntoF[Option, User] ==> Some(User("John", 10, 140))
      }

      test("when F = Either[List[String], +*]") {
        Person("John", 10, 140).intoF[Either[List[String], +*], User].transform ==> Right(User("John", 10, 140))
        Person("John", 10, 140).transformIntoF[Either[List[String], +*], User] ==> Right(User("John", 10, 140))
      }
    }

    test(
      """not allow transformation from a "subset" of fields into a "superset" of fields when missing values are not provided"""
    ) {
      import products.{Foo, Bar}

      test("when F = Option") {
        compileError("Bar(3, (3.14, 3.14)).transformIntoF[Option, Foo]").check(
          "",
          "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Bar to io.scalaland.chimney.examples.products.Foo",
          "io.scalaland.chimney.examples.products.Foo",
          "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.examples.products.Bar",
          "Consult https://scalalandio.github.io/chimney for usage examples."
        )

        compileError("Bar(3, (3.14, 3.14)).intoF[Option, Foo].transform").check(
          "",
          "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Bar to io.scalaland.chimney.examples.products.Foo",
          "io.scalaland.chimney.examples.products.Foo",
          "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.examples.products.Bar",
          "Consult https://scalalandio.github.io/chimney for usage examples."
        )
      }

      test("when F = Either[List[String], +*]") {
        type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

        compileError("Bar(3, (3.14, 3.14)).transformIntoF[EitherList, Foo]").check(
          "",
          "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Bar to io.scalaland.chimney.examples.products.Foo",
          "io.scalaland.chimney.examples.products.Foo",
          "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.examples.products.Bar",
          "Consult https://scalalandio.github.io/chimney for usage examples."
        )

        compileError("Bar(3, (3.14, 3.14)).intoF[EitherList, Foo].transform").check(
          "",
          "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Bar to io.scalaland.chimney.examples.products.Foo",
          "io.scalaland.chimney.examples.products.Foo",
          "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.examples.products.Bar",
          "Consult https://scalalandio.github.io/chimney for usage examples."
        )
      }
    }

    test("""transformation from a "superset" of fields into a "subset" of fields without modifiers""") {
      import products.{Foo, Bar}

      test("when F = Option") {
        Foo(3, "pi", (3.14, 3.14)).intoF[Option, Bar].transform ==> Some(Bar(3, (3.14, 3.14)))
        Foo(3, "pi", (3.14, 3.14)).transformIntoF[Option, Bar] ==> Some(Bar(3, (3.14, 3.14)))
      }

      test("when F = Either[List[String], +*]") {
        Foo(3, "pi", (3.14, 3.14)).intoF[Either[List[String], +*], Bar].transform ==> Right(Bar(3, (3.14, 3.14)))
        Foo(3, "pi", (3.14, 3.14)).transformIntoF[Either[List[String], +*], Bar] ==> Right(Bar(3, (3.14, 3.14)))
      }
    }

    test("""transform from a subtype to a non-abstract supertype without modifiers""") {
      class Foo(val x: Int)
      case class Bar(override val x: Int) extends Foo(x)

      test("when F = Option") {
        val optFoo: Option[Foo] = Bar(100).transformIntoF[Option, Foo]
        optFoo.get.x ==> 100
      }

      test("when F = Either[List[String], +*]") {
        val eitherFoo: Either[Vector[String], Foo] = Bar(200).transformIntoF[Either[Vector[String], +*], Foo]
        eitherFoo.map(_.x) ==> Right(200)
      }
    }

    test("setting .withFieldConst(_.field, value)") {

      test("should not compile when selector is invalid") {
        import products.{Foo, Bar, HaveY}

        test("when F = Option") {
          compileError(
            """
          Bar(3, (3.14, 3.14)).intoF[Option, Foo].withFieldConst(_.y, "pi").withFieldConst(_.z._1, 0.0).transform
         """
          ).check("", "Invalid selector expression")

          compileError("""
          Bar(3, (3.14, 3.14)).intoF[Option, Foo].withFieldConst(_.y + "abc", "pi").transform
        """).check("", "Invalid selector expression")

          compileError("""
          val haveY = HaveY("")
          Bar(3, (3.14, 3.14)).intoF[Option, Foo].withFieldConst(cc => haveY.y, "pi").transform
        """).check("", "Invalid selector expression")
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

          compileError(
            """
          Bar(3, (3.14, 3.14)).intoF[EitherList, Foo].withFieldConst(_.y, "pi").withFieldConst(_.z._1, 0.0).transform
         """
          ).check("", "Invalid selector expression")

          compileError("""
          Bar(3, (3.14, 3.14)).intoF[EitherList, Foo].withFieldConst(_.y + "abc", "pi").transform
        """).check("", "Invalid selector expression")

          compileError("""
          val haveY = HaveY("")
          Bar(3, (3.14, 3.14)).intoF[EitherList, Foo].withFieldConst(cc => haveY.y, "pi").transform
        """).check("", "Invalid selector expression")
        }
      }

      test("should provide a value for selected target case class field when selector is valid") {
        import products.{Foo, Bar}

        import trip.*

        test("when F = Option") {
          Bar(3, (3.14, 3.14)).intoF[Option, Foo].withFieldConst(_.y, "pi").transform ==> Some(
            Foo(3, "pi", (3.14, 3.14))
          )
          Bar(3, (3.14, 3.14)).intoF[Option, Foo].withFieldConst(cc => cc.y, "pi").transform ==> Some(
            Foo(3, "pi", (3.14, 3.14))
          )

          Person("John", 10, 140).intoF[Option, User].withFieldConst(_.age, 20).transform ==> Some(
            User("John", 20, 140)
          )
        }

        test("when F = Either[List[String], +*]") {
          Bar(3, (3.14, 3.14)).intoF[Either[List[String], +*], Foo].withFieldConst(_.y, "pi").transform ==> Right(
            Foo(3, "pi", (3.14, 3.14))
          )
          Bar(3, (3.14, 3.14))
            .intoF[Either[List[String], +*], Foo]
            .withFieldConst(cc => cc.y, "pi")
            .transform ==> Right(
            Foo(3, "pi", (3.14, 3.14))
          )

          Person("John", 10, 140).intoF[Either[List[String], +*], User].withFieldConst(_.age, 20).transform ==> Right(
            User("John", 20, 140)
          )
        }
      }
    }

    test("setting .withFieldConstF(_.field, value)") {

      test("should not compile when selector is invalid") {
        import products.{Foo, Bar, HaveY}

        test("when F = Option") {
          compileError(
            """
            Bar(3, (3.14, 3.14))
              .intoF[Option, Foo]
              .withFieldConstF(_.y, Some("pi"))
              .withFieldConstF(_.z._1, Some(0.0))
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            Bar(3, (3.14, 3.14))
              .intoF[Option, Foo]
              .withFieldConstF(_.y + "abc", Some("pi"))
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            val haveY = HaveY("")
            Bar(3, (3.14, 3.14))
              .intoF[Option, Foo]
              .withFieldConstF(cc => haveY.y, Some("pi"))
              .transform
            """
          ).check("", "Invalid selector expression")
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

          compileError(
            """
            Bar(3, (3.14, 3.14))
              .intoF[EitherList, Foo]
              .withFieldConstF(_.y, Right("pi"))
              .withFieldConstF(_.z._1, Right(0.0))
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            Bar(3, (3.14, 3.14))
              .intoF[EitherList, Foo]
              .withFieldConstF(_.y + "abc", Right("pi"))
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            val haveY = HaveY("")
            Bar(3, (3.14, 3.14))
              .intoF[EitherList, Foo]
              .withFieldConstF(cc => haveY.y, Right("pi"))
              .transform
            """
          ).check("", "Invalid selector expression")
        }
      }

      test("should provide a value for selected target case class field when selector is valid") {
        import products.{Foo, Bar}

        import trip.*

        test("when F = Option") {
          Bar(3, (3.14, 3.14))
            .intoF[Option, Foo]
            .withFieldConstF(_.y, Some("pi"))
            .transform ==> Some(Foo(3, "pi", (3.14, 3.14)))
          Bar(3, (3.14, 3.14))
            .intoF[Option, Foo]
            .withFieldConstF(cc => cc.y, Some("pi"))
            .transform ==> Some(Foo(3, "pi", (3.14, 3.14)))

          Person("John", 10, 140)
            .intoF[Option, User]
            .withFieldConstF(_.age, Some(20))
            .transform ==> Some(User("John", 20, 140))
        }

        test("when F = Either[List[String], +*]") {
          Bar(3, (3.14, 3.14))
            .intoF[Either[List[String], +*], Foo]
            .withFieldConstF(_.y, Right("pi"))
            .transform ==> Right(Foo(3, "pi", (3.14, 3.14)))
          Bar(3, (3.14, 3.14))
            .intoF[Either[List[String], +*], Foo]
            .withFieldConstF(cc => cc.y, Right("pi"))
            .transform ==> Right(Foo(3, "pi", (3.14, 3.14)))

          Person("John", 10, 140)
            .intoF[Either[List[String], +*], User]
            .withFieldConstF(_.age, Right(20))
            .transform ==> Right(User("John", 20, 140))
        }
      }

      // TODO: failed values
    }

    test("setting .withFieldComputed(_.field, source => value)") {

      test("should not compile when selector is invalid") {
        import products.{Foo, Bar, HaveY}

        test("when F = Option") {
          compileError(
            """
            Bar(3, (3.14, 3.14))
              .intoF[Option, Foo]
              .withFieldComputed(_.y, _.x.toString)
              .withFieldComputed(_.z._1, _.x.toDouble)
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            Bar(3, (3.14, 3.14))
              .intoF[Option, Foo]
              .withFieldComputed(_.y + "abc", _.toString)
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            val haveY = HaveY("")
            Bar(3, (3.14, 3.14))
              .intoF[Option, Foo]
              .withFieldComputed(cc => haveY.y, _.toString)
              .transform
            """
          ).check("", "Invalid selector expression")
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

          compileError(
            """
            Bar(3, (3.14, 3.14))
              .intoF[EitherList, Foo]
              .withFieldComputed(_.y, _.x.toString)
              .withFieldComputed(_.z._1, _.x.toDouble)
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            Bar(3, (3.14, 3.14))
              .intoF[EitherList, Foo]
              .withFieldComputed(_.y + "abc", _.toString)
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            val haveY = HaveY("")
            Bar(3, (3.14, 3.14))
              .intoF[EitherList, Foo]
              .withFieldComputed(cc => haveY.y, _.toString)
              .transform
            """
          ).check("", "Invalid selector expression")
        }
      }

      test("should provide a value for selected target case class field when selector is valid") {
        import products.{Foo, Bar}

        test("when F = Option") {
          Bar(3, (3.14, 3.14))
            .intoF[Option, Foo]
            .withFieldComputed(_.y, _.x.toString)
            .transform ==> Some(Foo(3, "3", (3.14, 3.14)))
          Bar(3, (3.14, 3.14))
            .intoF[Option, Foo]
            .withFieldComputed(cc => cc.y, _.x.toString)
            .transform ==> Some(Foo(3, "3", (3.14, 3.14)))

          import trip.*

          Person("John", 10, 140)
            .intoF[Option, User]
            .withFieldComputed(_.age, _.age * 2)
            .transform ==> Some(User("John", 20, 140))
        }

        test("when F = Either[List[String], +*]") {
          Bar(3, (3.14, 3.14))
            .intoF[Either[List[String], +*], Foo]
            .withFieldComputed(_.y, _.x.toString)
            .transform ==> Right(Foo(3, "3", (3.14, 3.14)))
          Bar(3, (3.14, 3.14))
            .intoF[Either[List[String], +*], Foo]
            .withFieldComputed(cc => cc.y, _.x.toString)
            .transform ==> Right(Foo(3, "3", (3.14, 3.14)))

          import trip.*

          Person("John", 10, 140)
            .intoF[Either[List[String], +*], User]
            .withFieldComputed(_.age, _.age * 2)
            .transform ==> Right(User("John", 20, 140))
        }
      }
    }

    test("setting .withFieldComputedF(_.field, source => value)") {

      test("should not compile when selector is invalid") {
        import products.{Foo, Bar, HaveY}

        test("when F = Option") {
          compileError(
            """
            Bar(3, (3.14, 3.14))
              .intoF[Option, Foo]
              .withFieldComputedF(_.y, bar => Some(bar.x.toString))
              .withFieldComputedF(_.z._1, bar => Some(bar.x.toDouble))
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            Bar(3, (3.14, 3.14)).intoF[Option, Foo]
              .withFieldComputedF(_.y + "abc", bar => Some(bar.x.toString))
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            val haveY = HaveY("")
            Bar(3, (3.14, 3.14))
              .intoF[Option, Foo]
              .withFieldComputedF(cc => haveY.y, bar => Some(bar.x.toString))
              .transform
            """
          ).check("", "Invalid selector expression")
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

          compileError(
            """
            Bar(3, (3.14, 3.14))
              .intoF[EitherList, Foo]
              .withFieldComputedF(_.y, bar => Right(bar.x.toString))
              .withFieldComputedF(_.z._1, bar => Right(bar.x.toDouble))
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            Bar(3, (3.14, 3.14))
              .intoF[EitherList, Foo]
              .withFieldComputedF(_.y + "abc", bar => Right(bar.x.toString))
              .transform
            """
          ).check("", "Invalid selector expression")

          compileError(
            """
            val haveY = HaveY("")
            Bar(3, (3.14, 3.14))
              .intoF[EitherList, Foo]
              .withFieldComputedF(cc => haveY.y, bar => Right(bar.x.toString))
              .transform
            """
          ).check("", "Invalid selector expression")
        }
      }

      test("should provide a value for selected target case class field when selector is valid") {
        import products.{Foo, Bar}

        import trip.*

        test("when F = Option") {
          Bar(3, (3.14, 3.14))
            .intoF[Option, Foo]
            .withFieldComputedF(_.y, bar => Some(bar.x.toString))
            .transform ==> Some(Foo(3, "3", (3.14, 3.14)))
          Bar(3, (3.14, 3.14))
            .intoF[Option, Foo]
            .withFieldComputedF(cc => cc.y, bar => Some(bar.x.toString))
            .transform ==> Some(Foo(3, "3", (3.14, 3.14)))

          Person("John", 10, 140)
            .intoF[Option, User]
            .withFieldComputedF(_.age, person => Some(person.age * 2))
            .transform ==> Some(User("John", 20, 140))
        }

        test("when F = Either[List[String], +*]") {
          Bar(3, (3.14, 3.14))
            .intoF[Either[List[String], +*], Foo]
            .withFieldComputedF(_.y, bar => Right(bar.x.toString))
            .transform ==> Right(Foo(3, "3", (3.14, 3.14)))
          Bar(3, (3.14, 3.14))
            .intoF[Either[List[String], +*], Foo]
            .withFieldComputedF(cc => cc.y, bar => Right(bar.x.toString))
            .transform ==> Right(Foo(3, "3", (3.14, 3.14)))

          Person("John", 10, 140)
            .intoF[Either[List[String], +*], User]
            .withFieldComputedF(_.age, person => Right(person.age * 2))
            .transform ==> Right(User("John", 20, 140))
        }
      }

      // TODO: failed values
    }

    test("""setting .withFieldRenamed(_.from, _.to)""") {

      test("should not be enabled by default") {
        import products.Renames.*

        test("when F = Option") {
          compileError("""User(1, "Kuba", Some(28)).transformIntoF[Option, UserPL]""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Renames.User to io.scalaland.chimney.examples.products.Renames.UserPL",
            "io.scalaland.chimney.examples.products.Renames.UserPL",
            "imie: java.lang.String - no accessor named imie in source type io.scalaland.chimney.examples.products.Renames.User",
            "wiek: scala.util.Either - no accessor named wiek in source type io.scalaland.chimney.examples.products.Renames.User",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )

          compileError("""User(1, "Kuba", Some(28)).intoF[Option, UserPL].transform""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Renames.User to io.scalaland.chimney.examples.products.Renames.UserPL",
            "io.scalaland.chimney.examples.products.Renames.UserPL",
            "imie: java.lang.String - no accessor named imie in source type io.scalaland.chimney.examples.products.Renames.User",
            "wiek: scala.util.Either - no accessor named wiek in source type io.scalaland.chimney.examples.products.Renames.User",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

          compileError("""User(1, "Kuba", Some(28)).transformIntoF[EitherList, UserPL]""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Renames.User to io.scalaland.chimney.examples.products.Renames.UserPL",
            "io.scalaland.chimney.examples.products.Renames.UserPL",
            "imie: java.lang.String - no accessor named imie in source type io.scalaland.chimney.examples.products.Renames.User",
            "wiek: scala.util.Either - no accessor named wiek in source type io.scalaland.chimney.examples.products.Renames.User",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )

          compileError("""User(1, "Kuba", Some(28)).intoF[EitherList, UserPL].transform""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Renames.User to io.scalaland.chimney.examples.products.Renames.UserPL",
            "io.scalaland.chimney.examples.products.Renames.UserPL",
            "imie: java.lang.String - no accessor named imie in source type io.scalaland.chimney.examples.products.Renames.User",
            "wiek: scala.util.Either - no accessor named wiek in source type io.scalaland.chimney.examples.products.Renames.User",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }
      }

      test("should not compile when selector is invalid") {
        import products.Renames.*

        test("when F = Option") {
          compileError(
            """
          User(1, "Kuba", Some(28)).intoF[Option, UserPL].withFieldRenamed(_.age.get, _.wiek.right.get).transform
        """
          ).check(
            "",
            "Invalid selector expression"
          )

          compileError(
            """
          User(1, "Kuba", Some(28)).intoF[Option, UserPL].withFieldRenamed(_.age + "ABC", _.toString).transform
        """
          )

          compileError("""
          val str = "string"
          User(1, "Kuba", Some(28)).intoF[Option, UserPL].withFieldRenamed(u => str, _.toString).transform
        """).check(
            "",
            "Invalid selector expression"
          )
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

          compileError(
            """
          User(1, "Kuba", Some(28)).intoF[EitherList, UserPL].withFieldRenamed(_.age.get, _.wiek.right.get).transform
        """
          ).check(
            "",
            "Invalid selector expression"
          )

          compileError(
            """
          User(1, "Kuba", Some(28)).intoF[EitherList, UserPL].withFieldRenamed(_.age + "ABC", _.toString).transform
        """
          )

          compileError(
            """
          val str = "string"
          User(1, "Kuba", Some(28)).intoF[EitherList, UserPL].withFieldRenamed(u => str, _.toString).transform
        """
          ).check(
            "",
            "Invalid selector expression"
          )
        }
      }

      test(
        "should provide a value to a selected target field from a selected source field when there is no same-named source field"
      ) {
        import products.Renames.*

        test("when F = Option") {
          User(1, "Kuba", Some(28))
            .intoF[Option, UserPLStd]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform ==> Some(UserPLStd(1, "Kuba", Some(28)))
        }

        test("when F = Either[List[String], +*]") {
          User(1, "Kuba", Some(28))
            .intoF[Either[List[String], +*], UserPLStd]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform ==> Right(UserPLStd(1, "Kuba", Some(28)))
        }
      }

      test(
        "should provide a value to a selected target field from a selected source field despite an existing same-named source field"
      ) {
        import products.Renames.*

        test("when F = Option") {
          User2ID(1, "Kuba", Some(28), 666)
            .intoF[Option, User]
            .withFieldRenamed(_.extraID, _.id)
            .transform ==> Some(User(666, "Kuba", Some(28)))
        }

        test("when F = Either[List[String], +*]") {
          User2ID(1, "Kuba", Some(28), 666)
            .intoF[Either[List[String], +*], User]
            .withFieldRenamed(_.extraID, _.id)
            .transform ==> Right(User(666, "Kuba", Some(28)))
        }
      }

      test("should not compile if renamed value change type but an there is no transformer available") {
        import products.Renames.*

        test("when F = Option") {
          compileError(
            """
          User(1, "Kuba", Some(28))
            .intoF[Option, UserPL]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform
          """
          ).check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Renames.User to io.scalaland.chimney.examples.products.Renames.UserPL",
            "io.scalaland.chimney.examples.products.Renames.UserPL",
            "wiek: scala.util.Either - can't derive transformation from wiek: scala.Option in source type io.scalaland.chimney.examples.products.Renames.User",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

          compileError(
            """
          User(1, "Kuba", Some(28))
            .intoF[EitherList, UserPL]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform
          """
          ).check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Renames.User to io.scalaland.chimney.examples.products.Renames.UserPL",
            "io.scalaland.chimney.examples.products.Renames.UserPL",
            "wiek: scala.util.Either - can't derive transformation from wiek: scala.Option in source type io.scalaland.chimney.examples.products.Renames.User",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }
      }

      test("should convert renamed value if types differ but an implicit Total Transformer exists") {
        import products.Renames.*
        implicit val convert: Transformer[Option[Int], Either[Unit, Int]] = ageToWiekTransformer

        test("when F = Option") {
          User(1, "Kuba", Some(28))
            .intoF[Option, UserPL]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform ==> Some(UserPL(1, "Kuba", Right(28)))
          User(1, "Kuba", None)
            .intoF[Option, UserPL]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform ==> Some(UserPL(1, "Kuba", Left(())))
        }

        test("when F = Either[List[String], +*]") {
          User(1, "Kuba", Some(28))
            .intoF[Either[List[String], +*], UserPL]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform ==> Right(UserPL(1, "Kuba", Right(28)))
          User(1, "Kuba", None)
            .intoF[Either[List[String], +*], UserPL]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform ==> Right(UserPL(1, "Kuba", Left(())))
        }
      }

      test("should convert renamed value if types differ but an implicit Lifted Transformer exists") {
        import products.Renames.*

        test("when F = Option") {
          implicit val convert: TransformerF[Option, Option[Int], Int] =
            new TransformerF[Option, Option[Int], Int] {
              override def transform(src: Option[Int]): Option[Int] = src
            }

          User(1, "Kuba", Some(28))
            .intoF[Option, UserPLStrict]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform ==> Some(UserPLStrict(1, "Kuba", 28))
          User(1, "Kuba", None)
            .intoF[Option, UserPLStrict]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform ==> None
        }

        test("when F = Either[List[String], +*]") {
          implicit val convert: TransformerF[Either[List[String], +*], Option[Int], Int] =
            new TransformerF[Either[List[String], +*], Option[Int], Int] {
              override def transform(src: Option[Int]): Either[List[String], Int] = src match {
                case Some(a) => Right(a)
                case None    => Left(List("bad int"))
              }
            }

          User(1, "Kuba", Some(28))
            .intoF[Either[List[String], +*], UserPLStrict]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform ==> Right(UserPLStrict(1, "Kuba", 28))
          User(1, "Kuba", None)
            .intoF[Either[List[String], +*], UserPLStrict]
            .withFieldRenamed(_.name, _.imie)
            .withFieldRenamed(_.age, _.wiek)
            .transform ==> Left(List("bad int"))
        }
      }
    }

    test("flag .enableDefaultValues") {

      test("should be disabled by default") {
        import products.Defaults.*

        test("when F = Option") {
          compileError("""Source(1, "yy", 1.0).transformIntoF[Option, Target]""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Defaults.Source to io.scalaland.chimney.examples.products.Defaults.Target",
            "io.scalaland.chimney.examples.products.Defaults.Target",
            "x: scala.Int - no accessor named x in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )

          compileError("""Source(1, "yy", 1.0).intoF[Option, Target].transform""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Defaults.Source to io.scalaland.chimney.examples.products.Defaults.Target",
            "io.scalaland.chimney.examples.products.Defaults.Target",
            "x: scala.Int - no accessor named x in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

          compileError("""Source(1, "yy", 1.0).transformIntoF[EitherList, Target]""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Defaults.Source to io.scalaland.chimney.examples.products.Defaults.Target",
            "io.scalaland.chimney.examples.products.Defaults.Target",
            "x: scala.Int - no accessor named x in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )

          compileError("""Source(1, "yy", 1.0).intoF[EitherList, Target].transform""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Defaults.Source to io.scalaland.chimney.examples.products.Defaults.Target",
            "io.scalaland.chimney.examples.products.Defaults.Target",
            "x: scala.Int - no accessor named x in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }
      }

      test("should not be needed if all target fields with default values have their values provided in other way") {
        import products.Defaults.*

        test("when F = Option") {
          Source(1, "yy", 1.0)
            .intoF[Option, Target]
            .withFieldConst(_.x, 30)
            .withFieldComputed(_.y, _.yy + "2")
            .transform ==> Some(Target(30, "yy2", 1.0))
        }

        test("when F = Either[List[String], +*]") {
          Source(1, "yy", 1.0)
            .intoF[Either[List[String], +*], Target]
            .withFieldConst(_.x, 30)
            .withFieldComputed(_.y, _.yy + "2")
            .transform ==> Right(Target(30, "yy2", 1.0))
        }
      }

      test("should enable using default values when no source value can be resolved in flat transformation") {
        import products.Defaults.*

        test("when F = Option") {
          Source(1, "yy", 1.0).intoF[Option, Target].enableDefaultValues.transform ==> Some(Target(10, "y", 1.0))

          locally {
            implicit val config = TransformerConfiguration.default.enableDefaultValues

            Source(1, "yy", 1.0).transformIntoF[Option, Target] ==> Some(Target(10, "y", 1.0))
            Source(1, "yy", 1.0).intoF[Option, Target].transform ==> Some(Target(10, "y", 1.0))
          }
        }

        test("when F = Either[List[String], +*]") {
          Source(1, "yy", 1.0).intoF[Either[List[String], +*], Target].enableDefaultValues.transform ==> Right(
            Target(10, "y", 1.0)
          )

          locally {
            implicit val config = TransformerConfiguration.default.enableDefaultValues

            Source(1, "yy", 1.0).transformIntoF[Either[List[String], +*], Target] ==> Right(Target(10, "y", 1.0))
            Source(1, "yy", 1.0).intoF[Either[List[String], +*], Target].transform ==> Right(Target(10, "y", 1.0))
          }
        }
      }

      test("should enable using default values when no source value can be resolved in nested transformation") {
        import products.Defaults.*

        test("when F = Option") {
          Nested(Source(1, "yy", 1.0))
            .intoF[Option, Nested[Target]]
            .enableDefaultValues
            .transform ==> Some(Nested(Target(10, "y", 1.0)))

          locally {
            implicit val config = TransformerConfiguration.default.enableDefaultValues

            Nested(Source(1, "yy", 1.0)).transformIntoF[Option, Nested[Target]] ==> Some(Nested(Target(10, "y", 1.0)))
            Nested(Source(1, "yy", 1.0)).intoF[Option, Nested[Target]].transform ==> Some(Nested(Target(10, "y", 1.0)))
          }
        }

        test("when F = Either[List[String], +*]") {
          Nested(Source(1, "yy", 1.0))
            .intoF[Either[List[String], +*], Nested[Target]]
            .enableDefaultValues
            .transform ==> Right(Nested(Target(10, "y", 1.0)))

          locally {
            implicit val config = TransformerConfiguration.default.enableDefaultValues

            Nested(Source(1, "yy", 1.0))
              .transformIntoF[Either[List[String], +*], Nested[Target]] ==> Right(Nested(Target(10, "y", 1.0)))
            Nested(Source(1, "yy", 1.0))
              .intoF[Either[List[String], +*], Nested[Target]]
              .transform ==> Right(Nested(Target(10, "y", 1.0)))
          }
        }
      }

      test("should ignore default value if other setting provides it or source field exists") {
        import products.Defaults.*

        test("when F = Option") {
          Source(1, "yy", 1.0)
            .intoF[Option, Target]
            .enableDefaultValues
            .withFieldConst(_.x, 30)
            .withFieldComputed(_.y, _.yy + "2")
            .transform ==> Some(Target(30, "yy2", 1.0))

          locally {
            implicit val config = TransformerConfiguration.default.enableDefaultValues

            Source(1, "yy", 1.0)
              .intoF[Option, Target]
              .withFieldConst(_.x, 30)
              .withFieldComputed(_.y, _.yy + "2")
              .transform ==> Some(Target(30, "yy2", 1.0))
          }
        }

        test("when F = Either[List[String], +*]") {
          Source(1, "yy", 1.0)
            .intoF[Either[List[String], +*], Target]
            .enableDefaultValues
            .withFieldConst(_.x, 30)
            .withFieldComputed(_.y, _.yy + "2")
            .transform ==> Right(Target(30, "yy2", 1.0))

          locally {
            implicit val config = TransformerConfiguration.default.enableDefaultValues

            Source(1, "yy", 1.0)
              .intoF[Either[List[String], +*], Target]
              .withFieldConst(_.x, 30)
              .withFieldComputed(_.y, _.yy + "2")
              .transform ==> Right(Target(30, "yy2", 1.0))
          }
        }
      }

      test("should ignore default value if source fields with different type but Total Transformer for it exists") {
        import products.Defaults.*
        implicit val converter: Transformer[Int, Long] = _.toLong

        test("when F = Option") {
          Source(1, "yy", 1.0)
            .intoF[Option, Target2]
            .enableDefaultValues
            .transform ==> Some(Target2(1L, "yy", 1.0))

          locally {
            implicit val config = TransformerConfiguration.default.enableDefaultValues

            Source(1, "yy", 1.0).transformIntoF[Option, Target2] ==> Some(Target2(1L, "yy", 1.0))
            Source(1, "yy", 1.0).intoF[Option, Target2].transform ==> Some(Target2(1L, "yy", 1.0))
          }
        }

        test("when F = Either[List[String], +*]") {
          Source(1, "yy", 1.0)
            .intoF[Either[List[String], +*], Target2]
            .enableDefaultValues
            .transform ==> Right(Target2(1L, "yy", 1.0))

          locally {
            implicit val config = TransformerConfiguration.default.enableDefaultValues

            Source(1, "yy", 1.0).transformIntoF[Either[List[String], +*], Target2] ==> Right(Target2(1L, "yy", 1.0))
            Source(1, "yy", 1.0).intoF[Either[List[String], +*], Target2].transform ==> Right(Target2(1L, "yy", 1.0))
          }
        }
      }

      test("should ignore default value if source fields with different type but Lifted Transformer for it exists") {
        import products.Defaults.*

        test("when F = Option") {
          implicit val converter: TransformerF[Option, Int, Long] = i => Some(i.toLong)

          Source(1, "yy", 1.0)
            .intoF[Option, Target2]
            .enableDefaultValues
            .transform ==> Some(Target2(1L, "yy", 1.0))

          locally {
            implicit val config = TransformerConfiguration.default.enableDefaultValues

            Source(1, "yy", 1.0).transformIntoF[Option, Target2] ==> Some(Target2(1L, "yy", 1.0))
            Source(1, "yy", 1.0).intoF[Option, Target2].transform ==> Some(Target2(1L, "yy", 1.0))
          }
        }

        test("when F = Either[List[String], +*]") {
          implicit val converter: TransformerF[Either[List[String], +*], Int, Long] = i => Right(i.toLong)

          Source(1, "yy", 1.0)
            .intoF[Either[List[String], +*], Target2]
            .enableDefaultValues
            .transform ==> Right(Target2(1L, "yy", 1.0))

          locally {
            implicit val config = TransformerConfiguration.default.enableDefaultValues

            Source(1, "yy", 1.0).transformIntoF[Either[List[String], +*], Target2] ==> Right(Target2(1L, "yy", 1.0))
            Source(1, "yy", 1.0).intoF[Either[List[String], +*], Target2].transform ==> Right(Target2(1L, "yy", 1.0))
          }
        }
      }
    }

    test("flag .disableDefaultValues") {

      test("should disable globally enabled .enableDefaultValues") {
        import products.Defaults.*

        implicit val config = TransformerConfiguration.default.enableDefaultValues

        test("when F = Option") {
          compileError("""Source(1, "yy", 1.0).intoF[Option, Target].disableDefaultValues.transform""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Defaults.Source to io.scalaland.chimney.examples.products.Defaults.Target",
            "io.scalaland.chimney.examples.products.Defaults.Target",
            "x: scala.Int - no accessor named x in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

          compileError("""Source(1, "yy", 1.0).intoF[EitherList, Target].disableDefaultValues.transform""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.products.Defaults.Source to io.scalaland.chimney.examples.products.Defaults.Target",
            "io.scalaland.chimney.examples.products.Defaults.Target",
            "x: scala.Int - no accessor named x in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "y: java.lang.String - no accessor named y in source type io.scalaland.chimney.examples.products.Defaults.Source",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }
      }
    }

    // TODO: test("flag .enableMethodAccessors") {}

    // TODO: test("flag .disableMethodAccessors") {}

    // TODO: refactor tests below

    test("transform always fails") {

      import trip.*

      test("option") {
        Person("John", 10, 140)
          .intoF[Option, User]
          .withFieldConstF(_.height, Option.empty[Double])
          .transform ==> None
      }

      test("either") {
        Person("John", 10, 140)
          .intoF[Either[List[String], +*], User]
          .withFieldConstF(_.height, Left(List("abc", "def")))
          .transform ==> Left(List("abc", "def"))
      }
    }

    test("simple transform with validation") {

      import trip.*

      test("success") {
        val okForm = PersonForm("John", "10", "140")

        test("1-arg") {

          test("option") {
            okForm
              .into[Person]
              .withFieldConst(_.age, 7)
              .withFieldComputedF(_.height, _.height.parseDouble)
              .transform ==> Some(Person("John", 7, 140))
          }

          test("either") {
            okForm
              .into[Person]
              .withFieldConst(_.height, 200.5)
              .withFieldComputedF[Either[List[String], +*], Int, Int](_.age, _.age.parseInt.toEitherList("bad age"))
              .transform ==> Right(Person("John", 10, 200.5))
          }
        }

        test("2-arg") {

          test("option") {
            okForm
              .into[Person]
              .withFieldComputedF(_.age, _.age.parseInt)
              .withFieldComputedF(_.height, _.height.parseDouble)
              .transform ==> Some(Person("John", 10, 140))
          }

          test("either") {
            okForm
              .intoF[Either[List[String], +*], Person]
              .withFieldConst(_.name, "Joe")
              .withFieldComputedF(_.height, _.height.parseDouble.toEitherList("bad height"))
              .withFieldComputedF(_.age, _.age.parseInt.toEitherList("bad age"))
              .transform ==> Right(Person("Joe", 10, 140))
          }
        }

        test("3-arg") {

          test("option") {
            okForm
              .into[Person]
              .withFieldComputedF(_.name, pf => if (pf.name.isEmpty) None else Some(pf.name.toUpperCase()))
              .withFieldComputedF(_.age, _.age.parseInt)
              .withFieldComputedF(_.height, _.height.parseDouble)
              .transform ==> Some(Person("JOHN", 10, 140))
          }

          test("either") {
            okForm
              .intoF[Either[List[String], +*], Person]
              .withFieldComputedF(
                _.name,
                pf =>
                  if (pf.name.isEmpty) Left(List("empty name"))
                  else Right(pf.name.toUpperCase())
              )
              .withFieldComputedF(_.age, _.age.parseInt.toEitherList("bad age"))
              .withFieldComputedF(_.height, _.height.parseDouble.toEitherList("bad height"))
              .transform ==> Right(Person("JOHN", 10, 140))
          }
        }
      }

      test("failure with error handling") {
        val badForm = PersonForm("", "foo", "bar")

        test("option") {
          badForm
            .intoF[Option, Person]
            .withFieldComputedF(_.age, _.age.parseInt)
            .withFieldComputedF(_.height, _.age.parseDouble)
            .transform ==> None
        }

        test("either") {
          badForm
            .into[Person]
            .withFieldComputedF[Either[List[String], +*], String, String](
              _.name,
              pf =>
                if (pf.name.isEmpty) Left(List("empty name"))
                else Right(pf.name.toUpperCase())
            )
            .withFieldComputedF(_.age, _.age.parseInt.toEitherList("bad age"))
            .withFieldComputedF(_.height, _.age.parseDouble.toEitherList("bad double"))
            .transform ==> Left(List("empty name", "bad age", "bad double"))
        }
      }
    }

    test("recursive transform with nested validation") {

      import trip.*

      implicit val personTransformerOpt: TransformerF[Option, PersonForm, Person] =
        Transformer
          .define[PersonForm, Person]
          .withFieldComputedF(_.age, _.age.parseInt)
          .withFieldComputedF(_.height, _.height.parseDouble)
          .buildTransformer

      implicit val personTransformerEithers: TransformerF[Either[List[String], +*], PersonForm, Person] =
        Transformer
          .defineF[Either[List[String], +*], PersonForm, Person]
          .withFieldComputedF(_.age, _.age.parseInt.toEitherList("bad age"))
          .withFieldComputedF(_.height, _.height.parseDouble.toEitherList("bad height"))
          .buildTransformer

      test("success") {

        val okTripForm = TripForm("100", List(PersonForm("John", "10", "140"), PersonForm("Caroline", "12", "155")))

        test("option") {

          okTripForm
            .into[Trip]
            .withFieldComputedF(_.id, _.tripId.parseInt)
            .transform ==> Some(Trip(100, Vector(Person("John", 10, 140), Person("Caroline", 12, 155))))
        }

        test("either") {
          okTripForm
            .intoF[Either[List[String], +*], Trip]
            .withFieldComputedF(_.id, tf => tf.tripId.parseInt.toEitherList("bad id"))
            .transform ==> Right(Trip(100, Vector(Person("John", 10, 140), Person("Caroline", 12, 155))))
        }
      }

      test("failure with error handling") {

        val badTripForm = TripForm("100", List(PersonForm("John", "10", "foo"), PersonForm("Caroline", "bar", "155")))

        test("option") {
          badTripForm
            .into[Trip]
            .withFieldComputedF(_.id, _.tripId.parseInt)
            .transform ==> None
        }

        test("either") {
          badTripForm
            .intoF[Either[List[String], +*], Trip]
            .withFieldComputedF(_.id, tf => tf.tripId.parseInt.toEitherList("bad id"))
            .transform ==> Left(List("bad height", "bad age"))
        }
      }
    }

    test("implicit conflict resolution") {

      final case class InnerIn(value: String)
      final case class InnerOut(value: String)

      final case class OuterIn(value: InnerIn)
      final case class OuterOut(value: InnerOut)

      implicit val pureTransformer: Transformer[InnerIn, InnerOut] =
        in => InnerOut(s"pure: ${in.value}")

      implicit val liftedTransformer: TransformerF[Either[List[String], +*], InnerIn, InnerOut] =
        in => Right(InnerOut(s"lifted: ${in.value}"))

      test("fail compilation if there is unresolved conflict") {

        compileError("""
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
          OuterIn(InnerIn("test")).transformIntoF[EitherList, OuterOut]
          """)
          .check(
            "",
            "Ambiguous implicits while resolving Chimney recursive transformation",
            "Please eliminate ambiguity from implicit scope or use withFieldComputed/withFieldComputedF to decide which one should be used"
          )
      }

      test("resolve conflict explicitly using .withFieldComputed") {
        OuterIn(InnerIn("test"))
          .intoF[Either[List[String], +*], OuterOut]
          .withFieldComputed(_.value, v => pureTransformer.transform(v.value))
          .transform ==> Right(OuterOut(InnerOut("pure: test")))
      }

      test("resolve conflict explicitly using .withFieldComputedF") {
        OuterIn(InnerIn("test"))
          .intoF[Either[List[String], +*], OuterOut]
          .withFieldComputedF(_.value, v => liftedTransformer.transform(v.value))
          .transform ==> Right(OuterOut(InnerOut("lifted: test")))
      }

      test("resolve conflict explicitly prioritizing: last wins") {
        OuterIn(InnerIn("test"))
          .intoF[Either[List[String], +*], OuterOut]
          .withFieldComputed(_.value, v => pureTransformer.transform(v.value))
          .withFieldComputedF(_.value, v => liftedTransformer.transform(v.value))
          .transform ==> Right(OuterOut(InnerOut("lifted: test")))

        OuterIn(InnerIn("test"))
          .intoF[Either[List[String], +*], OuterOut]
          .withFieldComputedF(_.value, v => liftedTransformer.transform(v.value))
          .withFieldComputed(_.value, v => pureTransformer.transform(v.value))
          .transform ==> Right(OuterOut(InnerOut("pure: test")))
      }
    }

    test("support scoped transformer configuration passed implicitly") {

      class Source {
        def field1: Int = 100
      }
      case class Target(field1: Int = 200, field2: Option[String] = Some("foo"))

      implicit val transformerConfiguration = {
        TransformerConfiguration.default.enableOptionDefaultsToNone.enableMethodAccessors.disableDefaultValues
      }

      test("scoped config only") {

        (new Source).transformIntoF[Option, Target] ==> Some(Target(100, None))
        (new Source).intoF[Option, Target].transform ==> Some(Target(100, None))
      }

      test("scoped config overridden by instance flag") {

        (new Source)
          .intoF[Option, Target]
          .disableMethodAccessors
          .enableDefaultValues
          .transform ==> Some(Target(200, Some("foo")))

        (new Source)
          .intoF[Option, Target]
          .enableDefaultValues
          .transform ==> Some(Target(100, Some("foo")))

        (new Source)
          .intoF[Option, Target]
          .disableOptionDefaultsToNone
          .withFieldConst(_.field2, Some("abc"))
          .transform ==> Some(Target(100, Some("abc")))

      }

      test("compile error when optionDefaultsToNone were disabled locally") {

        compileError("""
          (new Source).intoF[Option, Target].disableOptionDefaultsToNone.transform
        """)
          .check("", "Chimney can't derive transformation from Source to Target")
      }
    }

    test("support config type-aliases") {
      type VTransformer[A, B] =
        io.scalaland.chimney.TransformerF[VTransformer.F, A, B]

      object VTransformer {

        import io.scalaland.chimney.internal.{TransformerCfg, TransformerFlags}

        type F[+A] = Either[List[String], A]
        type DefaultCfg = TransformerCfg.WrapperType[F, TransformerCfg.Empty]
        type Definition[From, To] =
          TransformerFDefinition[F, From, To, DefaultCfg, TransformerFlags.Default]

        def define[From, To]: Definition[From, To] =
          io.scalaland.chimney.TransformerF.define[F, From, To]
      }

      implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
        _.parseInt.toEitherList("bad int")

      case class Foo(foo: String)

      case class Bar(bar: Int)

      implicit val fooToBar: VTransformer[Foo, Bar] =
        VTransformer.define[Foo, Bar].withFieldRenamed(_.foo, _.bar).buildTransformer

      fooToBar.transform(Foo("1")) ==> Right(Bar(1))
    }
  }
}
