package io.scalaland.chimney

import org.scalatest.{MustMatchers, WordSpec}
import shapeless.test._

class DslSpec extends WordSpec with MustMatchers {


  import dsl._

  "A Chimney DSL" should {

    "use implicit transformer directly" in {

      import Domain1._

      implicit val _ = userNameToStringTransformer

      UserName("Batman").transformInto[String] mustBe "Batman"
      UserName("Batman").transformInto[String] mustBe "Batman"
    }

    "use implicit transformer for nested field" in {

      import Domain1._

      implicit val _ = userNameToStringTransformer

      val batman = User("123", UserName("Batman"))
      val batmanDTO = batman.transformInto[UserDTO]

      batmanDTO.id mustBe batman.id
      batmanDTO.name mustBe batman.name.value
    }

    "support different set of fields of source and target" when {

      case class Foo(x: Int, y: String, z: Double)
      case class Bar(x: Int, z: Double)

      "field is dropped in the target" in {
        Foo(3, "pi", 3.14).transformInto[Bar] mustBe Bar(3, 3.14)
      }

      "field is added to the target" should {

        "not compile if source for the target fields is not provided" in {

          illTyped("Bar(3, 3.14).transformInto[Foo]")
        }

        "fill the field with provided default value" in {

          Bar(3, 3.14)
            .into[Foo]
            .withFieldConst('y, "pi")
            .transform mustBe
            Foo(3, "pi", 3.14)
        }

        "fill the field with provided generator function" in {

          Bar(3, 3.14)
            .into[Foo]
            .withFieldComputed('y, _.x.toString)
            .transform mustBe
            Foo(3, "3", 3.14)
        }
      }
    }

    "support relabelling of fields" should {

      case class Foo(x: Int, y: String)
      case class Bar(x: Int, z: String)

      "not compile if relabelling modifier is not provided" in {

        illTyped("""Foo(10, "something").transformInto[Bar]""")
      }

      "relabel fields with relabelling modifier" in {
        Foo(10, "something")
          .into[Bar]
          .withFieldRenamed('y, 'z)
          .transform mustBe
          Bar(10, "something")
      }

      "not compile if relabelling wrongly" in {

        illTyped(
          """Foo(10, "something").into[Bar].withFieldRenamed('y, 'ne).transform"""
        )

        illTyped(
          """Foo(10, "something").into[Bar].withFieldRenamed('ne, 'z).transform"""
        )
      }
    }

    "support value classes" when {

      import VCDomain1._

      "transforming value class to a value" in {

        UserName("Batman").transformInto[String] mustBe "Batman"
        User("100", UserName("abc")).transformInto[UserDTO] mustBe
          UserDTO("100", "abc")
      }

      "transforming value to a value class" in {

        "Batman".transformInto[UserName] mustBe UserName("Batman")
        UserDTO("100", "abc").transformInto[User] mustBe
          User("100", UserName("abc"))

      }
    }

    "support common data types" should {

      case class Foo(value: String)
      case class Bar(value: String)

      "support scala.Option" in {
        Option(Foo("a")).transformInto[Option[Bar]] mustBe Option(Bar("a"))
        (Some(Foo("a")): Option[Foo]).transformInto[Option[Bar]] mustBe Option(Bar("a"))
        Some(Foo("a")).transformInto[Option[Bar]] mustBe Some(Bar("a")) // FIXIT
        None.transformInto[Option[Bar]] mustBe None                     // FIXIT
        (None: Option[Foo]).transformInto[Option[Bar]] mustBe None
        Some(Foo("a")).transformInto[Some[Bar]] mustBe Some(Bar("a"))
        None.transformInto[None.type] mustBe None
      }

      "support scala.util.Either" in {
        (Left(Foo("a")): Either[Foo, Foo]).transformInto[Either[Bar, Bar]] mustBe Left(Bar("a"))
        (Right(Foo("a")): Either[Foo, Foo]).transformInto[Either[Bar, Bar]] mustBe Right(Bar("a"))
        Left(Foo("a")).transformInto[Either[Bar, Bar]] mustBe Left(Bar("a"))   // FIXIT
        Right(Foo("a")).transformInto[Either[Bar, Bar]] mustBe Right(Bar("a")) // FIXIT
        Left(Foo("a")).transformInto[Left[Bar, Bar]] mustBe Left(Bar("a"))
        Right(Foo("a")).transformInto[Right[Bar, Bar]] mustBe Right(Bar("a"))
      }

      "support Traversable collections" in {
        Seq(Foo("a")).transformInto[Seq[Bar]] mustBe Seq(Bar("a"))
        List(Foo("a")).transformInto[List[Bar]] mustBe List(Bar("a"))
        Vector(Foo("a")).transformInto[Vector[Bar]] mustBe Vector(Bar("a"))
      }

      "support Arrays" in {
        Array(Foo("a")).transformInto[Array[Bar]] mustBe Array(Bar("a"))
      }

      "support Map" in {
        Map("test" -> Foo("a")).transformInto[Map[String, Bar]] mustBe Map("test" -> Bar("a"))
      }
    }
  }
}

object Domain1 {

  case class UserName(value: String)

  val userNameToStringTransformer: Transformer[UserName, String] =
    (_: UserName).value

  case class UserDTO(id: String, name: String)
  case class User(id: String, name: UserName)
}

object VCDomain1 {

  case class UserName(value: String) extends AnyVal
  case class UserDTO(id: String, name: String)
  case class User(id: String, name: UserName)
}
