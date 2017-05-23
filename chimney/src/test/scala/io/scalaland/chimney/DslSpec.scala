package io.scalaland.chimney

import org.scalatest.{MustMatchers, WordSpec}
import shapeless.test._

class DslSpec extends WordSpec with MustMatchers {


  case class UserName(value: String)

  val userNameToStringTransformer: Transformer.Aux[UserName, String] =
    (un: UserName, _: Modifier.empty) => un.value

  case class UserDTO(id: String, name: String)
  case class User(id: String, name: UserName)

  import dsl._

  "A Chimney DSL" should {

    "use implicit transformer directly" in {

      implicit val _ = userNameToStringTransformer

      UserName("Batman").transformer[String].invoke mustBe "Batman"
    }

    "use implicit transformer for nested field" in {

      implicit val _ = userNameToStringTransformer

      val batman = User("123", UserName("Batman"))
      val batmanDTO = batman.transformer[UserDTO].invoke

      batmanDTO.id mustBe batman.id
      batmanDTO.name mustBe batman.name.value
    }

    "support different set of fields of source and target" when {

      case class Foo(x: Int, y: String, z: Double)
      case class Bar(x: Int, z: Double)

      "field is dropped in the target" in {
        Foo(3, "pi", 3.14).transformer[Bar].invoke mustBe Bar(3, 3.14)
      }

      "field is added to the target" should {

        "not compile if source for the target fields is not provided" in {

          illTyped("Bar(3, 3.14).transformer[Foo].invoke")
        }

        "fill the field with provided default value" in {

          Bar(3, 3.14).transformer[Foo].invokeM(Modifier.fieldValue('y, "pi")) mustBe
            Foo(3, "pi", 3.14)
        }

        "fill the field with provided generator function" in {

          Bar(3, 3.14).transformer[Foo].invokeM(Modifier.fieldFunction('y, (bar: Bar) => bar.x.toString)) mustBe
            Foo(3, "3", 3.14)
        }
      }
    }

    "support relabelling of fields" should {

      case class Foo(x: Int, y: String)
      case class Bar(x: Int, z: String)

      "not compile if relabelling modifier is not provided" in {

        illTyped("""Foo(10, "something").transform[Bar].invoke""")
      }

      "relabel fields with relabelling modifier" in {
        Foo(10, "something").transformer[Bar].invokeM(Modifier.relabel('y, 'z))
      }
    }
  }
}
