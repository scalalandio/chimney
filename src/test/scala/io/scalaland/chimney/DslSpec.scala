package io.scalaland.chimney

import org.scalatest.{MustMatchers, WordSpec}

class DslSpec extends WordSpec with MustMatchers {


  case class UserName(value: String)

  val userNameToStringTransformer: Transformer[UserName, String] =
    (_: UserName).value

  case class UserDTO(id: String, name: String)
  case class User(id: String, name: UserName)

  import dsl._

  "A Chimney DSL" should {

    "use implicit transformer directly" in {

      implicit val _ = userNameToStringTransformer

      UserName("Batman").transformTo[String] mustBe "Batman"
    }

    "use implicit transformer for nested field" in {

      implicit val _ = userNameToStringTransformer

      val batman = User("123", UserName("Batman"))
      val batmanDTO = batman.transformTo[UserDTO]

      batmanDTO.id mustBe batman.id
      batmanDTO.name mustBe batman.name.value
    }

    "transform to a target case class which contains only subset of source fields" in {

      case class Foo(x: Int, y: String, z: Double)
      case class Bar(x: Int, z: Double)

      Foo(3, "pi", 3.14).transformTo[Bar] mustBe Bar(3, 3.14)
    }
  }
}
