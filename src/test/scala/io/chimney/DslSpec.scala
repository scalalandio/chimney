package io.chimney

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

      UserName("Batman").to[String] mustBe "Batman"
    }

    "use implicit transformer for nested field" in {

      implicit val _ = userNameToStringTransformer

      val batman = User("123", UserName("Batman"))
      val batmanDTO = batman.to[UserDTO]

      batmanDTO.id mustBe batman.id
      batmanDTO.name mustBe batman.name.value
    }
  }
}
