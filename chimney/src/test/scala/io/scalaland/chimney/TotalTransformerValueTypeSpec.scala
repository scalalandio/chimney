package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples.valuetypes._
import utest._

object TotalTransformerValueTypeSpec extends TestSuite {

  val tests = Tests {

    test("transform from a value class into a value") {
      UserName("Batman").transformInto[String] ==> "Batman"
      User("100", UserName("abc")).transformInto[UserDTO] ==> UserDTO("100", "abc")
    }

    test("transforming from a value to a value class") {
      "Batman".transformInto[UserName] ==> UserName("Batman")
      UserDTO("100", "abc").transformInto[User] ==> User("100", UserName("abc"))
    }
  }
}
