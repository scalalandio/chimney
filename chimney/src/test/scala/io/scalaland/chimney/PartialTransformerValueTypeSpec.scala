package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.examples.valuetypes.*
import utest.*

object PartialTransformerValueTypeSpec extends TestSuite {

  val tests = Tests {

    test("transform from a value class into a value") {
      UserName("Batman").transformIntoPartial[String].asOption ==> Some("Batman")
      User("100", UserName("abc")).transformIntoPartial[UserDTO].asOption ==> Some(UserDTO("100", "abc"))
    }

    test("transforming from a value to a value class") {
      "Batman".transformIntoPartial[UserName].asOption ==> Some(UserName("Batman"))
      UserDTO("100", "abc").transformIntoPartial[User].asOption ==> Some(User("100", UserName("abc")))
    }
  }
}
