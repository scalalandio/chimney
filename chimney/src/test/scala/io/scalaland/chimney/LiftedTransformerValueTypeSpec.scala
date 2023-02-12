package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples.valuetypes._
import utest._

object LiftedTransformerValueTypeSpec extends TestSuite {

  val tests = Tests {

    test("transform from a value class into a value") {

      test("when F = Option") {
        UserName("Batman").transformIntoF[Option, String] ==> Some("Batman")
        User("100", UserName("abc")).transformIntoF[Option, UserDTO] ==> Some(UserDTO("100", "abc"))
      }

      test("when F = Either[List[String], +*]") {
        UserName("Batman").transformIntoF[Either[List[String], +*], String] ==> Right("Batman")
        User("100", UserName("abc")).transformIntoF[Either[List[String], +*], UserDTO] ==> Right(UserDTO("100", "abc"))
      }
    }

    test("transforming from a value to a value class") {

      test("when F = Option") {
        "Batman".transformIntoF[Option, UserName] ==> Some(UserName("Batman"))
        UserDTO("100", "abc").transformIntoF[Option, User] ==> Some(User("100", UserName("abc")))
      }

      test("when F = Either[List[String], +*]") {
        "Batman".transformIntoF[Either[List[String], +*], UserName] ==> Right(UserName("Batman"))
        UserDTO("100", "abc").transformIntoF[Either[List[String], +*], User] ==> Right(User("100", UserName("abc")))
      }
    }
  }
}
