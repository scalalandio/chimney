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

    test("transforming value class(member type: 'T') to a value class(member type: 'T')") {

      UserName("Batman").transformInto[UserNameAlias] ==> UserNameAlias("Batman")
      User("100", UserName("abc")).transformInto[UserAlias] ==>
        UserAlias("100", UserNameAlias("abc"))
    }

    test("transforming value class(member type: `S`) to value class(member type: 'T') if 'T'=>'S' transformer exists") {
      implicit val transformer = new Transformer[String, Int] {
        override def transform(src: String): Int = src.length
      }

      val batman = "Batman"
      val abc = "abc"
      UserName(batman).transformInto[UserId] ==> UserId(batman.length)
      UserWithName(UserName(abc)).transformInto[UserWithId] ==> UserWithId(UserId(abc.length))

    }
  }
}
