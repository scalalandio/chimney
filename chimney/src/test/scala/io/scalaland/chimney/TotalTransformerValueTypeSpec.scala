package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.valuetypes.*
import utest.*

object TotalTransformerValueTypeSpec extends TestSuite {

  val tests = Tests {

    test("transform from a value class(member type: 'T') into a value(type 'T')") {
      UserName("Batman").transformInto[String] ==> "Batman"
      User("100", UserName("abc")).transformInto[UserDTO] ==> UserDTO("100", "abc")
    }

    test("transforming from a value(type 'T') to a value class(member type: 'T')") {
      "Batman".transformInto[UserName] ==> UserName("Batman")
      UserDTO("100", "abc").transformInto[User] ==> User("100", UserName("abc"))
    }

    test("transforming value class(member type: 'T') to a value class(member type: 'T')") {

      UserName("Batman").transformInto[UserNameAlias] ==> UserNameAlias("Batman")
      User("100", UserName("abc")).transformInto[UserAlias] ==>
        UserAlias("100", UserNameAlias("abc"))
    }

    test("transform from a value class(member type: 'T') into a value(type 'S') if 'T'=>'S' exists") {
      implicit val transformer = new Transformer[String, Int] {
        override def transform(src: String): Int = src.length
      }

      val batman = "Batman"
      val abc = "abc"
      UserName(batman).transformInto[Int] ==> batman.length
      UserWithUserName(UserName(abc)).transformInto[UserWithId] ==> UserWithId(abc.length)
    }

    test("transform from a value(type: 'T') into a value class(member type: 'S') if 'T'=>'S' exists") {
      implicit val transformer = new Transformer[String, Int] {
        override def transform(src: String): Int = src.length
      }

      val batman = "Batman"
      val abc = "abc"
      batman.transformInto[UserId] ==> UserId(batman.length)
      UserWithName(abc).transformInto[UserWithUserId] ==> UserWithUserId(UserId(abc.length))
    }

    test("transforming value class(member type: `S`) to value class(member type: 'T') if 'T'=>'S' exists") {
      implicit val transformer = new Transformer[String, Int] {
        override def transform(src: String): Int = src.length
      }

      val batman = "Batman"
      val abc = "abc"
      UserName(batman).transformInto[UserId] ==> UserId(batman.length)
      UserWithUserName(UserName(abc)).transformInto[UserWithUserId] ==> UserWithUserId(UserId(abc.length))

    }
  }
}
