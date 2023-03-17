package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.valuetypes.*
import utest.*

object PartialTransformerValueTypeSpec extends TestSuite {

  val tests = Tests {

    test("transform from a value class(member type 'T') into a value(type 'T')") {
      UserName("Batman").transformIntoPartial[String].asOption ==> Some("Batman")
      User("100", UserName("abc")).transformIntoPartial[UserDTO].asOption ==> Some(UserDTO("100", "abc"))
    }

    test("transforming from a value(type 'T') to a value class(member type 'T')") {
      "Batman".transformIntoPartial[UserName].asOption ==> Some(UserName("Batman"))
      UserDTO("100", "abc").transformIntoPartial[User].asOption ==> Some(User("100", UserName("abc")))
    }

    test("transforming value class(member type: 'T') to a value class(member type: 'T')") {
      UserName("Batman").transformIntoPartial[UserNameAlias].asOption ==> Some(UserNameAlias("Batman"))
      User("100", UserName("abc")).transformIntoPartial[UserAlias].asOption ==>
        Some(UserAlias("100", UserNameAlias("abc")))
    }

    test("transform from a value class(member type: 'T') into a value(type 'S') if 'T'=>'S' exists") {
      implicit val transformer = new PartialTransformer[String, Int] {
        override def transform(src: String, failFast: Boolean): partial.Result[Int] = partial.Result.Value(src.length)
      }

      val batman = "Batman"
      val abc = "abc"
      UserName(batman).transformIntoPartial[Int].asOption ==> Some(batman.length)
      UserWithUserName(UserName(abc)).transformIntoPartial[UserWithId].asOption ==> Option(UserWithId(abc.length))
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
