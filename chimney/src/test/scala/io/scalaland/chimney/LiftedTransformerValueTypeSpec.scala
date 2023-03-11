package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples.valuetypes._
import utest._

object LiftedTransformerValueTypeSpec extends TestSuite {

  val tests = Tests {

    test("transform from a value class(member type 'T') into a value(type 'T')") {

      test("when F = Option") {
        UserName("Batman").transformIntoF[Option, String] ==> Some("Batman")
        User("100", UserName("abc")).transformIntoF[Option, UserDTO] ==> Some(UserDTO("100", "abc"))
      }

      test("when F = Either[List[String], +*]") {
        UserName("Batman").transformIntoF[Either[List[String], +*], String] ==> Right("Batman")
        User("100", UserName("abc")).transformIntoF[Either[List[String], +*], UserDTO] ==> Right(UserDTO("100", "abc"))
      }
    }

    test("transforming from a value(type 'T') to a value class(member type 'T')") {

      test("when F = Option") {
        "Batman".transformIntoF[Option, UserName] ==> Some(UserName("Batman"))
        UserDTO("100", "abc").transformIntoF[Option, User] ==> Some(User("100", UserName("abc")))
      }

      test("when F = Either[List[String], +*]") {
        "Batman".transformIntoF[Either[List[String], +*], UserName] ==> Right(UserName("Batman"))
        UserDTO("100", "abc").transformIntoF[Either[List[String], +*], User] ==> Right(User("100", UserName("abc")))
      }
    }

    test("transforming value class(member type: 'T') to a value class(member type: 'T')") {

      test("when F = Option") {
        UserName("Batman").transformIntoF[Option, UserNameAlias] ==> Some(UserNameAlias("Batman"))
        User("100", UserName("abc")).transformIntoF[Option, UserAlias] ==>
          Some(UserAlias("100", UserNameAlias("abc")))
      }

      test("when F = Either[List[String], +*]") {
        UserName("Batman").transformIntoF[Either[List[String], +*], UserNameAlias] ==> Right(UserNameAlias("Batman"))
        User("100", UserName("abc")).transformIntoF[Either[List[String], +*], UserAlias] ==>
          Right(UserAlias("100", UserNameAlias("abc")))
      }
    }

    test("transform from a value class(member type: 'T') into a value(type 'S') if 'T'=>'S' exists") {
      implicit val transformerOption = new TransformerF[Option, String, Int] {
        override def transform(src: String): Option[Int] = Some(src.length)
      }

      implicit val transformerEither = new TransformerF[Either[List[String], +*], String, Int] {
        override def transform(src: String): Either[List[String], Int] = Right(src.length)
      }

      val batman = "Batman"
      val abc = "abc"

      test("when F = Option") {
        UserName(batman).transformIntoF[Option, Int] ==> Some(batman.length)
        UserWithUserName(UserName(abc)).transformIntoF[Option, UserWithId] ==> Some(UserWithId(abc.length))
      }

      test("when F = Either[List[String], +*]") {
        UserName(batman).transformIntoF[Either[List[String], +*], Int] ==> Right(batman.length)
        UserWithUserName(UserName(abc)).transformIntoF[Either[List[String], +*], UserWithId] ==> Right(
          UserWithId(abc.length)
        )
      }
    }

    test("transform from a value(type: 'T') into a value class(member type: 'S') if 'T'=>'S' exists") {
      implicit val transformerOption = new TransformerF[Option, String, Int] {
        override def transform(src: String): Option[Int] = Some(src.length)
      }

      implicit val transformerEither = new TransformerF[Either[List[String], +*], String, Int] {
        override def transform(src: String): Either[List[String], Int] = Right(src.length)
      }

      val batman = "Batman"
      val abc = "abc"

      test("when F = Option") {
        batman.transformIntoF[Option, UserId] ==> Some(UserId(batman.length))
        UserWithName(abc).transformIntoF[Option, UserWithUserId] ==> Some(UserWithUserId(UserId(abc.length)))
      }

      test("when F = Either[List[String], +*]") {
        batman.transformIntoF[Either[List[String], +*], UserId] ==> Right(UserId(batman.length))
        UserWithName(abc).transformIntoF[Either[List[String], +*], UserWithUserId] ==> Right(
          UserWithUserId(UserId(abc.length))
        )
      }
    }

    test("transforming value class(member type: `S`) to value class(member type: 'T') if 'T'=>'S' exists") {
      implicit val transformerOption = new TransformerF[Option, String, Int] {
        override def transform(src: String): Option[Int] = Some(src.length)
      }

      implicit val transformerEither = new TransformerF[Either[List[String], +*], String, Int] {
        override def transform(src: String): Either[List[String], Int] = Right(src.length)
      }

      val batman = "Batman"
      val abc = "abc"

      test("when F = Option") {
        UserName(batman).transformIntoF[Option, UserId] ==> Some(UserId(batman.length))
        UserWithUserName(UserName(abc)).transformIntoF[Option, UserWithUserId] ==> Some(
          UserWithUserId(UserId(abc.length))
        )
      }

      test("when F = Either[List[String], +*]") {
        UserName(batman).transformIntoF[Either[List[String], +*], UserId] ==> Right(UserId(batman.length))
        UserWithUserName(UserName(abc)).transformIntoF[Either[List[String], +*], UserWithUserId] ==> Right(
          UserWithUserId(UserId(abc.length))
        )
      }
    }
  }
}
