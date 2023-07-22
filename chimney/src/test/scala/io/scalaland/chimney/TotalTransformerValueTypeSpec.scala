package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.valuetypes.*

import scala.annotation.unused

class TotalTransformerValueTypeSpec extends ChimneySpec {

  test("AnyVals with private val and single accessor of different name/type are not considered value classes") {
    @unused val transformer: Transformer[String, Int] = (src: String) => src.length

    compileErrorsFixed("new NotAValueType(100).transformInto[UserName]").check(
      "derivation from value: io.scalaland.chimney.fixtures.valuetypes.NotAValueType to io.scalaland.chimney.fixtures.valuetypes.UserName is not supported in Chimney!"
    )
    compileErrorsFixed("new NotAValueType(100).transformInto[UserId]").check(
      "derivation from notavaluetype: io.scalaland.chimney.fixtures.valuetypes.NotAValueType to scala.Int is not supported in Chimney!"
    )
    compileErrorsFixed("""UserName("Batman").transformInto[NotAValueType]""").check(
      "derivation from value: io.scalaland.chimney.fixtures.valuetypes.UserName to io.scalaland.chimney.fixtures.valuetypes.NotAValueType is not supported in Chimney!"
    )
    compileErrorsFixed("UserId(100).transformInto[NotAValueType]").check(
      "derivation from value: io.scalaland.chimney.fixtures.valuetypes.UserId to io.scalaland.chimney.fixtures.valuetypes.NotAValueType is not supported in Chimney!"
    )
  }

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
    implicit val transformer: Transformer[String, Int] = (src: String) => src.length

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
