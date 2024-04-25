package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.valuetypes.*

import scala.annotation.unused

class PartialTransformerValueTypeSpec extends ChimneySpec {

  test("AnyVals with private val and single accessor of different name/type are not considered value classes") {
    @unused val transformer: Transformer[String, Int] = (src: String) => src.length

    compileErrors("new NotAValueType(100).transformIntoPartial[UserName]").check(
      "derivation from value: io.scalaland.chimney.fixtures.valuetypes.NotAValueType to io.scalaland.chimney.fixtures.valuetypes.UserName is not supported in Chimney!"
    )
    compileErrors("new NotAValueType(100).transformIntoPartial[UserId]").check(
      "derivation from notavaluetype: io.scalaland.chimney.fixtures.valuetypes.NotAValueType to scala.Int is not supported in Chimney!"
    )
    compileErrors("""UserName("Batman").transformIntoPartial[NotAValueType]""").check(
      "derivation from value: io.scalaland.chimney.fixtures.valuetypes.UserName to io.scalaland.chimney.fixtures.valuetypes.NotAValueType is not supported in Chimney!"
    )
    compileErrors("UserId(100).transformIntoPartial[NotAValueType]").check(
      "derivation from value: io.scalaland.chimney.fixtures.valuetypes.UserId to io.scalaland.chimney.fixtures.valuetypes.NotAValueType is not supported in Chimney!"
    )
  }

  test("AnyVals with private private constructor are not considered value classes") {
    @unused val transformer: Transformer[String, Int] = (src: String) => src.length

    compileErrors("AlsoNotAValueType.create(100).transformIntoPartial[UserName]").check(
      "derivation from value: io.scalaland.chimney.fixtures.valuetypes.AlsoNotAValueType to io.scalaland.chimney.fixtures.valuetypes.UserName is not supported in Chimney!"
    )
    compileErrors("AlsoNotAValueType.create(100).transformIntoPartial[UserId]").check(
      "derivation from alsonotavaluetype: io.scalaland.chimney.fixtures.valuetypes.AlsoNotAValueType to scala.Int is not supported in Chimney!"
    )
    compileErrors("""UserName("Batman").transformIntoPartial[AlsoNotAValueType]""").check(
      "derivation from username: io.scalaland.chimney.fixtures.valuetypes.UserName to io.scalaland.chimney.fixtures.valuetypes.AlsoNotAValueType is not supported in Chimney!"
    )
    compileErrors("UserId(100).transformIntoPartial[AlsoNotAValueType]").check(
      "derivation from userid: io.scalaland.chimney.fixtures.valuetypes.UserId to io.scalaland.chimney.fixtures.valuetypes.AlsoNotAValueType is not supported in Chimney!"
    )
  }

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
    batman.transformIntoPartial[UserId].asOption.get ==> UserId(batman.length)
    UserWithName(abc).transformIntoPartial[UserWithUserId].asOption.get ==> UserWithUserId(UserId(abc.length))
  }

  test("transforming value class(member type: `S`) to value class(member type: 'T') if 'T'=>'S' exists") {
    implicit val transformer = new Transformer[String, Int] {
      override def transform(src: String): Int = src.length
    }

    val batman = "Batman"
    val abc = "abc"
    UserName(batman).transformIntoPartial[UserId].asOption.get ==> UserId(batman.length)
    UserWithUserName(UserName(abc)).transformIntoPartial[UserWithUserId].asOption.get ==> UserWithUserId(
      UserId(abc.length)
    )
  }

  test("transform value classes with overrides as product types") {
    UserName("Batman")
      .intoPartial[UserNameAlias]
      .withFieldConstPartial(_.value, partial.Result.fromValue("not Batman"))
      .transform
      .asOption
      .get ==> UserNameAlias("not Batman")
    UserName("Batman")
      .intoPartial[UserNameAlias]
      .withFieldConstPartial(_.value, partial.Result.fromEmpty)
      .transform
      .asOption ==> None
    UserName("Batman")
      .intoPartial[UserNameAlias]
      .withFieldComputedPartial(_.value, un => partial.Result.fromValue(un.value.toUpperCase))
      .transform
      .asOption
      .get ==> UserNameAlias("BATMAN")

    import fixtures.nestedpath.*

    NestedProduct(UserName("Batman"))
      .intoPartial[NestedValueClass[UserNameAlias]]
      .withFieldConst(_.value.value, "not Batman")
      .transform
      .asOption
      .get ==> NestedValueClass(UserNameAlias("not Batman"))
    NestedValueClass(UserName("Batman"))
      .intoPartial[NestedProduct[UserNameAlias]]
      .withFieldComputed(_.value.value, un => un.value.value.toUpperCase)
      .transform
      .asOption
      .get ==> NestedProduct(UserNameAlias("BATMAN"))
  }

  test("flags overrides aren't enough to skip on AnyVal rule application") {
    implicit val transformer = new Transformer[String, Int] {
      override def transform(src: String): Int = src.length
    }

    val batman = "Batman"
    val abc = "abc"
    UserName(batman).intoPartial[UserId].enableMethodAccessors.transform.asOption.get ==> UserId(batman.length)
    UserWithUserName(UserName(abc))
      .intoPartial[UserWithUserId]
      .enableMethodAccessors
      .transform
      .asOption
      .get ==> UserWithUserId(UserId(abc.length))
  }
}
