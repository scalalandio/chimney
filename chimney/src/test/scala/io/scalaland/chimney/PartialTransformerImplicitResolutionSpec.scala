package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.annotation.unused

class PartialTransformerImplicitResolutionSpec extends ChimneySpec {

  test("transform using implicit Total Transformer for whole transformation when available") {
    import products.Domain1.*
    implicit def instance: Transformer[UserName, String] = userNameToStringTransformer

    val result = UserName("Batman").intoPartial[String].transform
    result.asOption ==> Some("BatmanT")
    result.asEither ==> Right("BatmanT")
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = UserName("Batman").transformIntoPartial[String]
    result2.asOption ==> Some("BatmanT")
    result2.asEither ==> Right("BatmanT")
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("transform using implicit Partial Transformer for whole transformation when available") {
    import products.Domain1.*
    implicit def instance: PartialTransformer[UserName, String] = userNameToStringPartialTransformer

    val result = UserName("Batman").intoPartial[String].transform
    result.asOption ==> Some("BatmanT")
    result.asEither ==> Right("BatmanT")
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = UserName("Batman").transformIntoPartial[String]
    result2.asOption ==> Some("BatmanT")
    result2.asEither ==> Right("BatmanT")
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("transform using implicit Total Transformer for nested field when available") {
    import products.Domain1.*
    implicit def instance: Transformer[UserName, String] = userNameToStringTransformer

    val expected = UserDTO("123", "BatmanT")

    val result = User("123", UserName("Batman")).intoPartial[UserDTO].transform
    result.asOption ==> Some(expected)
    result.asEither ==> Right(expected)
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = User("123", UserName("Batman")).transformIntoPartial[UserDTO]
    result2.asOption ==> Some(expected)
    result2.asEither ==> Right(expected)
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("transform using implicit Partial Transformer for nested field when available") {
    import products.Domain1.*
    implicit def instance: PartialTransformer[UserName, String] = userNameToStringPartialTransformer

    val expected = UserDTO("123", "BatmanT")

    val result = User("123", UserName("Batman")).intoPartial[UserDTO].transform
    result.asOption ==> Some(expected)
    result.asEither ==> Right(expected)
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = User("123", UserName("Batman")).transformIntoPartial[UserDTO]
    result2.asOption ==> Some(expected)
    result2.asEither ==> Right(expected)
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("ignore implicit Partial Transformer if an override is present") {
    import trip.*

    @unused implicit def instance: PartialTransformer[Person, User] = PartialTransformer.derive

    val expected = User("Not John", 10, 140)

    val result = Person("John", 10, 140).intoPartial[User].withFieldConst(_.name, "Not John").transform
    result.asOption ==> Some(expected)
    result.asEither ==> Right(expected)
    result.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("ignore implicit Partial Transformer if an local flag is present but not if only implicit flag is present") {
    import trip.*

    @unused implicit def instance: PartialTransformer[Person, UserWithDefault] =
      PartialTransformer.fromFunction(person => UserWithDefault(person.name, person.age, 38))

    @unused implicit val cfg = TransformerConfiguration.default.enableDefaultValues

    val expectedInstance = UserWithDefault("John", 10, 38)

    val result = Person("John", 10, 140).transformIntoPartial[UserWithDefault]
    result.asOption ==> Some(expectedInstance)
    result.asEither ==> Right(expectedInstance)
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = Person("John", 10, 140).intoPartial[UserWithDefault].transform
    result2.asOption ==> Some(expectedInstance)
    result2.asEither ==> Right(expectedInstance)
    result2.asErrorPathMessageStrings ==> Iterable.empty

    val expectedDefault = UserWithDefault("John", 10)

    val result3 = Person("John", 10, 140).intoPartial[UserWithDefault].enableDefaultValues.transform
    result3.asOption ==> Some(expectedDefault)
    result3.asEither ==> Right(expectedDefault)
    result3.asErrorPathMessageStrings ==> Iterable.empty
  }
}
