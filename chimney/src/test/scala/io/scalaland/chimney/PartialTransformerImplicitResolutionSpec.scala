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

  group("flag .enableTypeConstraintEvidence") {
    import merges.Nested

    test("should be disabled by default") {
      compileErrors(
        """
        def indirection[A, B](value: Nested[A])(implicit ev: A <:< B): partial.Result[Nested[B]] =
          value.transformIntoPartial[Nested[B]]

        indirection[String, String](Nested("value"))
        """
      ).arePresent()
    }

    test("should use <:< based-evidence") {
      def indirection1[A, B](value: Nested[A])(implicit ev: A <:< B): partial.Result[Nested[B]] =
        value.intoPartial[Nested[B]].enableTypeConstraintEvidence.transform
      def indirection2[A, B](value: Nested[A])(implicit ev: A <:< B): partial.Result[Nested[B]] = {
        implicit val cfg = TransformerConfiguration.default.enableTypeConstraintEvidence
        value.transformIntoPartial[Nested[B]]
      }

      indirection1[String, String](Nested("value")).asOption ==> Some(Nested("value"))
      indirection2[String, String](Nested("value")).asOption ==> Some(Nested("value"))
    }
  }

  group("flag .disableTypeConstraintEvidence") {
    import merges.Nested

    test("should disable globally enabled .enableTypeConstraintEvidence") {
      @unused implicit val cfg = TransformerConfiguration.default.enableTypeConstraintEvidence

      compileErrors(
        """
        def indirection[A, B](value: Nested[A])(implicit ev: A <:< B): partial.Result[Nested[B]] =
          value.intoPartial[Nested[B]].disableTypeConstraintEvidence.transform

        indirection[String, String](Nested("value"))
        """
      ).arePresent()
    }
  }

  group("flag .enableImplicitConversions") {

    import scala.language.implicitConversions

    implicit def convert(a: Int): String = a.toString

    test("should be disabled by default") {
      compileErrors("""10.transformIntoPartial[String]""").check(
        "Chimney can't derive transformation from scala.Int to java.lang.String",
        "java.lang.String",
        "derivation from int: scala.Int to java.lang.String is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should convert values when enabled") {
      10.intoPartial[String].enableImplicitConversions.transform.asOption ==> Some("10")

      locally {
        implicit val cfg = TransformerConfiguration.default.enableImplicitConversions
        10.transformIntoPartial[String].asOption ==> Some("10")
      }
    }
  }

  group("flag .disableImplicitConversions") {

    import scala.language.implicitConversions

    @unused implicit def convert(a: Int): String = a.toString

    test("should disable globally enabled .enableImplicitConversions") {
      @unused implicit val cfg = TransformerConfiguration.default.enableImplicitConversions

      compileErrors("""10.intoPartial[String].disableImplicitConversions.transform""").check(
        "Chimney can't derive transformation from scala.Int to java.lang.String",
        "java.lang.String",
        "derivation from int: scala.Int to java.lang.String is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }
}
