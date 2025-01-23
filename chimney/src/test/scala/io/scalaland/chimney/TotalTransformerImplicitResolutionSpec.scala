package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.annotation.unused

class TotalTransformerImplicitResolutionSpec extends ChimneySpec {

  test("transform using implicit Total Transformer for whole transformation when available") {
    import products.Domain1.*

    implicit def instance: Transformer[UserName, String] = userNameToStringTransformer

    UserName("Batman").into[String].transform ==> "BatmanT"
    UserName("Batman").transformInto[String] ==> "BatmanT"
  }

  test("transform using implicit Total Transformer for nested field when available") {
    import products.Domain1.*

    implicit def instance: Transformer[UserName, String] = userNameToStringTransformer

    User("123", UserName("Batman")).into[UserDTO].transform ==> UserDTO("123", "BatmanT")
    User("123", UserName("Batman")).transformInto[UserDTO] ==> UserDTO("123", "BatmanT")
  }

  test("transform case classes with the same fields' number, names and types without modifiers") {
    import trip.*

    Person("John", 10, 140).into[User].transform ==> User("John", 10, 140)
    Person("John", 10, 140).transformInto[User] ==> User("John", 10, 140)
  }

  test("ignore implicit Total Transformer if an override is present") {
    import trip.*

    @unused implicit def instance: Transformer[Person, User] = Transformer.derive

    Person("John", 10, 140).into[User].withFieldConst(_.name, "Not John").transform ==> User("Not John", 10, 140)
  }

  test("ignore implicit Total Transformer if an local flag is present but not if only implicit flag is present") {
    import trip.*

    @unused implicit def instance: Transformer[Person, UserWithDefault] =
      person => UserWithDefault(person.name, person.age, 38)

    @unused implicit val cfg = TransformerConfiguration.default.enableDefaultValues

    Person("John", 10, 140).transformInto[UserWithDefault] ==> UserWithDefault("John", 10, 38)
    Person("John", 10, 140).into[UserWithDefault].transform ==> UserWithDefault("John", 10, 38)
    Person("John", 10, 140).into[UserWithDefault].enableDefaultValues.transform ==> UserWithDefault("John", 10)
  }

  group("flag .enableTypeConstraintEvidence") {
    import merges.Nested

    test("should be disabled by default") {
      compileErrors(
        """
        def indirection[A, B](value: Nested[A])(implicit ev: A <:< B): Nested[B] =
          value.transformInto[Nested[B]]

        indirection[String, String](Nested("value"))
        """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.A] to io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.B]",
        "io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.B]",
        "value: io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.B - can't derive transformation from value: io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.A in source type io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.A]",
        "io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.B",
        "derivation from nested.value: io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.A to io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.B is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should use <:< based-evidence") {
      def indirection1[A, B](value: Nested[A])(implicit ev: A <:< B): Nested[B] =
        value.into[Nested[B]].enableTypeConstraintEvidence.transform
      def indirection2[A, B](value: Nested[A])(implicit ev: A <:< B): Nested[B] = {
        implicit val cfg = TransformerConfiguration.default.enableTypeConstraintEvidence
        value.transformInto[Nested[B]]
      }

      indirection1[String, String](Nested("value")) ==> Nested("value")
      indirection2[String, String](Nested("value")) ==> Nested("value")
    }
  }

  group("flag .disableTypeConstraintEvidence") {
    import merges.Nested

    test("should disable globally enabled .enableTypeConstraintEvidence") {
      @unused implicit val cfg = TransformerConfiguration.default.enableTypeConstraintEvidence

      compileErrors(
        """
        def indirection[A, B](value: Nested[A])(implicit ev: A <:< B): Nested[B] =
          value.into[Nested[B]].disableTypeConstraintEvidence.transform

        indirection[String, String](Nested("value"))
        """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.A] to io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.B]",
        "io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.B]",
        "value: io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.B - can't derive transformation from value: io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.A in source type io.scalaland.chimney.fixtures.merges.Nested[io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.A]",
        "io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.B",
        "derivation from nested.value: io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.A to io.scalaland.chimney.TotalTransformerImplicitResolutionSpec.B is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }
}
