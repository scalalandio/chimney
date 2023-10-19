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
}
