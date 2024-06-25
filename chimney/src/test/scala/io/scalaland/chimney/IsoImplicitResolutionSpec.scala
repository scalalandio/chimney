package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class IsoImplicitResolutionSpec extends ChimneySpec {

  test("convert using the first Total Transformer from Iso when available") {
    import products.Domain1.*
    implicit def usernameToString: Transformer[UserName, String] =
      userNameToStringTransformer
    implicit def stringToUsername: Transformer[String, UserName] = value => UserName(value + "T")
    implicit def iso: Iso[User, UserDTO] = Iso.derive

    User("1", UserName("name")).transformInto[UserDTO] ==> UserDTO("1", "nameT")
  }

  test("convert using the second Total Transformer from Iso when available") {
    import products.Domain1.*
    implicit def usernameToString: Transformer[UserName, String] =
      userNameToStringTransformer
    implicit def stringToUsername: Transformer[String, UserName] = value => UserName(value + "T")
    implicit def iso: Iso[User, UserDTO] = Iso.derive

    UserDTO("1", "name").transformIntoPartial[User].asOption ==> Some(User("1", UserName("nameT")))
  }
}
