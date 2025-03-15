package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class IsoImplicitResolutionSpec extends ChimneySpec {

  /* Fix on Scala 3
  test("convert using the first Total Transformer from Iso when available") {
    import products.Domain1.*
    implicit def iso: Iso[User, UserDTO] = locally {
      implicit def usernameToString: Transformer[UserName, String] =
        userNameToStringTransformer
      implicit def stringToUsername: Transformer[String, UserName] = value => UserName(value + "T")
      Iso.derive[User, UserDTO]
    }

    User("1", UserName("name")).transformInto[UserDTO] ==> UserDTO("1", "nameT")
  }

  test("convert using the second Total Transformer from Iso when available") {
    import products.Domain1.*
    implicit def iso: Iso[User, UserDTO] = locally {
      implicit def usernameToString: Transformer[UserName, String] =
        userNameToStringTransformer
      implicit def stringToUsername: Transformer[String, UserName] = value => UserName(value + "T")
      Iso.derive[User, UserDTO]
    }

    UserDTO("1", "name").transformIntoPartial[User].asOption ==> Some(User("1", UserName("nameT")))
  }
   */
}
