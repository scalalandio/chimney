package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class CodecImplicitResolutionSpec extends ChimneySpec {

  test("encode using Total Transformer from Codec when available") {
    import products.Domain1.*
    implicit def codec: Codec[User, UserDTO] = locally {
      implicit def usernameToString: Transformer[UserName, String] =
        userNameToStringTransformer
      implicit def stringToUsername: PartialTransformer[String, UserName] =
        PartialTransformer.liftTotal(value => UserName(value))
      Codec.derive[User, UserDTO]
    }

    User("1", UserName("name")).transformInto[UserDTO] ==> UserDTO("1", "nameT")
  }

  test("decode using Partial Transformer from Codec when available") {
    import products.Domain1.*
    implicit def codec: Codec[User, UserDTO] = locally {
      implicit def usernameToString: Transformer[UserName, String] =
        userNameToStringTransformer
      implicit def stringToUsername: PartialTransformer[String, UserName] =
        PartialTransformer.liftTotal(value => UserName(value + "T"))
      Codec.derive[User, UserDTO]
    }

    UserDTO("1", "name").transformIntoPartial[User].asOption ==> Some(User("1", UserName("nameT")))
  }
}
