package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class CodecProductSpec extends ChimneySpec {

  group("""setting .withFieldRenamed(_.from, _.to)""") {

    test("should be correctly forwarded to Transformer/PartialTransformer") {
      import products.Renames.{User, UserPLStd}

      implicit val iso: Codec[User, UserPLStd] = Codec
        .define[User, UserPLStd]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .buildCodec

      User(1, "John", Some(27)).transformInto[UserPLStd] ==> UserPLStd(1, "John", Some(27))
      UserPLStd(1, "John", Some(27)).transformIntoPartial[User].asOption ==> Some(User(1, "John", Some(27)))
    }
  }
}
