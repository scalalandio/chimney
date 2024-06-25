package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class IsoProductSpec extends ChimneySpec {

  group("""setting .withFieldRenamed(_.from, _.to)""") {

    test("should be correctly forwarded to Transformers") {
      import products.Renames.{User, UserPLStd}

      implicit val iso: Iso[User, UserPLStd] = Iso
        .define[User, UserPLStd]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .buildIso

      User(1, "John", Some(27)).transformInto[UserPLStd] ==> UserPLStd(1, "John", Some(27))
      UserPLStd(1, "John", Some(27)).transformInto[User] ==> User(1, "John", Some(27))
    }
  }
}
