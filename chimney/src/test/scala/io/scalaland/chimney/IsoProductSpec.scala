package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class IsoProductSpec extends ChimneySpec {

  test("""setting .withFieldRenamed(_.from, _.to) should be correctly forwarded to Transformers""") {

    import products.Renames.{User, UserPLStd}

    implicit val iso: Iso[User, UserPLStd] = Iso
      .define[User, UserPLStd]
      .withFieldRenamed(_.name, _.imie)
      .withFieldRenamed(_.age, _.wiek)
      .buildIso

    User(1, "John", Some(27)).transformInto[UserPLStd] ==> UserPLStd(1, "John", Some(27))
    UserPLStd(1, "John", Some(27)).transformInto[User] ==> User(1, "John", Some(27))
  }

  test("""flags should be correctly forwarded to Transformers""") {
    import products.Defaults.{Target, Target2}

    implicit val iso: Iso[Target, Target2] = Iso
      .define[Target, Target2]
      .enableDefaultValues
      .buildIso

    Target(1, "n", 7.7).transformInto[Target2] ==> Target2(10, "y", 7.7)
    Target2(1, "n", 7.7).transformInto[Target] ==> Target(10, "y", 7.7)
  }
}
