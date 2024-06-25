package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class CodecProductSpec extends ChimneySpec {

  test("""setting .withFieldRenamed(_.from, _.to) should be correctly forwarded to Transformer/PartialTransformer""") {
    import products.Renames.{User, UserPLStd}

    implicit val iso: Codec[User, UserPLStd] = Codec
      .define[User, UserPLStd]
      .withFieldRenamed(_.name, _.imie)
      .withFieldRenamed(_.age, _.wiek)
      .buildCodec

    User(1, "John", Some(27)).transformInto[UserPLStd] ==> UserPLStd(1, "John", Some(27))
    UserPLStd(1, "John", Some(27)).transformIntoPartial[User].asOption ==> Some(User(1, "John", Some(27)))
  }

  test("""flags should be correctly forwarded to Transformers""") {
    import products.Defaults.{Target, Target2}

    implicit val codec: Codec[Target, Target2] = Codec
      .define[Target, Target2]
      .enableDefaultValues
      .buildCodec

    Target(1, "n", 7.7).transformInto[Target2] ==> Target2(10, "y", 7.7)
    Target2(1, "n", 7.7).transformIntoPartial[Target].asOption ==> Some(Target(10, "y", 7.7))
  }
}
