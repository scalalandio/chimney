package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class CodecProductSpec extends ChimneySpec {

  test("""setting .withFieldRenamed(_.from, _.to) should be correctly forwarded to Transformer/PartialTransformer""") {
    import products.Renames.{User, UserPLStd}

    implicit val codec: Codec[User, UserPLStd] = Codec
      .define[User, UserPLStd]
      .withFieldRenamed(_.name, _.imie)
      .withFieldRenamed(_.age, _.wiek)
      .buildCodec

    User(1, "John", Some(27)).transformInto[UserPLStd] ==> UserPLStd(1, "John", Some(27))
    UserPLStd(1, "John", Some(27)).transformIntoPartial[User].asOption ==> Some(User(1, "John", Some(27)))
  }

  group(
    "settings .withSealedSubtypeRenamed[FromSubtype, ToSubtype] should be correctly forwarded to Transformer/PartialTransformer"
  ) {

    import fixtures.renames.Subtypes.*

    test("transform sealed hierarchy's subtype into user-provided subtype") {

      implicit val codec: Codec[Foo3, Bar] =
        Codec.define[Foo3, Bar].withSealedSubtypeRenamed[Foo3.Bazz.type, Bar.Baz.type].buildCodec

      (Foo3.Baz: Foo3).transformInto[Bar] ==> Bar.Baz
      (Foo3.Bazz: Foo3).transformInto[Bar] ==> Bar.Baz
      (Bar.Baz: Bar).transformIntoPartial[Foo3].asOption ==> Some(Foo3.Bazz)
    }
  }

  group(
    "settings .withEnumCaseRenamed[FromSubtype, ToSubtype] should be correctly forwarded to Transformer/PartialTransformer"
  ) {

    import fixtures.renames.Subtypes.*

    test("transform sealed hierarchy's subtype into user-provided subtype") {

      implicit val codec: Codec[Foo3, Bar] =
        Codec.define[Foo3, Bar].withEnumCaseRenamed[Foo3.Bazz.type, Bar.Baz.type].buildCodec

      (Foo3.Baz: Foo3).transformInto[Bar] ==> Bar.Baz
      (Foo3.Bazz: Foo3).transformInto[Bar] ==> Bar.Baz
      (Bar.Baz: Bar).transformIntoPartial[Foo3].asOption ==> Some(Foo3.Bazz)
    }
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
