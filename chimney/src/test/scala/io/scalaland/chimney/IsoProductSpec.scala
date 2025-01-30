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

  group(
    "setting .withSealedSubtypeRenamed[FromSubtype, ToSubtype] should be correctly forwarded to Transformer/PartialTransformer"
  ) {

    import fixtures.renames.Subtypes.*

    test("transform sealed hierarchy's subtype into user-provided subtype") {

      implicit val iso: Iso[Foo3, Bar] =
        Iso.define[Foo3, Bar].withSealedSubtypeRenamed[Foo3.Bazz.type, Bar.Baz.type].buildIso

      (Foo3.Baz: Foo3).transformInto[Bar] ==> Bar.Baz
      (Foo3.Bazz: Foo3).transformInto[Bar] ==> Bar.Baz
      (Bar.Baz: Bar).transformInto[Foo3] ==> Foo3.Bazz
    }
  }

  group(
    "setting .withEnumCaseRenamed[FromSubtype, ToSubtype] should be correctly forwarded to Transformer/PartialTransformer"
  ) {

    import fixtures.renames.Subtypes.*

    test("transform sealed hierarchy's subtype into user-provided subtype") {

      implicit val iso: Iso[Foo3, Bar] =
        Iso.define[Foo3, Bar].withEnumCaseRenamed[Foo3.Bazz.type, Bar.Baz.type].buildIso

      (Foo3.Baz: Foo3).transformInto[Bar] ==> Bar.Baz
      (Foo3.Bazz: Foo3).transformInto[Bar] ==> Bar.Baz
      (Bar.Baz: Bar).transformInto[Foo3] ==> Foo3.Bazz
    }
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
