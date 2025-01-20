package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class PatcherStdLibSpec extends ChimneySpec {

  import PatcherStdLibSpec.*

  test("patch Option-type with Option-type of the same type, replacing the value") {
    Option(Bar("a")).patchUsing(Option(Bar("b"))) ==> Option(Bar("b"))
    Option(Bar("a")).using(Option(Bar("b"))).patch ==> Option(Bar("b"))

    Option(Bar("a")).patchUsing(None: Option[Bar]) ==> None
    Option(Bar("a")).using(None: Option[Bar]).patch ==> None

    (None: Option[Bar]).patchUsing(Option(Bar("b"))) ==> Option(Bar("b"))
    (None: Option[Bar]).using(Option(Bar("b"))).patch ==> Option(Bar("b"))

    (None: Option[Bar]).patchUsing(None: Option[Bar]) ==> None
    (None: Option[Bar]).using(None: Option[Bar]).patch ==> None
  }

  test("patch Either-type with Either-type of the same type, replacing the value") {
    Either.cond(true, Bar("a"), "fail").patchUsing(Either.cond(true, Bar("b"), "fall")) ==> Right(Bar("b"))
    Either.cond(true, Bar("a"), "fail").using(Either.cond(true, Bar("b"), "fall")).patch ==> Right(Bar("b"))

    Either.cond(true, Bar("a"), "fail").patchUsing(Either.cond(false, Bar("b"), "fall")) ==> Left("fall")
    Either.cond(true, Bar("a"), "fail").using(Either.cond(false, Bar("b"), "fall")).patch ==> Left("fall")

    Either.cond(false, Bar("a"), "fail").patchUsing(Either.cond(true, Bar("b"), "fall")) ==> Right(Bar("b"))
    Either.cond(false, Bar("a"), "fail").using(Either.cond(true, Bar("b"), "fall")).patch ==> Right(Bar("b"))

    Either.cond(false, Bar("a"), "fail").patchUsing(Either.cond(false, Bar("b"), "fall")) ==> Left("fall")
    Either.cond(false, Bar("a"), "fail").using(Either.cond(false, Bar("b"), "fall")).patch ==> Left("fall")
  }

  test("patch sequential-type with sequential-type of the same type, replacing the value") {
    List(Bar("a")).patchUsing(Vector(Bar("b"))) ==> List(Bar("b"))
    List(Bar("a")).using(Vector(Bar("b"))).patch ==> List(Bar("b"))
  }

  test("patch Map-type with Map-type of the same type, replacing the value") {
    Map("id" -> Bar("a")).patchUsing(Map("id2" -> Bar("b"))) ==> Map("id2" -> Bar("b"))
    Map("id" -> Bar("a")).using(Map("id2" -> Bar("b"))).patch ==> Map("id2" -> Bar("b"))
  }

  group("flag .ignoreNoneInPatch") {

    test("should patch Option-type with Option-type of the same type, with patch.orElse(obj)") {
      Option(Bar("a")).using(None: Option[Bar]).ignoreNoneInPatch.patch ==> Option(Bar("a"))
      Option(Bar("a")).using(Option(Bar("b"))).ignoreNoneInPatch.patch ==> Option(Bar("b"))
      (None: Option[Bar]).using(Option(Bar("b"))).ignoreNoneInPatch.patch ==> Option(Bar("b"))
      (None: Option[Bar]).using(None: Option[Bar]).ignoreNoneInPatch.patch ==> None

      locally {
        implicit val cfg = PatcherConfiguration.default.ignoreNoneInPatch

        Option(Bar("a")).patchUsing(Option(Bar("b"))) ==> Option(Bar("b"))
        Option(Bar("a")).patchUsing(None: Option[Bar]) ==> Option(Bar("a"))
        (None: Option[Bar]).patchUsing(Option(Bar("b"))) ==> Option(Bar("b"))
        (None: Option[Bar]).patchUsing(None: Option[Bar]) ==> None
      }
    }
  }

  group("flag .ignoreNoneInPatch") {

    test("should disable globally enabled .ignoreNoneInPatch") {
      implicit val cfg = PatcherConfiguration.default.ignoreNoneInPatch

      Option(Bar("a")).using(Option(Bar("b"))).clearOnNoneInPatch.patch ==> Option(Bar("b"))
      Option(Bar("a")).using(None: Option[Bar]).clearOnNoneInPatch.patch ==> None
      (None: Option[Bar]).using(Option(Bar("b"))).clearOnNoneInPatch.patch ==> Option(Bar("b"))
      (None: Option[Bar]).using(None: Option[Bar]).clearOnNoneInPatch.patch ==> None
    }
  }

  group("flag .ignoreLeftInPatch") {

    test("should patch Either-type with Either-type of the same type, with patch.orElse(obj)") {
      Either
        .cond(true, Bar("a"), "fail")
        .using(Either.cond(true, Bar("b"), "fall"))
        .ignoreLeftInPatch
        .patch ==> Right(Bar("b"))
      Either
        .cond(true, Bar("a"), "fail")
        .using(Either.cond(false, Bar("b"), "fall"))
        .ignoreLeftInPatch
        .patch ==> Right(Bar("a"))
      Either
        .cond(false, Bar("a"), "fail")
        .using(Either.cond(true, Bar("b"), "fall"))
        .ignoreLeftInPatch
        .patch ==> Right(Bar("b"))
      Either
        .cond(false, Bar("a"), "fail")
        .using(Either.cond(false, Bar("b"), "fall"))
        .ignoreLeftInPatch
        .patch ==> Left("fail")

      locally {
        implicit val cfg = PatcherConfiguration.default.ignoreLeftInPatch

        Either.cond(true, Bar("a"), "fail").patchUsing(Either.cond(true, Bar("b"), "fall")) ==> Right(Bar("b"))
        Either.cond(true, Bar("a"), "fail").patchUsing(Either.cond(false, Bar("b"), "fall")) ==> Right(Bar("a"))
        Either.cond(false, Bar("a"), "fail").patchUsing(Either.cond(true, Bar("b"), "fall")) ==> Right(Bar("b"))
        Either.cond(false, Bar("a"), "fail").patchUsing(Either.cond(false, Bar("b"), "fall")) ==> Left("fail")
      }
    }
  }

  group("flag .useLeftOnLeftInPatch") {

    test("should disable globally enabled .ignoreLeftInPatch") {
      implicit val cfg = PatcherConfiguration.default.ignoreLeftInPatch

      Either
        .cond(true, Bar("a"), "fail")
        .using(Either.cond(true, Bar("b"), "fall"))
        .useLeftOnLeftInPatch
        .patch ==> Right(Bar("b"))
      Either
        .cond(true, Bar("a"), "fail")
        .using(Either.cond(false, Bar("b"), "fall"))
        .useLeftOnLeftInPatch
        .patch ==> Left("fall")
      Either
        .cond(false, Bar("a"), "fail")
        .using(Either.cond(true, Bar("b"), "fall"))
        .useLeftOnLeftInPatch
        .patch ==> Right(Bar("b"))
      Either
        .cond(false, Bar("a"), "fail")
        .using(Either.cond(false, Bar("b"), "fall"))
        .useLeftOnLeftInPatch
        .patch ==> Left("fall")
    }
  }

  group("flag .appendCollectionInPatch") {

    test("should patch sequential-type with sequential-type of the same type, with obj ++ patch") {
      List(Bar("a")).using(Vector(Bar("b"))).appendCollectionInPatch.patch ==> List(Bar("a"), Bar("b"))

      locally {
        implicit val cfg = PatcherConfiguration.default.appendCollectionInPatch

        List(Bar("a")).patchUsing(Vector(Bar("b"))) ==> List(Bar("a"), Bar("b"))
      }
    }

    test("should patch Map-type with Map-type of the same type, with obj ++ patch") {
      Map("id" -> Bar("a")).using(Map("id2" -> Bar("b"))).appendCollectionInPatch.patch ==> Map(
        "id" -> Bar("a"),
        "id2" -> Bar("b")
      )

      locally {
        implicit val cfg = PatcherConfiguration.default.appendCollectionInPatch

        Map("id" -> Bar("a")).patchUsing(Map("id2" -> Bar("b"))) ==> Map("id" -> Bar("a"), "id2" -> Bar("b"))
      }
    }
  }

  group("flag .overrideCollectionInPatch") {

    test("should disable globally enabled .appendCollectionInPatch") {
      implicit val cfg = PatcherConfiguration.default.appendCollectionInPatch

      List(Bar("a")).using(Vector(Bar("b"))).overrideCollectionInPatch.patch ==> List(Bar("b"))
      Map("id" -> Bar("a")).using(Map("id2" -> Bar("b"))).overrideCollectionInPatch.patch ==> Map("id2" -> Bar("b"))
    }
  }
}
object PatcherStdLibSpec {

  case class Foo(value: String, extra: Int)
  case class Bar(value: String)
}
