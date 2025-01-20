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

  group("flag .ignoreNoneInPatch") {

    test("should patch Option-type with Option-type of the same type, with patch.orElse(obj)") {
      Option(Bar("a")).using(None: Option[Bar]).ignoreNoneInPatch.patch ==> Option(Bar("a"))
      Option(Bar("a")).using(Option(Bar("b"))).ignoreNoneInPatch.patch ==> Option(Bar("b"))
      (None: Option[Bar]).using(Option(Bar("b"))).ignoreNoneInPatch.patch ==> Option(Bar("b"))
      (None: Option[Bar]).using(None: Option[Bar]).ignoreNoneInPatch.patch ==> None

      locally {
        implicit val ctx = PatcherConfiguration.default.ignoreNoneInPatch

        Option(Bar("a")).patchUsing(Option(Bar("b"))) ==> Option(Bar("b"))
        Option(Bar("a")).patchUsing(None: Option[Bar]) ==> Option(Bar("a"))
        (None: Option[Bar]).patchUsing(Option(Bar("b"))) ==> Option(Bar("b"))
        (None: Option[Bar]).patchUsing(None: Option[Bar]) ==> None
      }
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
        implicit val ctx = PatcherConfiguration.default.ignoreLeftInPatch

        Either.cond(true, Bar("a"), "fail").patchUsing(Either.cond(true, Bar("b"), "fall")) ==> Right(Bar("b"))
        Either.cond(true, Bar("a"), "fail").patchUsing(Either.cond(false, Bar("b"), "fall")) ==> Right(Bar("a"))
        Either.cond(false, Bar("a"), "fail").patchUsing(Either.cond(true, Bar("b"), "fall")) ==> Right(Bar("b"))
        Either.cond(false, Bar("a"), "fail").patchUsing(Either.cond(false, Bar("b"), "fall")) ==> Left("fail")
      }
    }
  }
}
object PatcherStdLibSpec {

  case class Foo(value: String, extra: Int)
  case class Bar(value: String)
}
