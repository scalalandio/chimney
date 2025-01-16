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
}
object PatcherStdLibSpec {

  case class Foo(value: String, extra: Int)
  case class Bar(value: String)
}
