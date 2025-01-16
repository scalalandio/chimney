package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class PatcherIntegrationsSpec extends ChimneySpec {

  import TotalTransformerIntegrationsSpec.*
  import PatcherStdLibSpec.*

  test("patch Option-type with Option-type of the same type, replacing the value") {
    Possible(Bar("a")).patchUsing(Possible(Bar("b"))) ==> Possible(Bar("b"))
    Possible(Bar("a")).using(Possible(Bar("b"))).patch ==> Possible(Bar("b"))

    Possible(Bar("a")).patchUsing(Possible.Nope: Possible[Bar]) ==> Possible.Nope
    Possible(Bar("a")).using(Possible.Nope: Possible[Bar]).patch ==> Possible.Nope

    (Possible.Nope: Possible[Bar]).patchUsing(Possible(Bar("b"))) ==> Possible(Bar("b"))
    (Possible.Nope: Possible[Bar]).using(Possible(Bar("b"))).patch ==> Possible(Bar("b"))

    (Possible.Nope: Possible[Bar]).patchUsing(Possible.Nope: Possible[Bar]) ==> Possible.Nope
    (Possible.Nope: Possible[Bar]).using(Possible.Nope: Possible[Bar]).patch ==> Possible.Nope
  }

  group("flag .ignoreNoneInPatch") {

    test("should patch Option-type with Option-type of the same type, with patch.orElse(obj)") {
      Possible(Bar("a")).using(Possible.Nope: Possible[Bar]).ignoreNoneInPatch.patch ==> Possible(Bar("a"))
      Possible(Bar("a")).using(Possible(Bar("b"))).ignoreNoneInPatch.patch ==> Possible(Bar("b"))
      (Possible.Nope: Possible[Bar]).using(Possible(Bar("b"))).ignoreNoneInPatch.patch ==> Possible(Bar("b"))
      (Possible.Nope: Possible[Bar]).using(Possible.Nope: Possible[Bar]).ignoreNoneInPatch.patch ==> Possible.Nope

      locally {
        implicit val ctx = PatcherConfiguration.default.ignoreNoneInPatch

        Possible(Bar("a")).patchUsing(Possible(Bar("b"))) ==> Possible(Bar("b"))
        Possible(Bar("a")).patchUsing(Possible.Nope: Possible[Bar]) ==> Possible(Bar("a"))
        (Possible.Nope: Possible[Bar]).patchUsing(Possible(Bar("b"))) ==> Possible(Bar("b"))
        (Possible.Nope: Possible[Bar]).patchUsing(Possible.Nope: Possible[Bar]) ==> Possible.Nope
      }
    }
  }
}
