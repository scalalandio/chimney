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

  test("patch sequential-type with sequential-type of the same type, replacing the value") {
    CustomCollection.of(Bar("a")).patchUsing(Vector(Bar("b"))) ==> CustomCollection.of(Bar("b"))
    CustomCollection.of(Bar("a")).using(Vector(Bar("b"))).patch ==> CustomCollection.of(Bar("b"))

    Vector(Bar("a")).patchUsing(CustomCollection.of(Bar("b"))) ==> Vector(Bar("b"))
    Vector(Bar("a")).using(CustomCollection.of(Bar("b"))).patch ==> Vector(Bar("b"))

    CustomCollection.of(Bar("a")).patchUsing(CustomCollection.of(Bar("b"))) ==> CustomCollection.of(Bar("b"))
    CustomCollection.of(Bar("a")).using(CustomCollection.of(Bar("b"))).patch ==> CustomCollection.of(Bar("b"))
  }

  test("patch Map-type with Map-type of the same type, replacing the value") {
    CustomMap.of("id" -> Bar("a")).patchUsing(Map("id2" -> Bar("b"))) ==> CustomMap.of("id2" -> Bar("b"))
    CustomMap.of("id" -> Bar("a")).using(Map("id2" -> Bar("b"))).patch ==> CustomMap.of("id2" -> Bar("b"))

    Map("id" -> Bar("a")).patchUsing(CustomMap.of("id2" -> Bar("b"))) ==> Map("id2" -> Bar("b"))
    Map("id" -> Bar("a")).using(CustomMap.of("id2" -> Bar("b"))).patch ==> Map("id2" -> Bar("b"))

    CustomMap.of("id" -> Bar("a")).patchUsing(CustomMap.of("id2" -> Bar("b"))) ==> CustomMap.of("id2" -> Bar("b"))
    CustomMap.of("id" -> Bar("a")).using(CustomMap.of("id2" -> Bar("b"))).patch ==> CustomMap.of("id2" -> Bar("b"))
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

  group("flag .appendCollectionInPatch") {

    test("should patch sequential-type with sequential-type of the same type, with obj ++ patch") {
      CustomCollection
        .of(Bar("a"))
        .using(Vector(Bar("b")))
        .appendCollectionInPatch
        .patch ==> CustomCollection.of(Bar("a"), Bar("b"))
      Vector(Bar("a")).using(CustomCollection.of(Bar("b"))).appendCollectionInPatch.patch ==> Vector(Bar("a"), Bar("b"))
      CustomCollection
        .of(Bar("a"))
        .using(CustomCollection.of(Bar("b")))
        .appendCollectionInPatch
        .patch ==> CustomCollection.of(Bar("a"), Bar("b"))

      locally {
        implicit val cfg = PatcherConfiguration.default.appendCollectionInPatch

        CustomCollection.of(Bar("a")).patchUsing(Vector(Bar("b"))) ==> CustomCollection.of(Bar("a"), Bar("b"))
        Vector(Bar("a")).patchUsing(CustomCollection.of(Bar("b"))) ==> Vector(Bar("a"), Bar("b"))
        CustomCollection
          .of(Bar("a"))
          .patchUsing(CustomCollection.of(Bar("b"))) ==> CustomCollection.of(Bar("a"), Bar("b"))
      }
    }

    test("should patch Map-type with Map-type of the same type, with obj ++ patch") {
      CustomMap
        .of("id" -> Bar("a"))
        .using(Map("id2" -> Bar("b")))
        .appendCollectionInPatch
        .patch ==> CustomMap.of("id" -> Bar("a"), "id2" -> Bar("b"))
      Map("id" -> Bar("a"))
        .using(CustomMap.of("id2" -> Bar("b")))
        .appendCollectionInPatch
        .patch ==> Map("id" -> Bar("a"), "id2" -> Bar("b"))
      CustomMap
        .of("id" -> Bar("a"))
        .using(CustomMap.of("id2" -> Bar("b")))
        .appendCollectionInPatch
        .patch ==> CustomMap.of("id" -> Bar("a"), "id2" -> Bar("b"))

      locally {
        implicit val cfg = PatcherConfiguration.default.appendCollectionInPatch

        CustomMap
          .of("id" -> Bar("a"))
          .patchUsing(Map("id2" -> Bar("b"))) ==> CustomMap.of("id" -> Bar("a"), "id2" -> Bar("b"))
        Map("id" -> Bar("a")).patchUsing(CustomMap.of("id2" -> Bar("b"))) ==> Map("id" -> Bar("a"), "id2" -> Bar("b"))
        CustomMap
          .of("id" -> Bar("a"))
          .patchUsing(CustomMap.of("id2" -> Bar("b"))) ==> CustomMap.of("id" -> Bar("a"), "id2" -> Bar("b"))
      }
    }
  }
}
