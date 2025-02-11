package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

import scala.annotation.unused

class PartialMergingValueTypeSpec extends ChimneySpec {

  import TotalMergingValueTypeSpec.*

  test("unwrap both From and fallbacks AnyVals if source type is AnyVal") {
    ValueType(Foo("a"))
      .intoPartial[Baz]
      .withFallback(Bar("b"))
      .transform
      .asOption ==> Some(Baz("a", "b"))
    ValueType(Foo("a"))
      .intoPartial[ValueType[Baz]]
      .withFallback(Bar("b"))
      .transform
      .asOption ==> Some(ValueType(Baz("a", "b")))
    ValueType(Foo("a"))
      .intoPartial[Baz]
      .withFallback(ValueType(Bar("b")))
      .transform
      .asOption ==> Some(Baz("a", "b"))
    ValueType(Foo("a"))
      .intoPartial[ValueType[Baz]]
      .withFallback(ValueType(Bar("b")))
      .transform
      .asOption ==> Some(ValueType(Baz("a", "b")))
  }

  group("flag .enableNonAnyValWrappers") {

    test("should be turned off by default") {
      compileErrors("""WrapperType(Foo("a")).intoPartial[Baz].withFallback(Bar("b")).transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo] to io.scalaland.chimney.TotalMergingValueTypeSpec.Baz",
        "io.scalaland.chimney.TotalMergingValueTypeSpec.Baz",
        "  a: java.lang.String - no accessor named a in source type io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo]",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      compileErrors("""WrapperType(Foo("a")).intoPartial[WrapperType[Baz]].withFallback(Bar("b")).transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo] to io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Baz]",
        "io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Baz]",
        "  value: io.scalaland.chimney.TotalMergingValueTypeSpec.Baz - can't derive transformation from value: io.scalaland.chimney.TotalMergingValueTypeSpec.Foo in source type io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo]",
        "io.scalaland.chimney.TotalMergingValueTypeSpec.Baz",
        "  b: java.lang.String - no accessor named b in source type io.scalaland.chimney.TotalMergingValueTypeSpec.Foo",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      compileErrors("""WrapperType(Foo("a")).intoPartial[Baz].withFallback(WrapperType(Bar("b"))).transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo] to io.scalaland.chimney.TotalMergingValueTypeSpec.Baz",
        "io.scalaland.chimney.TotalMergingValueTypeSpec.Baz",
        "  a: java.lang.String - no accessor named a in source type io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo]",
        "  b: java.lang.String - no accessor named b in source type io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo]",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should enable unwrapping both From and fallbacks AnyVals/wrappers if source type is AnyVal/wrapper") {
      WrapperType(Foo("a"))
        .intoPartial[Baz]
        .withFallback(Bar("b"))
        .enableNonAnyValWrappers
        .transform
        .asOption ==> Some(Baz("a", "b"))
      WrapperType(Foo("a"))
        .intoPartial[WrapperType[Baz]]
        .withFallback(Bar("b"))
        .enableNonAnyValWrappers
        .transform
        .asOption ==> Some(WrapperType(Baz("a", "b")))
      WrapperType(Foo("a"))
        .intoPartial[Baz]
        .withFallback(WrapperType(Bar("b")))
        .enableNonAnyValWrappers
        .transform
        .asOption ==> Some(Baz("a", "b"))

      locally {
        implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers

        WrapperType(Foo("a"))
          .intoPartial[Baz]
          .withFallback(Bar("b"))
          .transform
          .asOption ==> Some(Baz("a", "b"))
        WrapperType(Foo("a"))
          .intoPartial[WrapperType[Baz]]
          .withFallback(Bar("b"))
          .transform
          .asOption ==> Some(WrapperType(Baz("a", "b")))
        WrapperType(Foo("a"))
          .intoPartial[Baz]
          .withFallback(WrapperType(Bar("b")))
          .transform
          .asOption ==> Some(Baz("a", "b"))
      }
    }
  }

  group("flag .disableNonAnyValWrappers") {

    test("should disable globally enabled .enableNonAnyValWrappers") {
      @unused implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers

      compileErrors(
        """WrapperType(Foo("a")).intoPartial[Baz].withFallback(Bar("b")).disableNonAnyValWrappers.transform"""
      )
        .check(
          "Chimney can't derive transformation from io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo] to io.scalaland.chimney.TotalMergingValueTypeSpec.Baz",
          "io.scalaland.chimney.TotalMergingValueTypeSpec.Baz",
          "  a: java.lang.String - no accessor named a in source type io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo]",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
      compileErrors(
        """WrapperType(Foo("a")).intoPartial[WrapperType[Baz]].withFallback(Bar("b")).disableNonAnyValWrappers.transform"""
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo] to io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Baz]",
        "io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Baz]",
        "  value: io.scalaland.chimney.TotalMergingValueTypeSpec.Baz - can't derive transformation from value: io.scalaland.chimney.TotalMergingValueTypeSpec.Foo in source type io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo]",
        "io.scalaland.chimney.TotalMergingValueTypeSpec.Baz",
        "  b: java.lang.String - no accessor named b in source type io.scalaland.chimney.TotalMergingValueTypeSpec.Foo",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      compileErrors(
        """WrapperType(Foo("a")).intoPartial[Baz].withFallback(WrapperType(Bar("b"))).disableNonAnyValWrappers.transform"""
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo] to io.scalaland.chimney.TotalMergingValueTypeSpec.Baz",
        "io.scalaland.chimney.TotalMergingValueTypeSpec.Baz",
        "  a: java.lang.String - no accessor named a in source type io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo]",
        "  b: java.lang.String - no accessor named b in source type io.scalaland.chimney.TotalMergingValueTypeSpec.WrapperType[io.scalaland.chimney.TotalMergingValueTypeSpec.Foo]",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }
}
