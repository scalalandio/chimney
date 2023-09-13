package io.scalaland.chimney

import io.scalaland.chimney.dsl.TransformerConfiguration

import scala.annotation.unused

class PartialTransformerDslSeparationSpec extends ChimneySpec {

  group("importing dsl.*") {
    import dsl.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should enable both automatic derivation") {
      Foo("test").transformIntoPartial[Bar].asOption.get ==> Bar("test")
    }

    test("should enable inlined derivation") {
      Foo("test").intoPartial[Bar].transform.asOption.get ==> Bar("test")
    }

    test("should enable summoning declared instances") {
      implicit val transformer: PartialTransformer[Foo, Bar] =
        PartialTransformer.define[Foo, Bar].withFieldConst(_.baz, "test2").buildTransformer

      Foo("test").transformIntoPartial[Bar].asOption.get ==> Bar("test2")
      Foo("test").intoPartial[Bar].transform.asOption.get ==> Bar("test2")
    }
  }

  group("importing auto.*") {
    import auto.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should enable automatic derivation") {
      // TODO: requires implicit conflict resolution settings since it generates identity transformer using macros
      implicit val cfg = TransformerConfiguration.default.enableImplicitConflictResolution(dsl.PreferTotalTransformer)
      implicitly[PartialTransformer[Foo, Bar]].transform(Foo("test")).asOption.get ==> Bar("test")
    }

    test("should not enable inlined derivation") {
      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").intoPartial[Bar].transform""").arePresent()
    }

    test("should not enable summoning declared instances") {
      @unused implicit val transformer: PartialTransformer[Foo, Bar] =
        PartialTransformer.define[Foo, Bar].withFieldConst(_.baz, "test2").buildTransformer

      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").transformIntoPartial[Bar]""").arePresent()
    }
  }

  group("importing inlined.*") {
    import inlined.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should not enable automatic derivation") {
      // format of missing implicit differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").transformIntoPartial[Bar]""").arePresent()
    }

    test("should enable inlined derivation") {
      Foo("test").intoPartial[Bar].transform.asOption.get ==> Bar("test")
    }

    test("should not enable summoning declared instances") {
      @unused implicit val transformer: Transformer[Foo, Bar] =
        Transformer.define[Foo, Bar].withFieldConst(_.baz, "test2").buildTransformer

      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").transformIntoPartial[Bar]""").arePresent()
    }
  }

  group("importing syntax.*") {
    import syntax.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should not enable automatic derivation") {
      // format of missing implicit differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").transformIntoPartial[Bar]""").arePresent()
    }

    test("should not enable inlined derivation") {
      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").intoPartial[Bar].transform""").arePresent()
    }

    test("should enable summoning declared instances") {
      implicit val transformer: PartialTransformer[Foo, Bar] = PartialTransformer.derive[Foo, Bar]

      Foo("test").transformIntoPartial[Bar].asOption.get ==> Bar("test")
    }
  }
}
