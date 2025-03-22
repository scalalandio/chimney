package io.scalaland.chimney

import scala.annotation.unused

class TotalTransformerDslsSeparationSpec extends ChimneySpec {

  group("importing dsl.*") {
    import dsl.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should enable both automatic derivation") {
      Foo("test").transformInto[Bar] ==> Bar("test")
    }

    test("should enable inlined derivation") {
      Foo("test").into[Bar].transform ==> Bar("test")
    }

    test("should enable summoning declared instances") {
      implicit val transformer: Transformer[Foo, Bar] =
        Transformer.define[Foo, Bar].withFieldConst(_.baz, "test2").buildTransformer

      Foo("test").transformInto[Bar] ==> Bar("test2")
      Foo("test").into[Bar].transform ==> Bar("test2")
    }
  }

  group("importing auto.*") {
    import auto.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should enable automatic derivation") {
      implicitly[Transformer[Foo, Bar]].transform(Foo("test")) ==> Bar("test")
    }

    test("should not enable inlined derivation") {
      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrors("""Foo("test").into[Bar].transform""").arePresent()
    }

    test("should not enable summoning declared instances") {
      @unused implicit val transformer: Transformer[Foo, Bar] =
        Transformer.define[Foo, Bar].withFieldConst(_.baz, "test2").buildTransformer

      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrors("""Foo("test").transformInto[Bar]""").arePresent()
    }
  }

  group("importing inlined.*") {
    import inlined.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should not enable automatic derivation") {
      // format of missing implicit differ between 2 and 3 and we cannot rely on it
      compileErrors("""Foo("test").transformInto[Bar]""").arePresent()
    }

    test("should enable inlined derivation") {
      Foo("test").into[Bar].transform ==> Bar("test")
    }

    test("should not enable summoning declared instances") {
      @unused implicit val transformer: Transformer[Foo, Bar] =
        Transformer.define[Foo, Bar].withFieldConst(_.baz, "test2").buildTransformer

      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrors("""Foo("test").transformInto[Bar]""").arePresent()
    }
  }

  group("importing syntax.*") {
    import syntax.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should not enable automatic derivation") {
      // format of missing implicit differ between 2 and 3 and we cannot rely on it
      compileErrors("""Foo("test").transformInto[Bar]""").arePresent()
    }

    test("should not enable inlined derivation") {
      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrors("""Foo("test").into[Bar].transform""").arePresent()
    }

    test("should enable summoning declared instances") {
      implicit val transformer: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar]

      Foo("test").transformInto[Bar] ==> Bar("test")
    }
  }
}
