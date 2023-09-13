package io.scalaland.chimney

import scala.annotation.unused

class PatcherDslSeparationSpec extends ChimneySpec {

  group("importing dsl.*") {
    import dsl.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should enable both automatic derivation") {
      Foo("test").patchUsing(Bar("test2")) ==> Foo("test2")
    }

    test("should enable inlined derivation") {
      Foo("test").using(Bar("test2")).patch ==> Foo("test2")
    }

    test("should enable summoning declared instances") {
      implicit val patcher: Patcher[Foo, Bar] =
        (foo: Foo, bar: Bar) => Foo(baz = "test3")

      Foo("test").patchUsing(Bar("test2")) ==> Foo("test3")
      Foo("test").using(Bar("test2")).patch ==> Foo("test3")
    }
  }

  group("importing auto.*") {
    import auto.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should enable automatic derivation") {
      implicitly[Patcher[Foo, Bar]].patch(Foo("test"), Bar("test2")) ==> Foo("test2")
    }

    test("should not enable inlined derivation") {
      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").using(Bar("test2")).patch""").arePresent()
    }

    test("should not enable summoning declared instances") {
      @unused implicit val patcher: Patcher[Foo, Bar] =
        Patcher.derive[Foo, Bar]

      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").patchUsing(Bar("test2"))""").arePresent()
    }
  }

  group("importing inlined.*") {
    import inlined.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should not enable automatic derivation") {
      // format of missing implicit differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").patchUsing(Bar("test2"))""").arePresent()
    }

    test("should enable inlined derivation") {
      Foo("test").using(Bar("test2")).patch ==> Foo("test2")
    }

    test("should not enable summoning declared instances") {
      @unused implicit val patcher: Patcher[Foo, Bar] =
        Patcher.define[Foo, Bar].buildPatcher

      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").patchUsing(Bar("test2"))""").arePresent()
    }
  }

  group("importing syntax.*") {
    import syntax.*

    case class Foo(baz: String)
    case class Bar(baz: String)

    test("should not enable automatic derivation") {
      // format of missing implicit differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").patchUsing(Bar("test2"))""").arePresent()
    }

    test("should not enable inlined derivation") {
      // format of missing method differ between 2 and 3 and we cannot rely on it
      compileErrorsFixed("""Foo("test").using(Bar("test2")).patch""").arePresent()
    }

    test("should enable summoning declared instances") {
      implicit val patcher: Patcher[Foo, Bar] = Patcher.derive[Foo, Bar]

      Foo("test").patchUsing(Bar("test2")) ==> Foo("test2")
    }
  }
}
