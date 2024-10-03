package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class IssuesScala3Spec extends ChimneySpec {

  test("fix issue #592 (givens in companion)") {
    case class Foo(a: Int, b: String)
    case class Bar(a: Int, b: String)
    case class Baz(a: Int)
    object Foo {
      given totalTransformer: Transformer[Foo, Bar] =
        Transformer.define[Foo, Bar].withFieldConst(_.a, 10).buildTransformer
      given partialTransformer: PartialTransformer[Foo, Bar] =
        PartialTransformer.define[Foo, Bar].withFieldConst(_.a, 20).buildTransformer
      given patcher: Patcher[Foo, Baz] = (_, baz) => Foo(baz.a, "patched")
    }

    Foo(1, "value").transformInto[Bar] ==> Bar(10, "value")
    Foo(1, "value").transformIntoPartial[Bar].asOption.get ==> Bar(20, "value")
    Foo(1, "value").patchUsing(Baz(30)) ==> Foo(30, "patched")
  }
}
