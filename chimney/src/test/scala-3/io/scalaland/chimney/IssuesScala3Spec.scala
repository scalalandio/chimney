package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import scala.annotation.unused

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

  group("fix issue #758 (exported Type)") {
    object Issue758 {
      object Inner {
        case class Foo(a: Int, b: String)
        trait Bar {}
      }
      export Inner.{Bar, Foo}
    }

    @unused case class Foo2(a: Int)
    @unused case class Foo3(a: Int, b: String)
    @unused case class Bar2()

    import Issue758.Foo
    test("case class") {
      Foo2(1).into[Foo].withFieldConst(_.b, "value").transform ==> Foo(1, "value")
    }

    test("case class transformInto") {
      Foo3(1, "value").transformInto[Foo] ==> Foo(1, "value")
    }

    import Issue758.Bar
    test("trait") {
      val error = compileErrors("Bar2().into[Bar].transform")
      error.check(
        "Chimney can't derive transformation from",
        "derivation from bar2: io.scalaland.chimney.IssuesScala3Spec.Bar2 to io.scalaland.chimney.IssuesScala3Spec.Issue758.Inner.Bar is not supported in Chimney!"
      )
    }
  }

}
