package io.scalaland.chimney

class DebugImportSpec extends ChimneySpec {

  import DebugImportSpec.*

  test("import io.scalaland.chimney.debug._ places a LogDerivation in scope and does not break derivation") {
    import io.scalaland.chimney.debug.*
    import io.scalaland.chimney.dsl.*

    // The import's whole contract: the implicit is visible to the expansion (which turns on the derivation log -
    // rendered to compiler info output, not asserted on, same as .enableMacrosLogging) and the result is unchanged.
    val _ = implicitly[LogDerivation]
    Bar("abc", 10).transformInto[Foo] ==> Foo("abc", 10)
  }
}

object DebugImportSpec {
  case class Foo(x: String, y: Int)
  case class Bar(x: String, y: Int)
}
