package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class TotalTransformerProduct3SyntaxSpec extends ChimneySpec {

  // Inference on value.this (without upcasting) works only with Scala 3
  test("transformation should automatically fill Enum.Value.type type parameters") {
    case class Foo(value: String)
    import TotalTransformerProduct3SyntaxSpec.*

    Foo("something").transformInto[PolyBar[Type.Value.type]] ==> PolyBar("something", Type.Value)
    Foo("something").into[PolyBar[Type.Value.type]].transform ==> PolyBar("something", Type.Value)
  }
}
object TotalTransformerProduct3SyntaxSpec {

  // Same issue as with https://github.com/scalalandio/chimney/pull/533 / https://github.com/scala/scala3/issues/20349
  // / https://github.com/scala/scala3/issues/19825.
  enum Type {
    case Value
  }
}
