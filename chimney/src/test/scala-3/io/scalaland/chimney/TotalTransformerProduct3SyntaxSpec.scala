package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class TotalTransformerProduct3SyntaxSpec extends ChimneySpec {

  // inference on value.this (without upcasting) works only with Scala 3
  test("transformation should automatically fill Enum.Value.type type parameters") {
    case class Foo(value: String)
    import TotalTransformerProduct3SyntaxSpec.*

    Foo("something").transformInto[PolyBar[Type.Value.type]] ==> PolyBar("something", Type.Value)
    Foo("something").into[PolyBar[Type.Value.type]].transform ==> PolyBar("something", Type.Value)
  }
}
object TotalTransformerProduct3SyntaxSpec {

  enum Type:
    case Value
}
