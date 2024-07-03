package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class TotalTransformerProduct_2_13plusSyntaxSpec extends ChimneySpec {

  // literal singleton types are available only in 2.13 and 3
  test("transformation should automatically fill literal singleton type parameters") {
    case class Foo(value: String)

    Foo("something").transformInto[PolyBar[true]] ==> PolyBar("something", true)
    Foo("something").into[PolyBar[true]].transform ==> PolyBar("something", true)
    Foo("something").transformInto[PolyBar[1024]] ==> PolyBar("something", 1024)
    Foo("something").into[PolyBar[1024]].transform ==> PolyBar("something", 1024)
    Foo("something").transformInto[PolyBar[1024L]] ==> PolyBar("something", 1024L)
    Foo("something").into[PolyBar[1024L]].transform ==> PolyBar("something", 1024L)
    Foo("something").transformInto[PolyBar[3.14f]] ==> PolyBar("something", 3.14f)
    Foo("something").into[PolyBar[3.14f]].transform ==> PolyBar("something", 3.14f)
    Foo("something").transformInto[PolyBar[3.14]] ==> PolyBar("something", 3.14)
    Foo("something").into[PolyBar[3.14]].transform ==> PolyBar("something", 3.14)
    Foo("something").transformInto[PolyBar['@']] ==> PolyBar("something", '@')
    Foo("something").into[PolyBar['@']].transform ==> PolyBar("something", '@')
    Foo("something").transformInto[PolyBar["str"]] ==> PolyBar("something", "str")
    Foo("something").into[PolyBar["str"]].transform ==> PolyBar("something", "str")
  }
}
