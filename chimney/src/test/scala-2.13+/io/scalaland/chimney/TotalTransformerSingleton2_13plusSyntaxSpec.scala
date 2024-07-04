package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class TotalTransformerSingleton2_13plusSyntaxSpec extends ChimneySpec {

  test("""transformation into literal-based singleton should always be possible""") {
    case class Example(a: Int, b: String)

    Example(10, "test").transformInto[true] ==> true
    Example(10, "test").into[true].transform ==> true
    Example(10, "test").transformInto[1024] ==> 1024
    Example(10, "test").into[1024].transform ==> 1024
    Example(10, "test").transformInto[1024L] ==> 1024L
    Example(10, "test").into[1024L].transform ==> 1024L
    Example(10, "test").transformInto[3.14f] ==> 3.14f
    Example(10, "test").into[3.14f].transform ==> 3.14f
    Example(10, "test").transformInto[3.14] ==> 3.14
    Example(10, "test").into[3.14].transform ==> 3.14
    Example(10, "test").transformInto['@'] ==> '@'
    Example(10, "test").into['@'].transform ==> '@'
    Example(10, "test").transformInto["str"] ==> "str"
    Example(10, "test").into["str"].transform ==> "str"
  }
}
