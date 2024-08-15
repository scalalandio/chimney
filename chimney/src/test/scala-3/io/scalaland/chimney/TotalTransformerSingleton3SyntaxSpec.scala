package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class TotalTransformerSingleton3SyntaxSpec extends ChimneySpec {

  test("""transformation into Enum.Value.type should always be possible""") {
    import TotalTransformerSingleton3SyntaxSpec.*

    (Input.Foo: Input).transformInto[Output.Bar.type] ==> Output.Bar
    (Input.Foo: Input).into[Output.Bar.type].transform ==> Output.Bar
  }
}
object TotalTransformerSingleton3SyntaxSpec {

  // Same issue as with https://github.com/scalalandio/chimney/pull/533 / https://github.com/scala/scala3/issues/20349
  // / https://github.com/scala/scala3/issues/19825.
  enum Input {
    case Foo
  }
  enum Output {
    case Bar
  }
}
