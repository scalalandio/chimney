package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class PartialTransformerSingleton3SyntaxSpec extends ChimneySpec {

  test("""transformation into Enum.Value.type should always be possible""") {
    import TotalTransformerSingleton3SyntaxSpec.*

    val result = (Input.Foo: Input).transformIntoPartial[Output.Bar.type]
    result.asOption ==> Some(Output.Bar)
    result.asEither ==> Right(Output.Bar)
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = (Input.Foo: Input).intoPartial[Output.Bar.type].transform
    result2.asOption ==> Some(Output.Bar)
    result2.asEither ==> Right(Output.Bar)
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }
}
