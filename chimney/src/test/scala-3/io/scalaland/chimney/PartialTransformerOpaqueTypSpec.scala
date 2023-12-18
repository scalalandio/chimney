package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class PartialTransformerOpaqueTypSpec extends ChimneySpec {

  test("opaque types with manually provided constructor should work as product types") {
    import opaquetypes.*

    given PartialTransformer[Foo, Bar] = PartialTransformer
      .define[Foo, Bar]
      .withConstructorPartial { (value: String) =>
        partial.Result.fromEitherString(Bar.parse(value))
      }
      .buildTransformer

    Foo("10").transformIntoPartial[Bar].asEither ==> Bar.parse("10")
  }
}
