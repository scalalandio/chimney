package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class TotalTransformerSingletonSpec extends ChimneySpec {

  test("""transformation into Null should always be possible""") {
    case class Example(a: Int, b: String)

    (Example(10, "test").transformInto[Null]: Any) ==> null
    (Example(10, "test").into[Null].transform: Any) ==> null
  }

  test("""transformation into Null should always be possible""") {
    case class Example(a: Int, b: String)
    case object Target

    Example(10, "test").transformInto[Target.type] ==> Target
    Example(10, "test").into[Target.type].transform ==> Target
  }
}
