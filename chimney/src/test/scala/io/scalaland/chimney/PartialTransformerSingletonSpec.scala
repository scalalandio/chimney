package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class PartialTransformerSingletonSpec extends ChimneySpec {

  test("""transformation into Null should always be possible""") {
    case class Example(a: Int, b: String)

    val result = Example(10, "test").transformIntoPartial[Null]
    result.asOption ==> Some(null)
    result.asEither ==> Right(null)
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = Example(10, "test").intoPartial[Null].transform
    result2.asOption ==> Some(null)
    result2.asEither ==> Right(null)
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }

  test("""transformation into Null should always be possible""") {
    case class Example(a: Int, b: String)
    case object Target

    val result = Example(10, "test").transformIntoPartial[Target.type]
    result.asOption ==> Some(Target)
    result.asEither ==> Right(Target)
    result.asErrorPathMessageStrings ==> Iterable.empty

    val result2 = Example(10, "test").intoPartial[Target.type].transform
    result2.asOption ==> Some(Target)
    result2.asEither ==> Right(Target)
    result2.asErrorPathMessageStrings ==> Iterable.empty
  }
}
