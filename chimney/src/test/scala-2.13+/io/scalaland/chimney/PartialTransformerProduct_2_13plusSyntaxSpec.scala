package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class PartialTransformerProduct_2_13plusSyntaxSpec extends ChimneySpec {

  // literal singleton types are available only in 2.13 and 3
  test("transformation should automatically fill literal singleton type parameters") {
    case class Foo(value: String)

    val expected = PolyBar("something", true)
    val result = Foo("something").transformIntoPartial[PolyBar[true]]
    result.asOption ==> Some(expected)
    result.asEither ==> Right(expected)
    result.asErrorPathMessageStrings ==> Iterable.empty
    val result2 = Foo("something").intoPartial[PolyBar[true]].transform
    result2.asOption ==> Some(expected)
    result2.asEither ==> Right(expected)
    result2.asErrorPathMessageStrings ==> Iterable.empty

    val expected2 = PolyBar("something", 1024)
    val result3 = Foo("something").transformIntoPartial[PolyBar[1024]]
    result3.asOption ==> Some(expected2)
    result3.asEither ==> Right(expected2)
    result3.asErrorPathMessageStrings ==> Iterable.empty
    val result4 = Foo("something").intoPartial[PolyBar[1024]].transform
    result4.asOption ==> Some(expected2)
    result4.asEither ==> Right(expected2)
    result4.asErrorPathMessageStrings ==> Iterable.empty

    val expected3 = PolyBar("something", 1024L)
    val result5 = Foo("something").transformIntoPartial[PolyBar[1024L]]
    result5.asOption ==> Some(expected3)
    result5.asEither ==> Right(expected3)
    result5.asErrorPathMessageStrings ==> Iterable.empty
    val result6 = Foo("something").intoPartial[PolyBar[1024L]].transform
    result6.asOption ==> Some(expected3)
    result6.asEither ==> Right(expected3)
    result6.asErrorPathMessageStrings ==> Iterable.empty

    val expected4 = PolyBar("something", 3.14f)
    val result7 = Foo("something").transformIntoPartial[PolyBar[3.14f]]
    result7.asOption ==> Some(expected4)
    result7.asEither ==> Right(expected4)
    result7.asErrorPathMessageStrings ==> Iterable.empty
    val result8 = Foo("something").intoPartial[PolyBar[3.14f]].transform
    result8.asOption ==> Some(expected4)
    result8.asEither ==> Right(expected4)
    result8.asErrorPathMessageStrings ==> Iterable.empty

    val expected5 = PolyBar("something", 3.14)
    val result9 = Foo("something").transformIntoPartial[PolyBar[3.14]]
    result9.asOption ==> Some(expected5)
    result9.asEither ==> Right(expected5)
    result9.asErrorPathMessageStrings ==> Iterable.empty
    val result10 = Foo("something").intoPartial[PolyBar[3.14]].transform
    result10.asOption ==> Some(expected5)
    result10.asEither ==> Right(expected5)
    result10.asErrorPathMessageStrings ==> Iterable.empty

    val expected6 = PolyBar("something", '@')
    val result11 = Foo("something").transformIntoPartial[PolyBar['@']]
    result11.asOption ==> Some(expected6)
    result11.asEither ==> Right(expected6)
    result11.asErrorPathMessageStrings ==> Iterable.empty
    val result12 = Foo("something").intoPartial[PolyBar['@']].transform
    result12.asOption ==> Some(expected6)
    result12.asEither ==> Right(expected6)
    result12.asErrorPathMessageStrings ==> Iterable.empty

    val expected7 = PolyBar("something", "str")
    val result13 = Foo("something").transformIntoPartial[PolyBar["str"]]
    result13.asOption ==> Some(expected7)
    result13.asEither ==> Right(expected7)
    result13.asErrorPathMessageStrings ==> Iterable.empty
    val result14 = Foo("something").intoPartial[PolyBar["str"]].transform
    result14.asOption ==> Some(expected7)
    result14.asEither ==> Right(expected7)
    result14.asErrorPathMessageStrings ==> Iterable.empty
  }
}
