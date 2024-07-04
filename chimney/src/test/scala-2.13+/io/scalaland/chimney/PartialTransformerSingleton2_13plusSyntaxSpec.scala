package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class PartialTransformerSingleton2_13plusSyntaxSpec extends ChimneySpec {

  test("""transformation into literal-based singleton should always be possible""") {
    case class Example(a: Int, b: String)

    val expected = true
    val result = Example(10, "test").transformIntoPartial[true]
    result.asOption ==> Some(expected)
    result.asEither ==> Right(expected)
    result.asErrorPathMessageStrings ==> Iterable.empty
    val result2 = Example(10, "test").intoPartial[true].transform
    result2.asOption ==> Some(expected)
    result2.asEither ==> Right(expected)
    result2.asErrorPathMessageStrings ==> Iterable.empty

    val expected2 = 1024
    val result3 = Example(10, "test").transformIntoPartial[1024]
    result3.asOption ==> Some(expected2)
    result3.asEither ==> Right(expected2)
    result3.asErrorPathMessageStrings ==> Iterable.empty
    val result4 = Example(10, "test").intoPartial[1024].transform
    result4.asOption ==> Some(expected2)
    result4.asEither ==> Right(expected2)
    result4.asErrorPathMessageStrings ==> Iterable.empty

    val expected3 = 1024L
    val result5 = Example(10, "test").transformIntoPartial[1024L]
    result5.asOption ==> Some(expected3)
    result5.asEither ==> Right(expected3)
    result5.asErrorPathMessageStrings ==> Iterable.empty
    val result6 = Example(10, "test").intoPartial[1024L].transform
    result6.asOption ==> Some(expected3)
    result6.asEither ==> Right(expected3)
    result6.asErrorPathMessageStrings ==> Iterable.empty

    val expected4 = 3.14f
    val result7 = Example(10, "test").transformIntoPartial[3.14f]
    result7.asOption ==> Some(expected4)
    result7.asEither ==> Right(expected4)
    result7.asErrorPathMessageStrings ==> Iterable.empty
    val result8 = Example(10, "test").intoPartial[3.14f].transform
    result8.asOption ==> Some(expected4)
    result8.asEither ==> Right(expected4)
    result8.asErrorPathMessageStrings ==> Iterable.empty

    val expected5 = 3.14
    val result9 = Example(10, "test").transformIntoPartial[3.14]
    result9.asOption ==> Some(expected5)
    result9.asEither ==> Right(expected5)
    result9.asErrorPathMessageStrings ==> Iterable.empty
    val result10 = Example(10, "test").intoPartial[3.14].transform
    result10.asOption ==> Some(expected5)
    result10.asEither ==> Right(expected5)
    result10.asErrorPathMessageStrings ==> Iterable.empty

    val expected6 = '@'
    val result11 = Example(10, "test").transformIntoPartial['@']
    result11.asOption ==> Some(expected6)
    result11.asEither ==> Right(expected6)
    result11.asErrorPathMessageStrings ==> Iterable.empty
    val result12 = Example(10, "test").intoPartial['@'].transform
    result12.asOption ==> Some(expected6)
    result12.asEither ==> Right(expected6)
    result12.asErrorPathMessageStrings ==> Iterable.empty

    val expected7 = "str"
    val result13 = Example(10, "test").transformIntoPartial["str"]
    result13.asOption ==> Some(expected7)
    result13.asEither ==> Right(expected7)
    result13.asErrorPathMessageStrings ==> Iterable.empty
    val result14 = Example(10, "test").intoPartial["str"].transform
    result14.asOption ==> Some(expected7)
    result14.asEither ==> Right(expected7)
    result14.asErrorPathMessageStrings ==> Iterable.empty
  }
}
