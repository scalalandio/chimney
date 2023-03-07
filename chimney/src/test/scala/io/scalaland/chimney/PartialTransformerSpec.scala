package io.scalaland.chimney

import utest.*

object PartialTransformerSpec extends TestSuite {

  private val pt1 = PartialTransformer[String, Int](str => partial.Result.fromValue(str.toInt))
  private val pt2 = PartialTransformer.fromFunction[String, Int](_.toInt)

  private val t1: Transformer[Int, Int] = _ * 2
  private val pt3 = PartialTransformer.liftTotal[Int, Int](t1)

  case class FooStr(s1: String, s2: String)
  case class Foo(s1: Int, s2: Int)

  private val pt4 = {
    implicit val strToInt: PartialTransformer[String, Int] = pt1
    PartialTransformer.derive[FooStr, Foo]
  }

  val tests = Tests {

    test("transform") {

      pt1.transform("100") ==> partial.Result.fromValue(100)
      pt1.transform("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))

      pt2.transform("100") ==> partial.Result.fromValue(100)
      pt2.transform("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))

      pt3.transform(100) ==> partial.Result.fromValue(200)

      pt4.transform(FooStr("100", "200")) ==> partial.Result.fromValue(Foo(100, 200))
      pt4.transform(FooStr("abc", "xyz")).asErrorPathMessageStrings ==> Iterable(
        ("s1", """For input string: "abc""""),
        ("s2", """For input string: "xyz"""")
      )
    }

    test("transformFailFast") {

      pt1.transformFailFast("100") ==> partial.Result.fromValue(100)
      pt1.transformFailFast("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))

      pt2.transformFailFast("100") ==> partial.Result.fromValue(100)
      pt2.transformFailFast("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))

      pt3.transformFailFast(100) ==> partial.Result.fromValue(200)

      pt4.transformFailFast(FooStr("100", "200")) ==> partial.Result.fromValue(Foo(100, 200))
      pt4.transformFailFast(FooStr("abc", "xyz")).asErrorPathMessageStrings ==> Iterable(
        ("s1", """For input string: "abc"""")
        // no second error due to fail fast mode
      )
    }
  }
}
