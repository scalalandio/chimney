package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

class PartialTransformerSpec extends ChimneySpec {

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

  val pt5 = {
    implicit val fooStrToFoo = pt4
    PartialTransformer.derive[List[FooStr], List[Foo]]
  }

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

    pt5.transform(List(FooStr("abc", "xyz"))).asErrorPathMessageStrings ==> Iterable(
      ("(0).s1", """For input string: "abc""""),
      ("(0).s2", """For input string: "xyz"""")
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

    pt5.transformFailFast(List(FooStr("abc", "xyz"))).asErrorPathMessageStrings ==> Iterable(
      ("(0).s1", """For input string: "abc"""")
    )
  }

  test("fail fast transform with dsl") {
    import io.scalaland.chimney.dsl.*

    implicit val strToInt: PartialTransformer[String, Int] = pt1

    FooStr("abc", "xyz").intoPartial[Foo].transformFailFast.asErrorPathMessageStrings ==> Iterable(
      ("s1", """For input string: "abc"""")
      // no second error due to fail fast mode
    )
  }

  test("map") {
    case class Length(length: Int)

    trait Prefix {
      def code: Int
    }

    object Prefix {

      def from(i: Int): Prefix = i match {
        case 1 => FooPrefix
        case 2 => BarPrefix
        case _ => NanPrefix
      }

      case object NanPrefix extends Prefix {
        override def code: Int = 0
      }

      case object FooPrefix extends Prefix {
        override def code: Int = 1
      }

      case object BarPrefix extends Prefix {
        override def code: Int = 2
      }
    }

    implicit val toLengthTransformer: PartialTransformer[String, Length] =
      pt1.map(Length.apply)

    implicit val toPrefixTransformer: PartialTransformer[String, Prefix] =
      pt1.map(Prefix.from)

    val id = "2"
    id.intoPartial[Length].transform ==> partial.Result.fromValue(Length(id.toInt))
    id.intoPartial[Prefix].transform ==> partial.Result.fromValue(Prefix.BarPrefix)
  }
}
