package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.utils.OptionUtils.StringOps

object PartialTransformerErrorPathSpec {
  sealed trait Foo
  object Foo {
    case class Baz(field: String) extends Foo
  }

  sealed trait Bar
  object Bar {
    case class Baz(field: Int) extends Bar
  }
}

class PartialTransformerErrorPathSpec extends ChimneySpec {

  implicit val intParserOpt: PartialTransformer[String, Int] =
    PartialTransformer(_.parseInt.toPartialResult)

  import PartialTransformerErrorPathSpec.*

  test("root error should not contain any path element") {
    val result = "error".transformIntoPartial[Int]
    result.asOption ==> None
    result.asEither.isLeft ==> true
    result.asErrorPathMessages ==> Seq(
      "" -> partial.ErrorMessage.EmptyValue
    )
  }

  test("case class field error should contain path to the failed field") {
    case class Foo(a: String, b: String, c: InnerFoo, d: String)
    case class InnerFoo(d: String, e: String)
    case class Bar(a: Int, b: Int, c: InnerBar, d: String)
    case class InnerBar(d: Int, e: Int)

    val result = Foo("mmm", "nnn", InnerFoo("lll", "jjj"), "d").transformIntoPartial[Bar]
    result.asOption ==> None
    result.asEither.isLeft ==> true
    result.asErrorPathMessages ==> Iterable(
      "a" -> partial.ErrorMessage.EmptyValue,
      "b" -> partial.ErrorMessage.EmptyValue,
      "c.d" -> partial.ErrorMessage.EmptyValue,
      "c.e" -> partial.ErrorMessage.EmptyValue
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "a" -> "empty value",
      "b" -> "empty value",
      "c.d" -> "empty value",
      "c.e" -> "empty value"
    )
  }

  test("case classes with field error coming from setting should contain path to the source field used in setting") {
    case class Foo(inner: InnerFoo)
    case class InnerFoo(str: String)

    case class Bar(inner: InnerBar, b: Int)
    case class InnerBar(int1: Int, int2: Int, double: Double)

    implicit val innerT: PartialTransformer[InnerFoo, InnerBar] = PartialTransformer
      .define[InnerFoo, InnerBar]
      .withFieldRenamed(_.str, _.int1)
      .withFieldConstPartial(_.int2, intParserOpt.transform("notint"))
      .withFieldComputedPartial(_.double, foo => partial.Result.fromOption(foo.str.parseDouble))
      .buildTransformer

    val result = Foo(InnerFoo("aaa"))
      .intoPartial[Bar]
      .withFieldConstPartial(_.b, intParserOpt.transform("bbb"))
      .transform
    result.asErrorPathMessages ==> Iterable(
      "inner.str" -> partial.ErrorMessage.EmptyValue,
      "inner.int2" -> partial.ErrorMessage.EmptyValue,
      "inner.double" -> partial.ErrorMessage.EmptyValue,
      "b" -> partial.ErrorMessage.EmptyValue
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "inner.str" -> "empty value",
      "inner.int2" -> "empty value",
      "inner.double" -> "empty value",
      "b" -> "empty value"
    )
  }

  test("Java Bean accessors error should contain path to the failed getter") {
    class Foo(a: String, b: String) {
      def getA: String = a
      def getB: String = b
    }
    case class Bar(a: Int, b: Int)

    val result = new Foo("a", "b").intoPartial[Bar].enableBeanGetters.transform
    result.asErrorPathMessages ==> Iterable(
      "getA" -> partial.ErrorMessage.EmptyValue,
      "getB" -> partial.ErrorMessage.EmptyValue
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "getA" -> "empty value",
      "getB" -> "empty value"
    )
  }

  // FIXME: ProductToProduct doesn't handle tuples yet
  /*
  test("tuple field's error should contain path to the failed field") {
    val result = ("a", "b").transformIntoPartial[(Int, Int)]
    result.asOption ==> None
    result.asEither.isLeft ==> true
    result.asErrorPathMessages ==> Iterable(
      "_1" -> partial.ErrorMessage.EmptyValue,
      "_2" -> partial.ErrorMessage.EmptyValue
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "_1" -> "empty value",
      "_2" -> "empty value"
    )
  }
   */

  test("sealed hierarchy's error should add path to failed subtype") {
    val result = (Foo.Baz("fail"): Foo).transformIntoPartial[Bar]
    result.asErrorPathMessages ==> Iterable(
      "field" -> partial.ErrorMessage.EmptyValue
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "field" -> "empty value"
    )
  }

  test("flat List's errors should contain indices to failed values") {
    val result = List("a", "b", "c").transformIntoPartial[List[Int]]
    result.asErrorPathMessages ==> Iterable(
      "(0)" -> partial.ErrorMessage.EmptyValue,
      "(1)" -> partial.ErrorMessage.EmptyValue,
      "(2)" -> partial.ErrorMessage.EmptyValue
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "(0)" -> "empty value",
      "(1)" -> "empty value",
      "(2)" -> "empty value"
    )
  }

  test("nested List's errors should contain indices to failed values") {
    case class Foo(list: List[String])
    case class Bar(list: List[Int])

    val result = Foo(List("a", "b", "c")).transformIntoPartial[Bar]
    result.asErrorPathMessages ==> Iterable(
      "list(0)" -> partial.ErrorMessage.EmptyValue,
      "list(1)" -> partial.ErrorMessage.EmptyValue,
      "list(2)" -> partial.ErrorMessage.EmptyValue
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "list(0)" -> "empty value",
      "list(1)" -> "empty value",
      "list(2)" -> "empty value"
    )
  }

  test("flat Array's errors should contain indices to failed values") {
    val result = Array("a", "b", "c").transformIntoPartial[Array[Int]]
    result.asErrorPathMessages ==> Iterable(
      "(0)" -> partial.ErrorMessage.EmptyValue,
      "(1)" -> partial.ErrorMessage.EmptyValue,
      "(2)" -> partial.ErrorMessage.EmptyValue
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "(0)" -> "empty value",
      "(1)" -> "empty value",
      "(2)" -> "empty value"
    )
  }

  test("nested Array's errors should contain indices to failed values") {
    case class Foo(list: Array[String])
    case class Bar(list: Array[Int])

    val result = Foo(Array("a", "b", "c")).transformIntoPartial[Bar]
    result.asErrorPathMessages ==> Iterable(
      "list(0)" -> partial.ErrorMessage.EmptyValue,
      "list(1)" -> partial.ErrorMessage.EmptyValue,
      "list(2)" -> partial.ErrorMessage.EmptyValue
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "list(0)" -> "empty value",
      "list(1)" -> "empty value",
      "list(2)" -> "empty value"
    )
  }

  test("flat Map's error should contain key/value that failed conversion") {
    val result = Map("1" -> "x", "y" -> "20").transformIntoPartial[Map[Int, Int]]
    result.asOption ==> None
    result.asEither.isLeft ==> true
    result.asErrorPathMessages ==> Iterable(
      "(1)" -> partial.ErrorMessage.EmptyValue,
      "keys(y)" -> partial.ErrorMessage.EmptyValue
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "(1)" -> "empty value",
      "keys(y)" -> "empty value"
    )
  }

  test("case class-nested Map's error should contain path to key/value that failed conversion") {
    case class EnvelopeStr(map: Map[String, String])
    case class EnvelopeInt(map: Map[Int, Int])

    val result = EnvelopeStr(Map("1" -> "x", "y" -> "20")).transformIntoPartial[EnvelopeInt]
    result.asOption ==> None
    result.asEither.isLeft ==> true
    result.asErrorPathMessages ==> Iterable(
      "map(1)" -> partial.ErrorMessage.EmptyValue,
      "map.keys(y)" -> partial.ErrorMessage.EmptyValue
    )
    result.asErrorPathMessageStrings ==> Iterable(
      "map(1)" -> "empty value",
      "map.keys(y)" -> "empty value"
    )
  }
}
