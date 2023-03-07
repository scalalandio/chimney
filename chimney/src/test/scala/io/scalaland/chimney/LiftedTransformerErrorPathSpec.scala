package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.utils.OptionUtils.*
import utest.*

object LiftedTransformerErrorPathSpec extends TestSuite {

  // TODO: class to its subtype

  type ErrorOr[+A] = Either[List[TransformationError[String]], A]

  val readableError: List[TransformationError[String]] => List[(String, String)] =
    _.map(error => error.showErrorPath -> error.message)

  implicit val intParserOpt: TransformerF[ErrorOr, String, Int] =
    str => str.parseInt.fold[ErrorOr[Int]](Left(List(TransformationError(s"Can't parse int from $str"))))(Right(_))

  sealed trait Foo
  object Foo {
    case class Baz(field: String) extends Foo
  }
  sealed trait Bar
  object Bar {
    case class Baz(field: Int) extends Bar
  }

  val tests = Tests {

    test("root error should not contain any path element") {
      "error".transformIntoF[ErrorOr, Int].left.map(readableError) ==> Left(
        List(
          "" -> s"Can't parse int from error"
        )
      )
    }

    test("case class field error should contain path to the failed field") {
      case class Foo(a: String, b: String, c: InnerFoo, d: String)
      case class InnerFoo(d: String, e: String)
      case class Bar(a: Int, b: Int, c: InnerBar, d: String)
      case class InnerBar(d: Int, e: Int)

      Foo("mmm", "nnn", InnerFoo("lll", "jjj"), "d").transformIntoF[ErrorOr, Bar].left.map(readableError) ==> Left(
        List(
          "a" -> "Can't parse int from mmm",
          "b" -> "Can't parse int from nnn",
          "c.d" -> "Can't parse int from lll",
          "c.e" -> "Can't parse int from jjj"
        )
      )
    }

    test("case classes with field error coming from setting should contain path to the source field used in setting") {
      case class Foo(inner: InnerFoo)
      case class InnerFoo(str: String)

      case class Bar(inner: InnerBar, b: Int)
      case class InnerBar(int1: Int, int2: Int, double: Double)

      implicit val innerT: TransformerF[ErrorOr, InnerFoo, InnerBar] = TransformerF
        .define[ErrorOr, InnerFoo, InnerBar]
        .withFieldRenamed(_.str, _.int1)
        .withFieldConstF(_.int2, intParserOpt.transform("notint"))
        .withFieldComputedF(
          _.double,
          foo =>
            foo.str.parseDouble
              .fold[ErrorOr[Double]](Left(List(TransformationError(s"Can't parse int from ${foo.str}"))))(Right(_))
        )
        .buildTransformer

      Foo(InnerFoo("aaa"))
        .intoF[ErrorOr, Bar]
        .withFieldConstF(_.b, intParserOpt.transform("bbb"))
        .transform
        .left
        .map(readableError) ==> Left(
        List(
          "inner.str" -> "Can't parse int from aaa",
          "inner" -> "Can't parse int from notint",
          "inner" -> "Can't parse int from aaa",
          "" -> "Can't parse int from bbb"
        )
      )
    }

    test("Java Bean accessors error should contain path to the failed getter") {
      class Foo(a: String, b: String) {
        def getA: String = a
        def getB: String = b
      }
      case class Bar(a: Int, b: Int)

      new Foo("a", "b").intoF[ErrorOr, Bar].enableBeanGetters.transform.left.map(readableError) ==> Left(
        List(
          "getA" -> "Can't parse int from a",
          "getB" -> "Can't parse int from b"
        )
      )
    }

    test("tuple field's error should contain path to the failed field") {
      ("a", "b").transformIntoF[ErrorOr, (Int, Int)].left.map(readableError) ==> Left(
        List(
          "_1" -> "Can't parse int from a",
          "_2" -> "Can't parse int from b"
        )
      )
    }

    test("sealed hierarchy's error should add path to failed subtype") {
      (Foo.Baz("fail"): Foo).transformIntoF[ErrorOr, Bar].left.map(readableError) ==> Left(
        List(
          "field" -> "Can't parse int from fail"
        )
      )
    }

    test("flat List's errors should contain indices to failed values") {
      List("a", "b", "c").transformIntoF[ErrorOr, List[Int]].left.map(readableError) ==> Left(
        List(
          "(0)" -> "Can't parse int from a",
          "(1)" -> "Can't parse int from b",
          "(2)" -> "Can't parse int from c"
        )
      )
    }

    test("nested List's errors should contain indices to failed values") {
      case class Foo(list: List[String])
      case class Bar(list: List[Int])

      Foo(List("a", "b", "c")).transformIntoF[ErrorOr, Bar].left.map(readableError) ==> Left(
        List(
          "list(0)" -> "Can't parse int from a",
          "list(1)" -> "Can't parse int from b",
          "list(2)" -> "Can't parse int from c"
        )
      )
    }

    test("flat Array's errors should contain indices to failed values") {
      Array("a", "b", "c").transformIntoF[ErrorOr, Array[Int]].left.map(readableError) ==> Left(
        List(
          "(0)" -> "Can't parse int from a",
          "(1)" -> "Can't parse int from b",
          "(2)" -> "Can't parse int from c"
        )
      )
    }

    test("nested Array's errors should contain indices to failed values") {
      case class Foo(list: Array[String])
      case class Bar(list: Array[Int])

      Foo(Array("a", "b", "c")).transformIntoF[ErrorOr, Bar].left.map(readableError) ==> Left(
        List(
          "list(0)" -> "Can't parse int from a",
          "list(1)" -> "Can't parse int from b",
          "list(2)" -> "Can't parse int from c"
        )
      )
    }

    test("flat Map's error should contain key/value that failed conversion") {
      Map("1" -> "x", "y" -> "20").transformIntoF[ErrorOr, Map[Int, Int]].left.map(readableError) ==> Left(
        List(
          "(1)" -> "Can't parse int from x",
          "keys(y)" -> "Can't parse int from y"
        )
      )
    }

    test("case class-nested Map's error should contain path to key/value that failed conversion") {
      case class EnvelopeStr(map: Map[String, String])
      case class EnvelopeInt(map: Map[Int, Int])

      EnvelopeStr(Map("1" -> "x", "y" -> "20")).transformIntoF[ErrorOr, EnvelopeInt].left.map(readableError) ==> Left(
        List(
          "map(1)" -> "Can't parse int from x",
          "map.keys(y)" -> "Can't parse int from y"
        )
      )
    }
  }
}
