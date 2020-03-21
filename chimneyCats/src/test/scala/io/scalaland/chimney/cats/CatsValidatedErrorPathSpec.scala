package io.scalaland.chimney.cats

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import io.scalaland.chimney.{TransformationError, TransformerF}
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.utils.OptionUtils._

import utest._

object CatsValidatedErrorPathSpec extends TestSuite {
  val tests = Tests {
    "path of error should capture for" - {
      type V[+A] = ValidatedNec[TransformationError[String], A]

      def printError(err: TransformationError[String]): String =
        s"${err.message} on ${err.showErrorPath}"

      implicit val intParse: TransformerF[V, String, Int] =
        str =>
          Validated.fromOption(
            str.parseInt,
            NonEmptyChain.one(TransformationError[String](s"Can't parse int from $str"))
          )

      "case classes" - {
        case class Foo(a: String, b: String, c: InnerFoo)

        case class InnerFoo(d: String, e: String)

        case class Bar(a: Int, b: Int, c: InnerBar)

        case class InnerBar(d: Int, e: Int)

        Foo("mmm", "nnn", InnerFoo("lll", "jjj")).transformIntoF[V, Bar].leftMap(_.map(printError)) ==>
          Validated.Invalid(
            NonEmptyChain(
              "Can't parse int from mmm on a",
              "Can't parse int from nnn on b",
              "Can't parse int from lll on c.d",
              "Can't parse int from jjj on c.e"
            )
          )
      }

      "list" - {
        case class Foo(list: List[String])

        case class Bar(list: List[Int])

        Foo(List("a", "b", "c")).transformIntoF[V, Bar].leftMap(_.map(printError)) ==>
          Validated.Invalid(
            NonEmptyChain(
              "Can't parse int from a on list(0)",
              "Can't parse int from b on list(1)",
              "Can't parse int from c on list(2)"
            )
          )
      }

      "map" - {
        case class FooKey(value: String)

        case class FooValue(value: String)

        case class Foo(map: Map[String, String], map2: Map[FooKey, FooValue])

        case class Bar(map: Map[Int, Int], map2: Map[BarKey, BarValue])

        case class BarKey(value: Int)

        case class BarValue(value: Int)

        Foo(Map("a" -> "b", "c" -> "d"), Map(FooKey("e") -> FooValue("f")))
          .transformIntoF[V, Bar]
          .leftMap(_.map(printError)) ==>
          Validated.Invalid(
            NonEmptyChain(
              "Can't parse int from a on map.keys(a)",
              "Can't parse int from b on map(a)",
              "Can't parse int from c on map.keys(c)",
              "Can't parse int from d on map(c)",
              "Can't parse int from e on map2.keys(FooKey(e)).value",
              "Can't parse int from f on map2(FooKey(e)).value"
            )
          )
      }
    }
  }
}
