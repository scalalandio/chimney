package io.scalaland.chimney.cats

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import io.scalaland.chimney.{TransformationError, Transformer, TransformerF}
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.utils.OptionUtils.*
import utest.*

object LiftedTransformerErrorPathValidatedNelInstanceSpec extends TestSuite {

  val tests = Tests {
    test("path of error should capture for") {
      type V[+A] = ValidatedNel[TransformationError[String], A]

      def printError(err: TransformationError[String]): String =
        s"${err.message} on ${err.showErrorPath}"

      implicit val intParse: TransformerF[V, String, Int] =
        str =>
          Validated.fromOption(
            str.parseInt,
            NonEmptyList.one(TransformationError[String](s"Can't parse int from $str"))
          )

      case class StringWrapper(str: String)

      implicit val stringUnwrap: Transformer[StringWrapper, String] =
        _.str

      test("case classes") {
        case class Foo(a: String, b: String, c: InnerFoo, d: StringWrapper)

        case class InnerFoo(d: String, e: String)

        case class Bar(a: Int, b: Int, c: InnerBar, d: String)

        case class InnerBar(d: Int, e: Int)

        Foo("mmm", "nnn", InnerFoo("lll", "jjj"), StringWrapper("d"))
          .transformIntoF[V, Bar]
          .leftMap(_.map(printError)) ==>
          Validated.Invalid(
            NonEmptyList.of(
              "Can't parse int from mmm on a",
              "Can't parse int from nnn on b",
              "Can't parse int from lll on c.d",
              "Can't parse int from jjj on c.e"
            )
          )
      }

      test("list") {
        case class Foo(list: List[String])

        case class Bar(list: List[Int])

        Foo(List("a", "b", "c")).transformIntoF[V, Bar].leftMap(_.map(printError)) ==>
          Validated.Invalid(
            NonEmptyList.of(
              "Can't parse int from a on list(0)",
              "Can't parse int from b on list(1)",
              "Can't parse int from c on list(2)"
            )
          )
      }

      test("map") {
        case class FooKey(value: String)

        case class FooValue(value: String)

        case class Foo(map: Map[String, String], map2: Map[FooKey, FooValue], map3: Map[Foo2Key, Foo2Value])

        case class Bar(map: Map[Int, Int], map2: Map[BarKey, BarValue], map3: Map[Bar2Key, Bar2Value])

        case class BarKey(value: Int)

        case class BarValue(value: Int)

        case class Foo2Key(value: String)

        case class Foo2Value(value: String)

        case class Bar2Key(str: String)

        case class Bar2Value(str: String)

        implicit val foo2KeyToBar2Key: Transformer[Foo2Key, Bar2Key] =
          foo => Bar2Key(foo.value)

        implicit val foo2ValueToBar2Value: Transformer[Foo2Value, Bar2Value] =
          foo => Bar2Value(foo.value)

        Foo(Map("a" -> "b", "c" -> "d"), Map(FooKey("e") -> FooValue("f")), Map(Foo2Key("g") -> Foo2Value("j")))
          .transformIntoF[V, Bar]
          .leftMap(_.map(printError)) ==>
          Validated.Invalid(
            NonEmptyList.of(
              "Can't parse int from a on map.keys(a)",
              "Can't parse int from b on map(a)",
              "Can't parse int from c on map.keys(c)",
              "Can't parse int from d on map(c)",
              "Can't parse int from e on map2.keys(FooKey(e)).value",
              "Can't parse int from f on map2(FooKey(e)).value"
            )
          )

        val error = compileError("""Map(FooKey("a") -> FooValue("b")).transformIntoF[V, Map[Double, Double]]""")

        error.check(
          "",
          "derivation from k: io.scalaland.chimney.cats.LiftedTransformerErrorPathValidatedNelInstanceSpec.FooKey to scala.Double is not supported in Chimney!"
        )

        error.check(
          "",
          "derivation from v: io.scalaland.chimney.cats.LiftedTransformerErrorPathValidatedNelInstanceSpec.FooValue to scala.Double is not supported in Chimney!"
        )

        Map(StringWrapper("a") -> StringWrapper("b"), StringWrapper("c") -> StringWrapper("d"))
          .transformIntoF[V, Map[String, String]] ==> Validated
          .Valid(
            Map("a" -> "b", "c" -> "d")
          )
      }
    }
  }
}
