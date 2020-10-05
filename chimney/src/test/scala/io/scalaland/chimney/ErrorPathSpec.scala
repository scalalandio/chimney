package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.internal.utils.EitherUtils._
import io.scalaland.chimney.utils.OptionUtils._

import utest._

object ErrorPathSpec extends TestSuite {
  val tests = Tests {
    "error path should capture for" - {
      type V[+A] = Either[List[TransformationError[String]], A]

      def printError(err: TransformationError[String]): String =
        s"${err.message} on ${err.showErrorPath}"

      implicit val intParse: TransformerF[V, String, Int] =
        str => str.parseInt.fold[V[Int]](Left(List(TransformationError(s"Can't parse int from $str"))))(Right(_))

      "root" - {
        val errors = "invalid".transformIntoF[V, Int]

        errors.mapLeft(_.map(_.message)) ==> Left(List("Can't parse int from invalid"))

        errors.mapLeft(_.map(_.showErrorPath)) ==> Left(List(""))
      }

      "case classes" - {
        case class Foo(a: String, b: String, c: InnerFoo, d: String)

        case class InnerFoo(d: String, e: String)

        case class Bar(a: Int, b: Int, c: InnerBar, d: String)

        case class InnerBar(d: Int, e: Int)

        Foo("mmm", "nnn", InnerFoo("lll", "jjj"), "d").transformIntoF[V, Bar].mapLeft(_.map(printError)) ==>
          Left(
            List(
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

        Foo(List("a", "b", "c")).transformIntoF[V, Bar].mapLeft(_.map(printError)) ==>
          Left(
            List(
              "Can't parse int from a on list(0)",
              "Can't parse int from b on list(1)",
              "Can't parse int from c on list(2)"
            )
          )
      }

      "map" - {
        case class Foo(map: Map[String, String], map2: Map[String, String])

        case class Bar(map: Map[Int, Int], map2: Map[String, Int])

        case class Bar2(list: List[(Int, Int)], list2: List[(String, Int)])

        val foo = Foo(Map("a" -> "b", "c" -> "d"), Map("e" -> "f"))

        val errors = Left(
          List(
            "Can't parse int from a on map.keys(a)",
            "Can't parse int from b on map(a)",
            "Can't parse int from c on map.keys(c)",
            "Can't parse int from d on map(c)",
            "Can't parse int from f on map2(e)"
          )
        )

        foo.transformIntoF[V, Bar].mapLeft(_.map(printError)) ==> errors
        foo
          .intoF[V, Bar2]
          .withFieldRenamed(_.map, _.list)
          .withFieldRenamed(_.map2, _.list2)
          .transform
          .mapLeft(_.map(printError)) ==> errors

        val error = compileError("""Map("a" -> "b").transformIntoF[V, Map[Double, Double]]""")

        error.check(
          "",
          "derivation from k: java.lang.String to scala.Double is not supported in Chimney!"
        )

        error.check(
          "",
          "derivation from v: java.lang.String to scala.Double is not supported in Chimney!"
        )
      }

      "java beans" - {
        class Foo(a: String, b: String) {
          def getA: String = a
          def getB: String = b
        }

        case class Bar(a: Int, b: Int)

        new Foo("a", "b")
          .intoF[V, Bar]
          .enableBeanGetters
          .transform
          .mapLeft(_.map(printError)) ==>
          Left(
            List(
              "Can't parse int from a on getA",
              "Can't parse int from b on getB"
            )
          )
      }

      "tuples" - {
        ("a", "b").transformIntoF[V, (Int, Int)].mapLeft(_.map(printError)) ==>
          Left(
            List(
              "Can't parse int from a on _1",
              "Can't parse int from b on _2"
            )
          )
      }
    }
  }
}
