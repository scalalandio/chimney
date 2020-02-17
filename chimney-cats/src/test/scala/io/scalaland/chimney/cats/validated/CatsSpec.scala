package io.scalaland.chimney.cats.validated

import utest._
import cats.data.{NonEmptyChain, Validated}
import io.scalaland.chimney.TransformerF
import io.scalaland.chimney.dsl._
import cats.syntax.validated._

object CatsSpec extends TestSuite {
  val tests = Tests {
    "invalid case" - {
      implicit val intParse: TransformerF[V, String, Int] =
        str => Validated.catchNonFatal(str.toInt).leftMap(_ => NonEmptyChain(TransformationError(s"$str is not int")))

      case class From(inner: InnerFrom, b: Option[Int], c: String, d: List[String])

      case class InnerFrom(a: String, mapka: Map[String, String], opt: Option[String])

      case class To(inner: InnerTo, b: Int, c: Int, d: List[Int])

      case class InnerTo(a: Int, mapka: Map[Int, Int], opt: String)

      From(InnerFrom("a", Map("aaa" -> "bbb", "ccc" -> "ddd"), None), Some(2), "b", List("c", "d"))
        .transformIntoF[V, To]
        .formatErrors ==> Validated.Invalid(
        NonEmptyChain(
          "a is not int on inner.a",
          "aaa is not int on inner.mapka.keys",
          "bbb is not int on inner.mapka.aaa",
          "ccc is not int on inner.mapka.keys",
          "ddd is not int on inner.mapka.ccc",
          "Required field expected. Got None on inner.opt",
          "b is not int on c",
          "c is not int on d.[0]",
          "d is not int on d.[1]"
        )
      )
    }

    "valid case" - {
      implicit val longParse: TransformerF[V, String, Long] =
        str => Validated.catchNonFatal(str.toLong).leftMap(_ => NonEmptyChain(TransformationError(s"$str is not long")))

      case class From(innerFrom: InnerFrom, b: Option[String], c: String)

      case class To(innerTo: InnerTo, b: Long, c: Option[Long], d: Option[String])

      case class InnerFrom(a: String, b: List[String], mapka: Map[String, String])

      case class InnerTo(a: String, b: List[Long], mapka: Map[Long, Long])

      From(InnerFrom("1", List("2", "3"), Map("3" -> "4")), Some("4"), "5")
        .intoF[V, To]
        .withFieldConst(_.d, Some("d"))
        .withFieldRenamed(_.innerFrom, _.innerTo)
        .transform ==>
        Validated.Valid(To(InnerTo("1", List(2L, 3L), Map(3L -> 4L)), 4L, Some(5L), Some("d")))
    }

    "with constF and computedF" - {
      case class From(a: Int, b: String, c: Option[String], e: String)

      case class To(a: Int, b: String, c: String, d: String, ee: String)

      val from = From(1, "b", Some("c"), "e")

      from
        .intoF[V, To]
        .withFieldConstF(_.d, "d".validNec)
        .withFieldComputedF(_.ee, _.e.validNec)
        .transform ==> To(1, "b", "c", "d", "e").validNec

      from
        .intoF[V, To]
        .withFieldConstF(_.d, TransformationError("Invalid value").invalidNec)
        .withFieldComputedF(_.ee, _ => TransformationError("Invalid value 2").invalidNec)
        .transform
        .formatErrors ==> Validated.Invalid(NonEmptyChain("Invalid value on d", "Invalid value 2 on ee"))
    }
  }
}
