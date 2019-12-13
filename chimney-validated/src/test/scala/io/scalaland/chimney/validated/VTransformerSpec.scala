package io.scalaland.chimney.validated

import cats.data.{Validated, ValidatedNec, NonEmptyChain}
import cats.implicits._
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.validated.dsl._
import utest._

object VTransformerSpec extends TestSuite {
  def showErrors[A](res: ValidatedNec[VTransformer.Error, A]): ValidatedNec[String, A] =
    res.leftMap(_.map(_.info))

  val tests = Tests {
    "pure case" - {
      case class From(a: Int, b: String, c: String)

      case class To(a: Int, b: String, c: String)

      val from = From(1, "b", "c")

      from.tryTransformInto[To] ==> To(1, "b", "c").validNec
    }

    "single option validating and list" - {
      case class From(a: Int, b: Option[String], c: List[String])

      case class To(a: Int, b: Int, c: List[Int])

      val list = List[ValidatedNec[VTransformer.Error, String]]("kek".validNec, "lol".validNec)

      implicit val intParse: VTransformer[String, Int] =
        str => Validated.catchNonFatal(str.toInt).leftMap(_ => NonEmptyChain(VTransformer.Error(s"$str is not int")))

      val success = From(1, "2".some, List("3", "4"))

      val failure = From(1, None, Nil)

      val failureIntParse = From(1, "str".some, List("str2", "str3"))

      success.tryTransformInto[To] ==> To(1, 2, List(3, 4)).validNec

      showErrors(failure.tryTransformInto[To]) ==> "Should be required on b".invalidNec

      showErrors(failureIntParse.tryTransformInto[To]) ==> Validated.Invalid(
        NonEmptyChain("str is not int on b", "str2 is not int on c.[0]", "str3 is not int on c.[1]")
      )
    }

    "multiply and deep" - {
      case class From(a: Int, b: Option[InnerFrom], e: Option[String])

      case class InnerFrom(c: Option[Int], d: Option[Int])

      case class To(a: Int, b: InnerTo, e: String)

      case class InnerTo(c: Int, d: Int)

      val success = From(1, InnerFrom(10.some, 20.some).some, "e".some)

      val failure = From(1, InnerFrom(None, None).some, None)

      success.tryTransformInto[To] ==> To(1, InnerTo(10, 20), "e").validNec

      showErrors(failure.tryTransformInto[To]) ==> Validated.Invalid(
        NonEmptyChain("Should be required on b.c", "Should be required on b.d", "Should be required on e")
      )
    }

    "with const and rename" - {
      case class From(a: Int, b: Option[InnerFrom], c: Option[Int], e: Option[String])

      case class InnerFrom(innerA: Option[Int])

      case class To(a: Int, b: InnerTo, d: Int, e: Option[String])

      case class InnerTo(innerA: Int, b: String)

      implicit val innerT: VTransformer[InnerFrom, InnerTo] =
        _.intoV[InnerTo].withFieldConst(_.b, "b").tryTransform

      implicit val fromToT: VTransformer[From, To] =
        _.intoV[To].withFieldRenamed(_.c, _.d).tryTransform

      val success = From(1, InnerFrom(2.some).some, 3.some, "e".some)

      val failure = From(1, InnerFrom(None).some, 1.some, None)

      success.tryTransformInto[To] ==> To(1, InnerTo(2, "b"), 3, "e".some).validNec

      showErrors(failure.tryTransformInto[To]) ==> Validated.Invalid(NonEmptyChain("Should be required on b.innerA"))
    }

    "with vconst and vcomputed" - {
      case class From(a: Int, b: String, c: Option[String], e: String)

      case class To(a: Int, b: String, c: String, d: String, ee: String)

      val from = From(1, "b", "c".some, "e")

      from
        .intoV[To]
        .withFieldConstV(_.d, "d".validNec)
        .withFieldComputedV(_.ee, _.e.validNec)
        .tryTransform ==> To(1, "b", "c", "d", "e").validNec

      showErrors(
        from
          .intoV[To]
          .withFieldConstV(_.d, VTransformer.Error("Invalid value").invalidNec)
          .withFieldComputedV(_.ee, _ => VTransformer.Error("Invalid value 2").invalidNec)
          .tryTransform
      ) ==> Validated.Invalid(NonEmptyChain("Invalid value on d", "Invalid value 2 on ee"))
    }

    "support map" - {
      case class From(mapka: Map[String, String])

      case class To(mapka: Map[Int, Int])

      implicit val intParse: VTransformer[String, Int] =
        str => Validated.catchNonFatal(str.toInt).leftMap(_ => NonEmptyChain(VTransformer.Error(s"$str is not int")))

      val success = From(Map("1" -> "2", "3" -> "4"))

      val failure = From(Map("kek" -> "lol", "3" -> "4"))

      success.tryTransformInto[To] ==> To(Map(1 -> 2, 3 -> 4)).validNec

      showErrors(failure.tryTransformInto[To]) ==> Validated.Invalid(
        NonEmptyChain("kek is not int on mapka.keys", "lol is not int on mapka.kek")
      )
    }

    "not compile on unsupported transformation" - {
      compileError("""1.tryTransformInto[String]""")
        .check("", "derivation from int: scala.Int to java.lang.String is not supported in Chimney!")

      compileError("""Map("kek" -> "lol").tryTransformInto[Map[Int, Int]]""")
        .check("", "derivation from key: java.lang.String to scala.Int is not supported in Chimney!")
    }

    "tuple to case class" - {
      val tuple = (1.some, 2.0.some, "3".some)

      case class Foo(a: Int, b: Double, c: String)

      tuple.tryTransformInto[Foo] ==> Foo(1, 2.0, "3").validNec

      case class Bar(a: Int, b: Double, c: String, d: Int)

      compileError("""tuple.tryTransformInto[Bar]""").check(
        "",
        "source tuple scala.Tuple3 is of arity 3, while target type io.scalaland.chimney.validated.VTransformerSpec.Bar is of arity 4; they need to be equal!"
      )
    }

    "missing targets" - {
      case class From(a: Option[Int], b: Option[String], c: Option[Double])

      case class To(a: Option[Int], b: String, d: String)

      compileError("""From(1.some, "b".some, 2.0.some).tryTransformInto[To]""").check(
        "",
        "d: java.lang.String - no accessor named d in source type io.scalaland.chimney.validated.VTransformerSpec.From"
      )
    }

    "traversable" - {
      case class From(collection: List[String])

      case class To1(collection: Vector[Int])

      case class To2(collection: Vector[String])

      case class To3(collection: List[String])

      case class To4(collection: List[Int])

      implicit val intParse: VTransformer[String, Int] =
        str => Validated.catchNonFatal(str.toInt).leftMap(_ => NonEmptyChain(VTransformer.Error(s"$str is not int")))

      val from = From(List("1", "2", "3"))

      from.tryTransformInto[To1] ==> To1(Vector(1, 2, 3)).validNec

      from.tryTransformInto[To2] ==> To2(Vector("1", "2", "3")).validNec

      from.tryTransformInto[To3] ==> To3(List("1", "2", "3")).validNec

      from.tryTransformInto[To4] ==> To4(List(1, 2, 3)).validNec
    }

    "no rule for case class field" - {
      case class From(a: String, b: String, c: String)

      case class To(a: String, b: Int, c: String)

      compileError("""From("1", "2", "3").tryTransformInto[To]""")
        .check("", "derivation from from.b: java.lang.String to scala.Int is not supported in Chimney!")
    }
  }
}
