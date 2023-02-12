package io.scalaland.chimney.cats

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import io.scalaland.chimney.{Transformer, TransformerF}
import io.scalaland.chimney.cats.utils.ValidatedUtils._
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.examples.trip._
import io.scalaland.chimney.utils.OptionUtils._
import utest._

import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer

object LiftedTransformerValidatedNelInstanceSpec extends TestSuite {

  val tests = Tests {

    test("default transformation always becomes a Validated.Valid") {

      Person("John", 10, 140).transformIntoF[ValidatedNel[String, +*], User] ==> Validated.valid(User("John", 10, 140))
    }

    test("transformation becomes an Validated.Invalid if any component was converted to an Validated.Invalid") {

      Person("John", 10, 140)
        .intoF[ValidatedNel[String, +*], User]
        .withFieldConstF(_.height, Validated.invalid(NonEmptyList.of("abc", "def")))
        .transform ==> Validated.invalid(NonEmptyList.of("abc", "def"))
    }

    test("transformation with field validated with .withFieldComputedF") {

      test("combines validation results") {
        val okForm = PersonForm("John", "10", "140")

        test("with 1 argument validation to Validated.Valid should return Validated.Valid") {

          okForm
            .into[Person]
            .withFieldConst(_.height, 200.5)
            .withFieldComputedF[ValidatedNel[String, +*], Int, Int](_.age, _.age.parseInt.toValidatedNel("bad age"))
            .transform ==> Validated.valid(Person("John", 10, 200.5))
        }

        test("with 2 arguments validation to Validated.Invalid should accumulates errors in Validated.Invalid") {
          okForm
            .intoF[ValidatedNel[String, +*], Person]
            .withFieldComputedF(_.height, _.height.parseDouble.toValidatedNel("bad height"))
            .withFieldComputedF(_.age, _.age.parseInt.toValidatedNel("bad age"))
            .transform ==> Validated.valid(Person("John", 10, 140))
        }

        test("with 3 argument validation to Validated.Invalid and Ior.Both should accumulate errors to the first Validated.Invalid") {

          okForm
            .intoF[ValidatedNel[String, +*], Person]
            .withFieldComputedF(
              _.name,
              pf =>
                if (pf.name.isEmpty) Validated.invalidNel("empty name")
                else Validated.valid(pf.name.toUpperCase())
            )
            .withFieldComputedF(_.age, _.age.parseInt.toValidatedNel("bad age"))
            .withFieldComputedF(_.height, _.height.parseDouble.toValidatedNel("bad height"))
            .transform ==> Validated.valid(Person("JOHN", 10, 140))
        }
      }

      test("failure with error handling") {
        val badForm = PersonForm("", "foo", "bar")

        badForm
          .into[Person]
          .withFieldComputedF[ValidatedNel[String, +*], String, String](
            _.name,
            pf =>
              if (pf.name.isEmpty) Validated.invalidNel("empty name")
              else Validated.valid(pf.name.toUpperCase())
          )
          .withFieldComputedF(_.age, _.age.parseInt.toValidatedNel("bad age"))
          .withFieldComputedF(_.height, _.age.parseDouble.toValidatedNel("bad double"))
          .transform ==> Validated.invalid(NonEmptyList.of("empty name", "bad age", "bad double"))

      }
    }

    test("recursive transform with nested validation") {

      implicit val personTransformerEithers: TransformerF[ValidatedNel[String, +*], PersonForm, Person] =
        Transformer
          .defineF[ValidatedNel[String, +*], PersonForm, Person]
          .withFieldComputedF(_.age, _.age.parseInt.toValidatedNel("bad age"))
          .withFieldComputedF(_.height, _.height.parseDouble.toValidatedNel("bad height"))
          .buildTransformer

      test("success") {

        val okTripForm = TripForm("100", List(PersonForm("John", "10", "140"), PersonForm("Caroline", "12", "155")))

        okTripForm
          .intoF[ValidatedNel[String, +*], Trip]
          .withFieldComputedF(_.id, tf => tf.tripId.parseInt.toValidatedNel("bad id"))
          .transform ==> Validated.valid(Trip(100, Vector(Person("John", 10, 140), Person("Caroline", 12, 155))))
      }

      test("failure with error handling") {

        val badTripForm = TripForm("100", List(PersonForm("John", "10", "foo"), PersonForm("Caroline", "bar", "155")))

        badTripForm
          .intoF[ValidatedNel[String, +*], Trip]
          .withFieldComputedF(_.id, tf => tf.tripId.parseInt.toValidatedNel("bad id"))
          .transform ==> Validated.invalid(NonEmptyList.of("bad height", "bad age"))
      }
    }

    test("wrapped subtype transformation") {

      class Foo(val x: Int)
      case class Bar(override val x: Int) extends Foo(x)

      val optFoo: ValidatedNel[String, Foo] = Bar(100).intoF[ValidatedNel[String, +*], Foo].transform
      optFoo.getValid.x ==> 100
    }

    test("wrapped value classes") {

      test("from value class") {
        addressbook.Email("abc@def.com").intoF[ValidatedNel[String, +*], String].transform ==>
          Validated.valid("abc@def.com")
      }

      test("to value class") {
        "abc@def.com".intoF[ValidatedNel[String, +*], addressbook.Email].transform ==>
          Validated.valid(addressbook.Email("abc@def.com"))
      }
    }

    test("wrapped options") {

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        Option(123).intoF[ValidatedNel[String, +*], Option[String]].transform ==> Validated.valid(Some("123"))
        Option.empty[Int].intoF[ValidatedNel[String, +*], Option[String]].transform ==> Validated.valid(None)
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[ValidatedNel[String, +*], String, Int] =
          _.parseInt.toValidatedNel("bad int")

        Option("123").intoF[ValidatedNel[String, +*], Option[Int]].transform ==> Validated.valid(Some(123))
        Option("abc").intoF[ValidatedNel[String, +*], Option[Int]].transform ==> Validated.invalidNel("bad int")
        Option.empty[String].intoF[ValidatedNel[String, +*], Option[Int]].transform ==> Validated.valid(None)
      }
    }

    test("wrapped T to Option[T]") {

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        10.intoF[ValidatedNel[String, +*], Option[String]].transform ==> Validated.valid(Some("10"))
        (null: String).intoF[ValidatedNel[String, +*], Option[String]].transform ==> Validated.valid(None)
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[ValidatedNel[String, +*], String, Int] =
          _.parseInt.toValidatedNel("bad int")

        "10".intoF[ValidatedNel[String, +*], Option[Int]].transform ==> Validated.valid(Some(10))
        (null: String).intoF[ValidatedNel[String, +*], Option[Int]].transform ==> Validated.valid(None)
        "x".intoF[ValidatedNel[String, +*], Option[Int]].transform ==> Validated.invalidNel("bad int")
      }
    }

    test("wrapped .enableUnsafeOption") {

      test("pure inner transformer") {
        implicit val intPrinter: Transformer[Int, String] = _.toString

        Option(10).intoF[ValidatedNel[String, +*], String].enableUnsafeOption.transform ==> Validated.valid("10")
        intercept[NoSuchElementException] {
          Option.empty[Int].intoF[ValidatedNel[String, +*], String].enableUnsafeOption.transform
        }
      }

      test("wrapped inner transformer") {
        implicit val intParserValidated: TransformerF[ValidatedNel[String, +*], String, Int] =
          _.parseInt.toValidatedNel("bad int")

        Option("10").intoF[ValidatedNel[String, +*], Int].enableUnsafeOption.transform ==> Validated.valid(10)
        Option("x").intoF[ValidatedNel[String, +*], Int].enableUnsafeOption.transform ==>
          Validated.invalidNel("bad int")
        intercept[NoSuchElementException] {
          Option.empty[String].intoF[ValidatedNel[String, +*], Int].enableUnsafeOption.transform
        }
      }
    }

    test("wrapped iterables or arrays") {

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        List(123, 456).intoF[ValidatedNel[String, +*], List[String]].transform ==> Validated.valid(List("123", "456"))
        Vector(123, 456).intoF[ValidatedNel[String, +*], Queue[String]].transform ==> Validated.valid(
          Queue("123", "456")
        )
        Array.empty[Int].intoF[ValidatedNel[String, +*], Seq[String]].transform ==> Validated.valid(Seq.empty[String])
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[ValidatedNel[String, +*], String, Int] =
          _.parseInt.toValidatedNel("bad int")

        List("123", "456").intoF[ValidatedNel[String, +*], List[Int]].transform ==> Validated.valid(List(123, 456))
        Vector("123", "456").intoF[ValidatedNel[String, +*], Queue[Int]].transform ==> Validated.valid(Queue(123, 456))
        Array.empty[String].intoF[ValidatedNel[String, +*], Seq[Int]].transform ==> Validated.valid(Seq.empty[Int])
        Set("123", "456").intoF[ValidatedNel[String, +*], Array[Int]].transform.getValid.sorted ==> Array(123, 456)

        List("abc", "456").intoF[ValidatedNel[String, +*], List[Int]].transform ==> Validated.invalidNel("bad int")
        Vector("123", "def").intoF[ValidatedNel[String, +*], Queue[Int]].transform ==> Validated.invalidNel("bad int")
        Array("abc", "def").intoF[ValidatedNel[String, +*], Seq[Int]].transform ==>
          Validated.invalid(NonEmptyList.of("bad int", "bad int"))
        Set("123", "xyz").intoF[ValidatedNel[String, +*], Array[Int]].transform ==> Validated.invalidNel("bad int")
      }
    }

    test("wrapped maps") {

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        Map(1 -> 10, 2 -> 20).intoF[ValidatedNel[String, +*], Map[String, String]].transform ==>
          Validated.valid(Map("1" -> "10", "2" -> "20"))
        Map(1 -> 10, 2 -> 20).intoF[ValidatedNel[String, +*], Map[String, Int]].transform ==>
          Validated.valid(Map("1" -> 10, "2" -> 20))
        Seq(1 -> 10, 2 -> 20).intoF[ValidatedNel[String, +*], Map[String, String]].transform ==>
          Validated.valid(Map("1" -> "10", "2" -> "20"))
        ArrayBuffer(1 -> 10, 2 -> 20).intoF[ValidatedNel[String, +*], Map[Int, String]].transform ==>
          Validated.valid(Map(1 -> "10", 2 -> "20"))
        Map(1 -> 10, 2 -> 20).intoF[ValidatedNel[String, +*], List[(String, String)]].transform ==>
          Validated.valid(List("1" -> "10", "2" -> "20"))
        Map(1 -> 10, 2 -> 20).intoF[ValidatedNel[String, +*], Vector[(String, Int)]].transform ==>
          Validated.valid(Vector("1" -> 10, "2" -> 20))
        Array(1 -> 10, 2 -> 20).intoF[ValidatedNel[String, +*], Map[String, String]].transform ==>
          Validated.valid(Map("1" -> "10", "2" -> "20"))
        Array(1 -> 10, 2 -> 20).intoF[ValidatedNel[String, +*], Map[Int, String]].transform ==>
          Validated.valid(Map(1 -> "10", 2 -> "20"))
        Map(1 -> 10, 2 -> 20).intoF[ValidatedNel[String, +*], Array[(String, String)]].transform.getValid ==>
          Array("1" -> "10", "2" -> "20")
        Map(1 -> 10, 2 -> 20).intoF[ValidatedNel[String, +*], Array[(String, Int)]].transform.getValid ==>
          Array("1" -> 10, "2" -> 20)
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[ValidatedNel[String, +*], String, Int] =
          _.parseInt.toValidatedNel("bad int")

        Map("1" -> "10", "2" -> "20").intoF[ValidatedNel[String, +*], Map[Int, Int]].transform ==>
          Validated.valid(Map(1 -> 10, 2 -> 20))
        Map("1" -> "10", "2" -> "20").intoF[ValidatedNel[String, +*], Map[Int, String]].transform ==>
          Validated.valid(Map(1 -> "10", 2 -> "20"))
        Seq("1" -> "10", "2" -> "20").intoF[ValidatedNel[String, +*], Map[Int, Int]].transform ==>
          Validated.valid(Map(1 -> 10, 2 -> 20))
        ArrayBuffer("1" -> "10", "2" -> "20").intoF[ValidatedNel[String, +*], Map[String, Int]].transform ==>
          Validated.valid(Map("1" -> 10, "2" -> 20))
        Map("1" -> "10", "2" -> "20").intoF[ValidatedNel[String, +*], List[(Int, Int)]].transform ==>
          Validated.valid(List(1 -> 10, 2 -> 20))
        Map("1" -> "10", "2" -> "20").intoF[ValidatedNel[String, +*], Vector[(Int, String)]].transform ==>
          Validated.valid(Vector(1 -> "10", 2 -> "20"))
        Array("1" -> "10", "2" -> "20").intoF[ValidatedNel[String, +*], Map[Int, Int]].transform ==>
          Validated.valid(Map(1 -> 10, 2 -> 20))
        Array("1" -> "10", "2" -> "20").intoF[ValidatedNel[String, +*], Map[String, Int]].transform ==>
          Validated.valid(Map("1" -> 10, "2" -> 20))
        Map("1" -> "10", "2" -> "20").intoF[ValidatedNel[String, +*], Array[(Int, Int)]].transform.getValid ==>
          Array(1 -> 10, 2 -> 20)
        Map("1" -> "10", "2" -> "20").intoF[ValidatedNel[String, +*], Array[(Int, String)]].transform.getValid ==>
          Array(1 -> "10", 2 -> "20")

        Map("1" -> "x", "y" -> "20").intoF[ValidatedNel[String, +*], Map[Int, Int]].transform ==>
          Validated.invalid(NonEmptyList.of("bad int", "bad int"))
        Map("x" -> "10", "2" -> "20").intoF[ValidatedNel[String, +*], Map[Int, String]].transform ==>
          Validated.invalidNel("bad int")
        Seq("1" -> "10", "2" -> "x").intoF[ValidatedNel[String, +*], Map[Int, Int]].transform ==>
          Validated.invalidNel("bad int")
        ArrayBuffer("1" -> "x", "2" -> "y").intoF[ValidatedNel[String, +*], Map[String, Int]].transform ==>
          Validated.invalid(NonEmptyList.of("bad int", "bad int"))
        Map("x" -> "10", "y" -> "z").intoF[ValidatedNel[String, +*], ArrayBuffer[(Int, Int)]].transform ==>
          Validated.invalid(NonEmptyList.of("bad int", "bad int", "bad int"))
        Map("1" -> "10", "x" -> "20").intoF[ValidatedNel[String, +*], Vector[(Int, String)]].transform ==>
          Validated.invalidNel("bad int")
        Array("x" -> "y", "z" -> "v").intoF[ValidatedNel[String, +*], Map[Int, Int]].transform ==>
          Validated.invalid(NonEmptyList.of("bad int", "bad int", "bad int", "bad int"))
        Array("1" -> "x", "2" -> "y").intoF[ValidatedNel[String, +*], Map[String, Int]].transform ==>
          Validated.invalid(NonEmptyList.of("bad int", "bad int"))
        Map("1" -> "10", "x" -> "20").intoF[ValidatedNel[String, +*], Array[(Int, Int)]].transform ==>
          Validated.invalidNel("bad int")
        Map("x" -> "10", "y" -> "20").intoF[ValidatedNel[String, +*], Array[(Int, String)]].transform ==>
          Validated.invalid(NonEmptyList.of("bad int", "bad int"))
      }
    }

    test("wrapped eithers") {

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        (Left(1): Either[Int, Int]).intoF[ValidatedNel[String, +*], Either[String, String]].transform ==>
          Validated.valid(Left("1"))
        (Right(1): Either[Int, Int]).intoF[ValidatedNel[String, +*], Either[String, String]].transform ==>
          Validated.valid(Right("1"))
        Left(1).intoF[ValidatedNel[String, +*], Either[String, String]].transform ==> Validated.valid(Left("1"))
        Right(1).intoF[ValidatedNel[String, +*], Either[String, String]].transform ==> Validated.valid(Right("1"))
        Left(1).intoF[ValidatedNel[String, +*], Left[String, String]].transform ==> Validated.valid(Left("1"))
        Right(1).intoF[ValidatedNel[String, +*], Right[String, String]].transform ==> Validated.valid(Right("1"))
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[ValidatedNel[String, +*], String, Int] =
          _.parseInt.toValidatedNel("bad int")

        (Left("1"): Either[String, String]).intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==>
          Validated.valid(Left(1))
        (Right("1"): Either[String, String]).intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==>
          Validated.valid(Right(1))
        Left("1").intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==> Validated.valid(Left(1))
        Right("1").intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==> Validated.valid(Right(1))
        Left("1").intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==> Validated.valid(Left(1))
        Right("1").intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==> Validated.valid(Right(1))

        (Left("x"): Either[String, String]).intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==>
          Validated.invalidNel("bad int")
        (Right("x"): Either[String, String]).intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==>
          Validated.invalidNel("bad int")
        Left("x").intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==> Validated.invalidNel("bad int")
        Right("x").intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==> Validated.invalidNel("bad int")
        Left("x").intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==> Validated.invalidNel("bad int")
        Right("x").intoF[ValidatedNel[String, +*], Either[Int, Int]].transform ==> Validated.invalidNel("bad int")
      }

      test("mixed inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString
        implicit val intParserValidated: TransformerF[ValidatedNel[String, +*], String, Int] =
          _.parseInt.toValidatedNel("bad int")

        (Left("1"): Either[String, Int]).intoF[ValidatedNel[String, +*], Either[Int, String]].transform ==>
          Validated.valid(Left(1))
        (Left("x"): Either[String, Int]).intoF[ValidatedNel[String, +*], Either[Int, String]].transform ==>
          Validated.invalidNel("bad int")
        (Right(100): Either[String, Int]).intoF[ValidatedNel[String, +*], Either[Int, String]].transform ==>
          Validated.valid(Right("100"))

        (Left(100): Either[Int, String]).intoF[ValidatedNel[String, +*], Either[String, Int]].transform ==>
          Validated.valid(Left("100"))
        (Right("1"): Either[Int, String]).intoF[ValidatedNel[String, +*], Either[String, Int]].transform ==>
          Validated.valid(Right(1))
        (Right("x"): Either[Int, String]).intoF[ValidatedNel[String, +*], Either[String, Int]].transform ==>
          Validated.invalidNel("bad int")
      }
    }

    test("wrapped sealed families") {
      import numbers._

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        import ScalesTransformerF.shortToLongPureInner

        (short.Zero: short.NumScale[Int, Nothing])
          .intoF[ValidatedNel[String, +*], long.NumScale[String]]
          .transform ==> Validated.valid(long.Zero)
        (short.Million(4): short.NumScale[Int, Nothing])
          .intoF[ValidatedNel[String, +*], long.NumScale[String]]
          .transform ==> Validated.valid(long.Million("4"))
        (short.Billion(2): short.NumScale[Int, Nothing])
          .intoF[ValidatedNel[String, +*], long.NumScale[String]]
          .transform ==> Validated.valid(long.Milliard("2"))
        (short.Trillion(100): short.NumScale[Int, Nothing])
          .intoF[ValidatedNel[String, +*], long.NumScale[String]]
          .transform ==> Validated.valid(long.Billion("100"))
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[ValidatedNel[String, +*], String, Int] =
          _.parseInt.toValidatedNel("bad int")

        import ScalesTransformerF.shortToLongWrappedInner

        (short.Zero: short.NumScale[String, Nothing])
          .intoF[ValidatedNel[String, +*], long.NumScale[Int]]
          .transform ==> Validated.valid(long.Zero)
        (short.Million("4"): short.NumScale[String, Nothing])
          .intoF[ValidatedNel[String, +*], long.NumScale[Int]]
          .transform ==> Validated.valid(long.Million(4))
        (short.Billion("2"): short.NumScale[String, Nothing])
          .intoF[ValidatedNel[String, +*], long.NumScale[Int]]
          .transform ==> Validated.valid(long.Milliard(2))
        (short.Trillion("100"): short.NumScale[String, Nothing])
          .intoF[ValidatedNel[String, +*], long.NumScale[Int]]
          .transform ==> Validated.valid(long.Billion(100))

        (short.Million("x"): short.NumScale[String, Nothing])
          .intoF[ValidatedNel[String, +*], long.NumScale[Int]]
          .transform ==> Validated.invalidNel("bad int")
        (short.Billion("x"): short.NumScale[String, Nothing])
          .intoF[ValidatedNel[String, +*], long.NumScale[Int]]
          .transform ==> Validated.invalidNel("bad int")
        (short.Trillion("x"): short.NumScale[String, Nothing])
          .intoF[ValidatedNel[String, +*], long.NumScale[Int]]
          .transform ==> Validated.invalidNel("bad int")
      }
    }
  }
}
