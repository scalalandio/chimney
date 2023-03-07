package io.scalaland.chimney.cats

import cats.data.{Ior, IorNec, NonEmptyChain}
import io.scalaland.chimney.{Transformer, TransformerF}
import io.scalaland.chimney.cats.utils.ValidatedUtils.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.examples.*
import io.scalaland.chimney.examples.trip.*
import io.scalaland.chimney.utils.OptionUtils.*
import utest.*

import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer

object LiftedTransformerIorNecInstanceSpec extends TestSuite {

  val tests: Tests = Tests {

    test("default transformation always becomes an Ior.Right") {

      Person("John", 10, 140).intoF[IorNec[String, +*], User].transform ==> Ior.right(User("John", 10, 140))
    }

    test("transformation becomes an Ior.Both if any component was converted to an Ior.Both") {

      Person("John", 10, 140)
        .intoF[IorNec[String, +*], User]
        .withFieldConstF(_.name, Ior.both(NonEmptyChain("Name should not have dots in it"), "John.Doe"))
        .transform ==> Ior.both(NonEmptyChain("Name should not have dots in it"), User("John.Doe", 10, 140))
    }

    test("transformation becomes an Ior.Left if any component was converted to an Ior.Left") {

      Person("", 10, 140)
        .intoF[IorNec[String, +*], User]
        .withFieldConstF(_.name, Ior.left(NonEmptyChain("You must provide a name")))
        .transform ==> Ior.left(NonEmptyChain("You must provide a name"))
    }

    test("transformation with field validated with .withFieldComputedF") {

      test("combines validation results") {

        val okForm = PersonForm("John", "10", "140")

        test("with 1 argument validation to Ior.Right should return Ior.Right") {

          okForm
            .into[Person]
            .withFieldConst(_.height, 200.5)
            .withFieldComputedF[IorNec[String, +*], Int, Int](
              _.age,
              _.age.parseInt.map(Ior.right).getOrElse(Ior.left(NonEmptyChain("Invalid age entered")))
            )
            .transform ==> Ior.right(Person("John", 10, 200.5))
        }

        test("with 2 arguments validation to Ior.Both should accumulates errors in Ior.Both") {

          okForm
            .intoF[IorNec[String, +*], Person]
            .withFieldComputedF(_.age, _ => Ior.both(NonEmptyChain("age warning"), 10))
            .withFieldComputedF(_.height, _ => Ior.both(NonEmptyChain("height warning"), 100.0))
            .transform ==> Ior.both(NonEmptyChain("age warning", "height warning"), Person("John", 10, 100.0))
        }

        test("with 3 argument validation to Ior.Left and Ior.Both should accumulate errors to the first Ior.Left") {

          okForm
            .intoF[IorNec[String, +*], Person]
            .withFieldComputedF(
              _.name,
              _ => Ior.both(NonEmptyChain("Putting a dot in the name is deprecated"), "John.Doe")
            )
            .withFieldConstF(_.age, Ior.left(NonEmptyChain("age is too low")))
            .withFieldConstF(_.height, Ior.both(NonEmptyChain("height not available, using default"), 10.0))
            .transform ==> Ior.left(NonEmptyChain("Putting a dot in the name is deprecated", "age is too low"))
        }

        test("failure with error handling") {
          val badForm = PersonForm("", "foo", "bar")

          badForm
            .into[Person]
            .withFieldComputedF[IorNec[String, +*], String, String](
              _.name,
              pf =>
                if (pf.name.isEmpty) Ior.leftNec("empty name")
                else Ior.right(pf.name.toUpperCase())
            )
            .withFieldComputedF(_.age, _.age.parseInt.toIorNec("bad age"))
            .withFieldComputedF(_.height, _.age.parseDouble.toIorNec("bad double"))
            .transform ==> Ior.left(NonEmptyChain("empty name"))
        }
      }
    }

    test("recursive transform with nested validation") {

      implicit val personTransformerEithers: TransformerF[IorNec[String, +*], PersonForm, Person] =
        Transformer
          .defineF[IorNec[String, +*], PersonForm, Person]
          .withFieldComputedF(_.age, _.age.parseInt.toIorNec("bad age"))
          .withFieldComputedF(_.height, _.height.parseDouble.toIorNec("bad height"))
          .buildTransformer

      test("success") {

        val okTripForm = TripForm("100", List(PersonForm("John", "10", "140"), PersonForm("Caroline", "12", "155")))

        okTripForm
          .intoF[IorNec[String, +*], Trip]
          .withFieldComputedF(_.id, tf => tf.tripId.parseInt.toIorNec("bad id"))
          .transform ==> Ior.right(Trip(100, Vector(Person("John", 10, 140), Person("Caroline", 12, 155))))
      }

      test("failure with error handling") {

        val badTripForm = TripForm("100", List(PersonForm("John", "10", "foo"), PersonForm("Caroline", "bar", "155")))

        badTripForm
          .intoF[IorNec[String, +*], Trip]
          .withFieldComputedF(_.id, tf => tf.tripId.parseInt.toIorNec("bad id"))
          .transform ==> Ior.left(NonEmptyChain("bad height"))
      }
    }

    test(".traverse should accumulate errors on the Left side") {

      TransformerFIorSupport[NonEmptyChain[String]]
        .traverse(
          Iterator("bla", "ha", "hee", "bee"),
          (input: String) => Ior.both(NonEmptyChain(s"Accumulating $input"), input)
        )
        .map(_.toList) ==>
        Ior.both(
          NonEmptyChain("Accumulating bla", "Accumulating ha", "Accumulating hee", "Accumulating bee"),
          List("bla", "ha", "hee", "bee")
        )
    }

    test("wrapped subtype transformation") {

      class Foo(val x: Int)
      case class Bar(override val x: Int) extends Foo(x)

      Bar(100).intoF[IorNec[String, +*], Foo].transform.right.map(_.x) ==> Some(100)
    }

    test("wrapped value classes") {

      test("from value class") {
        addressbook.Email("abc@def.com").intoF[IorNec[String, +*], String].transform ==>
          Ior.right("abc@def.com")
      }

      test("to value class") {
        "abc@def.com".intoF[IorNec[String, +*], addressbook.Email].transform ==>
          Ior.right(addressbook.Email("abc@def.com"))
      }
    }

    test("wrapped options") {

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        Option(123).intoF[IorNec[String, +*], Option[String]].transform ==> Ior.right(Some("123"))
        Option.empty[Int].intoF[IorNec[String, +*], Option[String]].transform ==> Ior.right(None)
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[IorNec[String, +*], String, Int] =
          _.parseInt.toIorNec("bad int")

        Option("123").intoF[IorNec[String, +*], Option[Int]].transform ==> Ior.right(Some(123))
        Option("abc").intoF[IorNec[String, +*], Option[Int]].transform ==> Ior.leftNec("bad int")
        Option.empty[String].intoF[IorNec[String, +*], Option[Int]].transform ==> Ior.right(None)
      }
    }

    test("wrapped T to Option[T]") {

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        10.intoF[IorNec[String, +*], Option[String]].transform ==> Ior.right(Some("10"))
        (null: String).intoF[IorNec[String, +*], Option[String]].transform ==> Ior.right(None)
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[IorNec[String, +*], String, Int] =
          _.parseInt.toIorNec("bad int")

        "10".intoF[IorNec[String, +*], Option[Int]].transform ==> Ior.right(Some(10))
        (null: String).intoF[IorNec[String, +*], Option[Int]].transform ==> Ior.right(None)
        "x".intoF[IorNec[String, +*], Option[Int]].transform ==> Ior.leftNec("bad int")
      }
    }

    test("wrapped .enableUnsafeOption") {

      test("pure inner transformer") {
        implicit val intPrinter: Transformer[Int, String] = _.toString

        Option(10).intoF[IorNec[String, +*], String].enableUnsafeOption.transform ==> Ior.right("10")
        intercept[NoSuchElementException] {
          Option.empty[Int].intoF[IorNec[String, +*], String].enableUnsafeOption.transform
        }
      }

      test("wrapped inner transformer") {
        implicit val intParserValidated: TransformerF[IorNec[String, +*], String, Int] =
          _.parseInt.toIorNec("bad int")

        Option("10").intoF[IorNec[String, +*], Int].enableUnsafeOption.transform ==> Ior.right(10)
        Option("x").intoF[IorNec[String, +*], Int].enableUnsafeOption.transform ==>
          Ior.leftNec("bad int")
        intercept[NoSuchElementException] {
          Option.empty[String].intoF[IorNec[String, +*], Int].enableUnsafeOption.transform
        }
      }
    }

    test("wrapped iterables or arrays") {

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        List(123, 456).intoF[IorNec[String, +*], List[String]].transform ==> Ior.right(List("123", "456"))
        Vector(123, 456).intoF[IorNec[String, +*], Queue[String]].transform ==> Ior.right(
          Queue("123", "456")
        )
        Array.empty[Int].intoF[IorNec[String, +*], Seq[String]].transform ==> Ior.right(Seq.empty[String])
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[IorNec[String, +*], String, Int] =
          _.parseInt.toIorNec("bad int")

        List("123", "456").intoF[IorNec[String, +*], List[Int]].transform ==> Ior.right(List(123, 456))
        Vector("123", "456").intoF[IorNec[String, +*], Queue[Int]].transform ==> Ior.right(Queue(123, 456))
        Array.empty[String].intoF[IorNec[String, +*], Seq[Int]].transform ==> Ior.right(Seq.empty[Int])
        Set("123", "456").intoF[IorNec[String, +*], Array[Int]].transform.right.get.sorted ==> Array(123, 456)

        List("abc", "456").intoF[IorNec[String, +*], List[Int]].transform ==> Ior.leftNec("bad int")
        Vector("123", "def").intoF[IorNec[String, +*], Queue[Int]].transform ==> Ior.leftNec("bad int")
        Array("abc", "def").intoF[IorNec[String, +*], Seq[Int]].transform ==> Ior.left(NonEmptyChain("bad int"))
        Set("123", "xyz").intoF[IorNec[String, +*], Array[Int]].transform ==> Ior.leftNec("bad int")
      }
    }

    test("wrapped maps") {

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        Map(1 -> 10, 2 -> 20).intoF[IorNec[String, +*], Map[String, String]].transform ==>
          Ior.right(Map("1" -> "10", "2" -> "20"))
        Map(1 -> 10, 2 -> 20).intoF[IorNec[String, +*], Map[String, Int]].transform ==>
          Ior.right(Map("1" -> 10, "2" -> 20))
        Seq(1 -> 10, 2 -> 20).intoF[IorNec[String, +*], Map[String, String]].transform ==>
          Ior.right(Map("1" -> "10", "2" -> "20"))
        ArrayBuffer(1 -> 10, 2 -> 20).intoF[IorNec[String, +*], Map[Int, String]].transform ==>
          Ior.right(Map(1 -> "10", 2 -> "20"))
        Map(1 -> 10, 2 -> 20).intoF[IorNec[String, +*], List[(String, String)]].transform ==>
          Ior.right(List("1" -> "10", "2" -> "20"))
        Map(1 -> 10, 2 -> 20).intoF[IorNec[String, +*], Vector[(String, Int)]].transform ==>
          Ior.right(Vector("1" -> 10, "2" -> 20))
        Array(1 -> 10, 2 -> 20).intoF[IorNec[String, +*], Map[String, String]].transform ==>
          Ior.right(Map("1" -> "10", "2" -> "20"))
        Array(1 -> 10, 2 -> 20).intoF[IorNec[String, +*], Map[Int, String]].transform ==>
          Ior.right(Map(1 -> "10", 2 -> "20"))
        Map(1 -> 10, 2 -> 20).intoF[IorNec[String, +*], Array[(String, String)]].transform.right.get ==>
          Array("1" -> "10", "2" -> "20")
        Map(1 -> 10, 2 -> 20).intoF[IorNec[String, +*], Array[(String, Int)]].transform.right.get ==>
          Array("1" -> 10, "2" -> 20)
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[IorNec[String, +*], String, Int] =
          _.parseInt.toIorNec("bad int")

        Map("1" -> "10", "2" -> "20").intoF[IorNec[String, +*], Map[Int, Int]].transform ==>
          Ior.right(Map(1 -> 10, 2 -> 20))
        Map("1" -> "10", "2" -> "20").intoF[IorNec[String, +*], Map[Int, String]].transform ==>
          Ior.right(Map(1 -> "10", 2 -> "20"))
        Seq("1" -> "10", "2" -> "20").intoF[IorNec[String, +*], Map[Int, Int]].transform ==>
          Ior.right(Map(1 -> 10, 2 -> 20))
        ArrayBuffer("1" -> "10", "2" -> "20").intoF[IorNec[String, +*], Map[String, Int]].transform ==>
          Ior.right(Map("1" -> 10, "2" -> 20))
        Map("1" -> "10", "2" -> "20").intoF[IorNec[String, +*], List[(Int, Int)]].transform ==>
          Ior.right(List(1 -> 10, 2 -> 20))
        Map("1" -> "10", "2" -> "20").intoF[IorNec[String, +*], Vector[(Int, String)]].transform ==>
          Ior.right(Vector(1 -> "10", 2 -> "20"))
        Array("1" -> "10", "2" -> "20").intoF[IorNec[String, +*], Map[Int, Int]].transform ==>
          Ior.right(Map(1 -> 10, 2 -> 20))
        Array("1" -> "10", "2" -> "20").intoF[IorNec[String, +*], Map[String, Int]].transform ==>
          Ior.right(Map("1" -> 10, "2" -> 20))
        Map("1" -> "10", "2" -> "20").intoF[IorNec[String, +*], Array[(Int, Int)]].transform.right.get ==>
          Array(1 -> 10, 2 -> 20)
        Map("1" -> "10", "2" -> "20").intoF[IorNec[String, +*], Array[(Int, String)]].transform.right.get ==>
          Array(1 -> "10", 2 -> "20")

        Map("1" -> "x", "y" -> "20").intoF[IorNec[String, +*], Map[Int, Int]].transform ==>
          Ior.left(NonEmptyChain("bad int"))
        Map("x" -> "10", "2" -> "20").intoF[IorNec[String, +*], Map[Int, String]].transform ==>
          Ior.leftNec("bad int")
        Seq("1" -> "10", "2" -> "x").intoF[IorNec[String, +*], Map[Int, Int]].transform ==>
          Ior.leftNec("bad int")
        ArrayBuffer("1" -> "x", "2" -> "y").intoF[IorNec[String, +*], Map[String, Int]].transform ==>
          Ior.left(NonEmptyChain("bad int"))
        Map("x" -> "10", "y" -> "z").intoF[IorNec[String, +*], ArrayBuffer[(Int, Int)]].transform ==>
          Ior.left(NonEmptyChain("bad int"))
        Map("1" -> "10", "x" -> "20").intoF[IorNec[String, +*], Vector[(Int, String)]].transform ==>
          Ior.leftNec("bad int")
        Array("x" -> "y", "z" -> "v").intoF[IorNec[String, +*], Map[Int, Int]].transform ==>
          Ior.left(NonEmptyChain("bad int"))
        Array("1" -> "x", "2" -> "y").intoF[IorNec[String, +*], Map[String, Int]].transform ==>
          Ior.left(NonEmptyChain("bad int"))
        Map("1" -> "10", "x" -> "20").intoF[IorNec[String, +*], Array[(Int, Int)]].transform ==>
          Ior.leftNec("bad int")
        Map("x" -> "10", "y" -> "20").intoF[IorNec[String, +*], Array[(Int, String)]].transform ==>
          Ior.left(NonEmptyChain("bad int"))
      }
    }

    test("wrapped eithers") {

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        (Left(1): Either[Int, Int]).intoF[IorNec[String, +*], Either[String, String]].transform ==>
          Ior.right(Left("1"))
        (Right(1): Either[Int, Int]).intoF[IorNec[String, +*], Either[String, String]].transform ==>
          Ior.right(Right("1"))
        Left(1).intoF[IorNec[String, +*], Either[String, String]].transform ==> Ior.right(Left("1"))
        Right(1).intoF[IorNec[String, +*], Either[String, String]].transform ==> Ior.right(Right("1"))
        Left(1).intoF[IorNec[String, +*], Left[String, String]].transform ==> Ior.right(Left("1"))
        Right(1).intoF[IorNec[String, +*], Right[String, String]].transform ==> Ior.right(Right("1"))
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[IorNec[String, +*], String, Int] =
          _.parseInt.toIorNec("bad int")

        (Left("1"): Either[String, String]).intoF[IorNec[String, +*], Either[Int, Int]].transform ==>
          Ior.right(Left(1))
        (Right("1"): Either[String, String]).intoF[IorNec[String, +*], Either[Int, Int]].transform ==>
          Ior.right(Right(1))
        Left("1").intoF[IorNec[String, +*], Either[Int, Int]].transform ==> Ior.right(Left(1))
        Right("1").intoF[IorNec[String, +*], Either[Int, Int]].transform ==> Ior.right(Right(1))
        Left("1").intoF[IorNec[String, +*], Either[Int, Int]].transform ==> Ior.right(Left(1))
        Right("1").intoF[IorNec[String, +*], Either[Int, Int]].transform ==> Ior.right(Right(1))

        (Left("x"): Either[String, String]).intoF[IorNec[String, +*], Either[Int, Int]].transform ==>
          Ior.leftNec("bad int")
        (Right("x"): Either[String, String]).intoF[IorNec[String, +*], Either[Int, Int]].transform ==>
          Ior.leftNec("bad int")
        Left("x").intoF[IorNec[String, +*], Either[Int, Int]].transform ==> Ior.leftNec("bad int")
        Right("x").intoF[IorNec[String, +*], Either[Int, Int]].transform ==> Ior.leftNec("bad int")
        Left("x").intoF[IorNec[String, +*], Either[Int, Int]].transform ==> Ior.leftNec("bad int")
        Right("x").intoF[IorNec[String, +*], Either[Int, Int]].transform ==> Ior.leftNec("bad int")
      }

      test("mixed inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString
        implicit val intParserValidated: TransformerF[IorNec[String, +*], String, Int] =
          _.parseInt.toIorNec("bad int")

        (Left("1"): Either[String, Int]).intoF[IorNec[String, +*], Either[Int, String]].transform ==>
          Ior.right(Left(1))
        (Left("x"): Either[String, Int]).intoF[IorNec[String, +*], Either[Int, String]].transform ==>
          Ior.leftNec("bad int")
        (Right(100): Either[String, Int]).intoF[IorNec[String, +*], Either[Int, String]].transform ==>
          Ior.right(Right("100"))

        (Left(100): Either[Int, String]).intoF[IorNec[String, +*], Either[String, Int]].transform ==>
          Ior.right(Left("100"))
        (Right("1"): Either[Int, String]).intoF[IorNec[String, +*], Either[String, Int]].transform ==>
          Ior.right(Right(1))
        (Right("x"): Either[Int, String]).intoF[IorNec[String, +*], Either[String, Int]].transform ==>
          Ior.leftNec("bad int")
      }
    }

    test("wrapped sealed families") {
      import numbers.*

      test("pure inner transformer") {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        import ScalesTransformerF.shortToLongPureInner

        (short.Zero: short.NumScale[Int, Nothing])
          .intoF[IorNec[String, +*], long.NumScale[String]]
          .transform ==> Ior.right(long.Zero)
        (short.Million(4): short.NumScale[Int, Nothing])
          .intoF[IorNec[String, +*], long.NumScale[String]]
          .transform ==> Ior.right(long.Million("4"))
        (short.Billion(2): short.NumScale[Int, Nothing])
          .intoF[IorNec[String, +*], long.NumScale[String]]
          .transform ==> Ior.right(long.Milliard("2"))
        (short.Trillion(100): short.NumScale[Int, Nothing])
          .intoF[IorNec[String, +*], long.NumScale[String]]
          .transform ==> Ior.right(long.Billion("100"))
      }

      test("wrapped inner transformer") {

        implicit val intParserValidated: TransformerF[IorNec[String, +*], String, Int] =
          _.parseInt.toIorNec("bad int")

        import ScalesTransformerF.shortToLongWrappedInner

        (short.Zero: short.NumScale[String, Nothing])
          .intoF[IorNec[String, +*], long.NumScale[Int]]
          .transform ==> Ior.right(long.Zero)
        (short.Million("4"): short.NumScale[String, Nothing])
          .intoF[IorNec[String, +*], long.NumScale[Int]]
          .transform ==> Ior.right(long.Million(4))
        (short.Billion("2"): short.NumScale[String, Nothing])
          .intoF[IorNec[String, +*], long.NumScale[Int]]
          .transform ==> Ior.right(long.Milliard(2))
        (short.Trillion("100"): short.NumScale[String, Nothing])
          .intoF[IorNec[String, +*], long.NumScale[Int]]
          .transform ==> Ior.right(long.Billion(100))

        (short.Million("x"): short.NumScale[String, Nothing])
          .intoF[IorNec[String, +*], long.NumScale[Int]]
          .transform ==> Ior.leftNec("bad int")
        (short.Billion("x"): short.NumScale[String, Nothing])
          .intoF[IorNec[String, +*], long.NumScale[Int]]
          .transform ==> Ior.leftNec("bad int")
        (short.Trillion("x"): short.NumScale[String, Nothing])
          .intoF[IorNec[String, +*], long.NumScale[Int]]
          .transform ==> Ior.leftNec("bad int")
      }
    }
  }
}
