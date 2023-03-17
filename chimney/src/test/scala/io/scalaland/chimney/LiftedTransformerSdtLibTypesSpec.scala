package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.utils.EitherUtils.*
import io.scalaland.chimney.utils.OptionUtils.*
import utest.*

import scala.annotation.nowarn
import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer

object LiftedTransformerSdtLibTypesSpec extends TestSuite {

  val tests = Tests {

    test("not support converting non-Unit field to Unit field if there is no implicit converter allowing that") {
      case class Buzz(value: String)
      case class ConflictingFooBuzz(value: Unit)

      test("when F = Option") {
        compileError("""Buzz("a").transformIntoF[Option, ConflictingFooBuzz]""")
          .check(
            "",
            "Chimney can't derive transformation from Buzz to ConflictingFooBuzz",
            "io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.ConflictingFooBuzz",
            "value: scala.Unit - can't derive transformation from value: java.lang.String in source type io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.Buzz",
            "scala.Unit",
            "derivation from buzz.value: java.lang.String to scala.Unit is not supported in Chimney!",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
      }

      test("when F = Either[List[String], +*]") {
        @nowarn type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

        compileError("""Buzz("a").transformIntoF[EitherList, ConflictingFooBuzz]""").check(
          "",
          "Chimney can't derive transformation from Buzz to ConflictingFooBuzz",
          "io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.ConflictingFooBuzz",
          "value: scala.Unit - can't derive transformation from value: java.lang.String in source type io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.Buzz",
          "scala.Unit",
          "derivation from buzz.value: java.lang.String to scala.Unit is not supported in Chimney!",
          "Consult https://scalalandio.github.io/chimney for usage examples."
        )
      }
    }

    test("support automatically filling of scala.Unit") {
      case class Buzz(value: String)
      case class NewBuzz(value: String, unit: Unit)
      case class FooBuzz(unit: Unit)
      case class ConflictingFooBuzz(value: Unit)

      test("when F = Option") {
        Buzz("a").transformIntoF[Option, NewBuzz] ==> Some(NewBuzz("a", ()))
        Buzz("a").transformIntoF[Option, FooBuzz] ==> Some(FooBuzz(()))
        NewBuzz("a", null.asInstanceOf[Unit]).transformIntoF[Option, FooBuzz] ==> Some(FooBuzz(null.asInstanceOf[Unit]))
      }

      test("when F = Either[List[String], +*]") {
        Buzz("a").transformIntoF[Either[List[String], +*], NewBuzz] ==> Right(NewBuzz("a", ()))
        Buzz("a").transformIntoF[Either[List[String], +*], FooBuzz] ==> Right(FooBuzz(()))
        NewBuzz("a", null.asInstanceOf[Unit]).transformIntoF[Either[List[String], +*], FooBuzz] ==> Right(
          FooBuzz(null.asInstanceOf[Unit])
        )
      }
    }

    test("transform from Option-type into Option-type, using Total Transformer for inner type transformation") {

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test("when F = Option") {
        Option(123).transformIntoF[Option, Option[String]] ==> Some(Some("123"))
        Option.empty[Int].transformIntoF[Option, Option[String]] ==> Some(None)
      }

      test("when F = Either[List[String], +*]") {
        Option(123).transformIntoF[Either[List[String], +*], Option[String]] ==> Right(Some("123"))
        Option.empty[Int].transformIntoF[Either[List[String], +*], Option[String]] ==> Right(None)
      }
    }

    test("transform from Option-type into Option-type, using Lifted Transformer for inner type transformation") {

      test("when F = Option") {
        implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

        Option("123").transformIntoF[Option, Option[Int]] ==> Some(Some(123))
        Option("abc").transformIntoF[Option, Option[Int]] ==> None
        Option.empty[String].transformIntoF[Option, Option[Int]] ==> Some(None)
      }

      test("when F = Either[List[String], +*]") {
        implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
          _.parseInt.toEitherList("bad int")

        Option("123").transformIntoF[Either[List[String], +*], Option[Int]] ==> Right(Some(123))
        Option("abc").transformIntoF[Either[List[String], +*], Option[Int]] ==> Left(List("bad int"))
        Option.empty[String].transformIntoF[Either[List[String], +*], Option[Int]] ==> Right(None)
      }
    }

    test("transform from non-Option-type into Option-type, using Total Transformer for inner type transformation") {

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test("when F = Option") {
        10.transformIntoF[Option, Option[String]] ==> Some(Some("10"))
        (null: String).transformIntoF[Option, Option[String]] ==> Some(None)
      }

      test("when F = Either[List[String], +*]") {
        10.transformIntoF[Either[List[String], +*], Option[String]] ==> Right(Some("10"))
        (null: String).transformIntoF[Either[List[String], +*], Option[String]] ==> Right(None)
      }
    }

    test("transform from non-Option-type into Option-type, using Lifted Transformer for inner type transformation") {

      test("when F = Option") {

        implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

        "10".transformIntoF[Option, Option[Int]] ==> Some(Some(10))
        (null: String).transformIntoF[Option, Option[Int]] ==> Some(None)
        "x".transformIntoF[Option, Option[Int]] ==> None
      }

      test("when F = Either[List[String], +*]") {

        implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
          _.parseInt.toEitherList("bad int")

        "10".transformIntoF[Either[List[String], +*], Option[Int]] ==> Right(Some(10))
        (null: String).transformIntoF[Either[List[String], +*], Option[Int]] ==> Right(None)
        "x".transformIntoF[Either[List[String], +*], Option[Int]] ==> Left(List("bad int"))
      }
    }

    test("transform from Either-type into Either-type, using Total Transformer for inner types transformation") {

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test("when F = Option") {
        (Left(1): Either[Int, Int]).transformIntoF[Option, Either[String, String]] ==> Some(Left("1"))
        (Right(1): Either[Int, Int]).transformIntoF[Option, Either[String, String]] ==> Some(Right("1"))
        Left(1).transformIntoF[Option, Either[String, String]] ==> Some(Left("1"))
        Right(1).transformIntoF[Option, Either[String, String]] ==> Some(Right("1"))
        Left(1).transformIntoF[Option, Left[String, String]] ==> Some(Left("1"))
        Right(1).transformIntoF[Option, Right[String, String]] ==> Some(Right("1"))
      }

      test("when F = Either[List[String], +*]") {
        (Left(1): Either[Int, Int]).transformIntoF[Either[List[String], +*], Either[String, String]] ==>
          Right(Left("1"))
        (Right(1): Either[Int, Int]).transformIntoF[Either[List[String], +*], Either[String, String]] ==>
          Right(Right("1"))
        Left(1).transformIntoF[Either[List[String], +*], Either[String, String]] ==> Right(Left("1"))
        Right(1).transformIntoF[Either[List[String], +*], Either[String, String]] ==> Right(Right("1"))
        Left(1).transformIntoF[Either[List[String], +*], Left[String, String]] ==> Right(Left("1"))
        Right(1).transformIntoF[Either[List[String], +*], Right[String, String]] ==> Right(Right("1"))
      }
    }

    test("transform from Either-type into Either-type, using Total Transformer for inner types transformation") {

      test("when F = Option") {
        implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

        (Left("1"): Either[String, String]).transformIntoF[Option, Either[Int, Int]] ==> Some(Left(1))
        (Right("1"): Either[String, String]).transformIntoF[Option, Either[Int, Int]] ==> Some(Right(1))
        Left("1").transformIntoF[Option, Either[Int, Int]] ==> Some(Left(1))
        Right("1").transformIntoF[Option, Either[Int, Int]] ==> Some(Right(1))
        Left("1").transformIntoF[Option, Left[Int, Int]] ==> Some(Left(1))
        Right("1").transformIntoF[Option, Right[Int, Int]] ==> Some(Right(1))

        (Left("x"): Either[String, String]).transformIntoF[Option, Either[Int, Int]] ==> None
        (Right("x"): Either[String, String]).transformIntoF[Option, Either[Int, Int]] ==> None
        Left("x").transformIntoF[Option, Either[Int, Int]] ==> None
        Right("x").transformIntoF[Option, Either[Int, Int]] ==> None
        Left("x").transformIntoF[Option, Left[Int, Int]] ==> None
        Right("x").transformIntoF[Option, Right[Int, Int]] ==> None
      }

      test("when F = Either[List[String], +*]") {
        implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
          _.parseInt.toEitherList("bad int")

        (Left("1"): Either[String, String]).transformIntoF[Either[List[String], +*], Either[Int, Int]] ==>
          Right(Left(1))
        (Right("1"): Either[String, String]).transformIntoF[Either[List[String], +*], Either[Int, Int]] ==>
          Right(Right(1))
        Left("1").transformIntoF[Either[List[String], +*], Either[Int, Int]] ==> Right(Left(1))
        Right("1").transformIntoF[Either[List[String], +*], Either[Int, Int]] ==> Right(Right(1))
        Left("1").transformIntoF[Either[List[String], +*], Either[Int, Int]] ==> Right(Left(1))
        Right("1").transformIntoF[Either[List[String], +*], Either[Int, Int]] ==> Right(Right(1))

        (Left("x"): Either[String, String]).transformIntoF[Either[List[String], +*], Either[Int, Int]] ==>
          Left(List("bad int"))
        (Right("x"): Either[String, String]).transformIntoF[Either[List[String], +*], Either[Int, Int]] ==>
          Left(List("bad int"))
        Left("x").transformIntoF[Either[List[String], +*], Either[Int, Int]] ==> Left(List("bad int"))
        Right("x").transformIntoF[Either[List[String], +*], Either[Int, Int]] ==> Left(List("bad int"))
        Left("x").transformIntoF[Either[List[String], +*], Either[Int, Int]] ==> Left(List("bad int"))
        Right("x").transformIntoF[Either[List[String], +*], Either[Int, Int]] ==> Left(List("bad int"))
      }
    }

    test(
      "transform from Either-type into Either-type, using Total and Lifted Transformer for inner types transformation"
    ) {

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test("when F = Option") {
        implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

        (Left("1"): Either[String, Int]).transformIntoF[Option, Either[Int, String]] ==> Some(Left(1))
        (Left("x"): Either[String, Int]).transformIntoF[Option, Either[Int, String]] ==> None
        (Right(100): Either[String, Int]).transformIntoF[Option, Either[Int, String]] ==> Some(Right("100"))

        (Left(100): Either[Int, String]).transformIntoF[Option, Either[String, Int]] ==> Some(Left("100"))
        (Right("1"): Either[Int, String]).transformIntoF[Option, Either[String, Int]] ==> Some(Right(1))
        (Right("x"): Either[Int, String]).transformIntoF[Option, Either[String, Int]] ==> None
      }

      test("when F = Either[List[String], +*]") {
        implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
          _.parseInt.toEitherList("bad int")

        (Left("1"): Either[String, Int]).transformIntoF[Either[List[String], +*], Either[Int, String]] ==>
          Right(Left(1))
        (Left("x"): Either[String, Int]).transformIntoF[Either[List[String], +*], Either[Int, String]] ==>
          Left(List("bad int"))
        (Right(100): Either[String, Int]).transformIntoF[Either[List[String], +*], Either[Int, String]] ==>
          Right(Right("100"))

        (Left(100): Either[Int, String]).transformIntoF[Either[List[String], +*], Either[String, Int]] ==>
          Right(Left("100"))
        (Right("1"): Either[Int, String]).transformIntoF[Either[List[String], +*], Either[String, Int]] ==>
          Right(Right(1))
        (Right("x"): Either[Int, String]).transformIntoF[Either[List[String], +*], Either[String, Int]] ==>
          Left(List("bad int"))
      }
    }

    // TODO: transform from non-either to either

    test("transform from Iterable-type to Iterable-type, using Total Transformer for inner type transformation") {

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test("when F = Option") {
        List(123, 456).transformIntoF[Option, Vector[String]] ==> Some(Vector("123", "456"))
        Vector(123, 456).transformIntoF[Option, Queue[String]] ==> Some(Queue("123", "456"))
        Queue(123, 456).transformIntoF[Option, List[String]] ==> Some(List("123", "456"))

      }

      test("when F = Either[List[String], +*]") {
        List(123, 456).transformIntoF[Either[List[String], +*], Vector[String]] ==> Right(Vector("123", "456"))
        Vector(123, 456).transformIntoF[Either[List[String], +*], Queue[String]] ==> Right(Queue("123", "456"))
        Queue(123, 456).transformIntoF[Either[List[String], +*], List[String]] ==> Right(List("123", "456"))
      }
    }

    test("transform from Iterable-type to Iterable-type, using Lifted Transformer for inner type transformation") {

      test("when F = Option") {
        implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

        List("123", "456").transformIntoF[Option, List[Int]] ==> Some(List(123, 456))
        Vector("123", "456").transformIntoF[Option, Queue[Int]] ==> Some(Queue(123, 456))
        Queue("123", "456").transformIntoF[Option, List[Int]].get.sorted ==> List(123, 456)

        List("abc", "456").transformIntoF[Option, Vector[Int]] ==> None
        Vector("123", "def").transformIntoF[Option, Queue[Int]] ==> None
        Queue("123", "xyz").transformIntoF[Option, List[Int]] ==> None
      }

      test("when F = Either[List[String], +*]") {
        implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
          _.parseInt.toEitherList("bad int")

        List("123", "456").transformIntoF[Either[List[String], +*], Vector[Int]] ==> Right(Vector(123, 456))
        Vector("123", "456").transformIntoF[Either[List[String], +*], Queue[Int]] ==> Right(Queue(123, 456))
        Queue("123", "456").transformIntoF[Either[List[String], +*], List[Int]].toOption.get.sorted ==> List(123, 456)

        List("abc", "456").transformIntoF[Either[List[String], +*], Vector[Int]] ==> Left(Vector("bad int"))
        Vector("123", "def").transformIntoF[Either[List[String], +*], Queue[Int]] ==> Left(List("bad int"))
        Queue("123", "xyz").transformIntoF[Either[List[String], +*], List[Int]] ==> Left(List("bad int"))
      }
    }

    test("transform from Array-type to Array-type, using Total Transformer for inner type transformation") {

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test("when F = Option") {
        Array(123, 456).transformIntoF[Option, Array[String]].get ==> Array("123", "456")
      }

      test("when F = Either[List[String], +*]") {
        Array(123, 456).transformIntoF[Either[List[String], +*], Array[String]].toOption.get ==> Array("123", "456")
      }
    }

    test("transform from Array-type to Array-type, using Lifted Transformer for inner type transformation") {

      test("when F = Option") {
        implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

        Array("123", "456").transformIntoF[Option, Array[Int]].get ==> Array(123, 456)
        Array("abc", "456").transformIntoF[Option, Array[Int]] ==> None
      }

      test("when F = Either[List[String], +*]") {
        implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
          _.parseInt.toEitherList("bad int")

        Array("123", "456").transformIntoF[Either[List[String], +*], Array[Int]].toOption.get ==> Array(123, 456)
        Array("abc", "456").transformIntoF[Either[List[String], +*], Array[Int]] ==> Left(List("bad int"))
      }
    }

    test("transform between Array-type and Iterable-type, using Total Transformer for inner value transformation") {

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test("when F = Option") {
        Array(123, 456).transformIntoF[Option, Set[String]] ==> Some(Set("123", "456"))
        Array.empty[Int].transformIntoF[Option, Set[String]] ==> Some(Set.empty[String])
      }

      test("when F = Either[List[String], +*]") {
        Array(123, 456).transformIntoF[Either[List[String], +*], Set[String]] ==> Right(Set("123", "456"))
        Array.empty[Int].transformIntoF[Either[List[String], +*], Set[String]] ==> Right(Set.empty[String])
      }
    }

    test("transform between Array-type and Iterable-type, using Lifted Transformer for inner value transformation") {

      test("when F = Option") {
        implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

        Set("123", "456").transformIntoF[Option, Array[Int]].get ==> Array(123, 456)
        Set("123", "xyz").transformIntoF[Option, Array[Int]] ==> None
        Set.empty[String].transformIntoF[Option, Array[Int]].get ==> Array.empty[String]

        Array("123", "456").transformIntoF[Option, Set[Int]] ==> Some(Set(123, 456))
        Array("123", "xyz").transformIntoF[Option, Set[Int]] ==> None
        Array.empty[String].transformIntoF[Option, Set[Int]] ==> Some(Set.empty[Int])
      }

      test("when F = Either[List[String], +*]") {
        implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
          _.parseInt.toEitherList("bad int")

        Set("123", "456").transformIntoF[Either[List[String], +*], Array[Int]].toOption.get ==> Array(123, 456)
        Set("123", "xyz").transformIntoF[Either[List[String], +*], Array[Int]] ==> Left(List("bad int"))
        Set.empty[String].transformIntoF[Either[List[String], +*], Array[Int]].toOption.get ==> Array.empty[String]

        Array("123", "456").transformIntoF[Either[List[String], +*], Set[Int]] ==> Right(Set(123, 456))
        Array("123", "xyz").transformIntoF[Either[List[String], +*], Set[Int]] ==> Left(List("bad int"))
        Array.empty[String].transformIntoF[Either[List[String], +*], Set[Int]] ==> Right(Set.empty[Int])
      }
    }

    test("transform from Map-type to Map-type, using Total Transformer for inner value transformation") {

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test("when F = Option") {
        Map(1 -> 10, 2 -> 20).transformIntoF[Option, Map[String, String]] ==> Some(Map("1" -> "10", "2" -> "20"))
        Map(1 -> 10, 2 -> 20).transformIntoF[Option, Map[String, Int]] ==> Some(Map("1" -> 10, "2" -> 20))
      }

      test("when F = Either[List[String], +*]") {
        Map(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Map[String, String]] ==>
          Right(Map("1" -> "10", "2" -> "20"))
        Map(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Map[String, Int]] ==>
          Right(Map("1" -> 10, "2" -> 20))
      }
    }

    test("transform from Map-type to Map-type, using Lifted Transformer for inner value transformation") {
      test("when F = Option") {
        implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

        Map("1" -> "10", "2" -> "20").transformIntoF[Option, Map[Int, Int]] ==> Some(Map(1 -> 10, 2 -> 20))
        Map("1" -> "10", "2" -> "20").transformIntoF[Option, Map[Int, String]] ==> Some(Map(1 -> "10", 2 -> "20"))

        Map("1" -> "x", "y" -> "20").transformIntoF[Option, Map[Int, Int]] ==> None
        Map("x" -> "10", "2" -> "20").transformIntoF[Option, Map[Int, String]] ==> None
      }

      test("when F = Either[List[String], +*]") {
        implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
          _.parseInt.toEitherList("bad int")

        Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[Int, Int]] ==>
          Right(Map(1 -> 10, 2 -> 20))
        Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[Int, String]] ==>
          Right(Map(1 -> "10", 2 -> "20"))

        Map("1" -> "x", "y" -> "20").transformIntoF[Either[List[String], +*], Map[Int, Int]] ==>
          Left(List("bad int", "bad int"))
        Map("x" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[Int, String]] ==>
          Left(List("bad int"))
      }
    }

    test("transform between Iterables and Maps, using Total Transformer for inner type transformation") {

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test("when F = Option") {
        Seq(1 -> 10, 2 -> 20).transformIntoF[Option, Map[String, String]] ==> Some(Map("1" -> "10", "2" -> "20"))
        ArrayBuffer(1 -> 10, 2 -> 20).transformIntoF[Option, Map[Int, String]] ==> Some(Map(1 -> "10", 2 -> "20"))
        Map(1 -> 10, 2 -> 20).transformIntoF[Option, List[(String, String)]] ==> Some(List("1" -> "10", "2" -> "20"))
        Map(1 -> 10, 2 -> 20).transformIntoF[Option, Vector[(String, Int)]] ==> Some(Vector("1" -> 10, "2" -> 20))
      }

      test("when F = Either[List[String], +*]") {
        Seq(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Map[String, String]] ==>
          Right(Map("1" -> "10", "2" -> "20"))
        ArrayBuffer(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Map[Int, String]] ==>
          Right(Map(1 -> "10", 2 -> "20"))
        Map(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], List[(String, String)]] ==>
          Right(List("1" -> "10", "2" -> "20"))
        Map(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Vector[(String, Int)]] ==>
          Right(Vector("1" -> 10, "2" -> 20))
      }
    }

    test("transform between Iterables and Maps, using Lifted Transformer for inner type transformation") {

      test("when F = Option") {
        implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

        Seq("1" -> "10", "2" -> "20").transformIntoF[Option, Map[Int, Int]] ==> Some(Map(1 -> 10, 2 -> 20))
        ArrayBuffer("1" -> "10", "2" -> "20").transformIntoF[Option, Map[String, Int]] ==>
          Some(Map("1" -> 10, "2" -> 20))
        Map("1" -> "10", "2" -> "20").transformIntoF[Option, List[(Int, Int)]] ==> Some(List(1 -> 10, 2 -> 20))
        Map("1" -> "10", "2" -> "20").transformIntoF[Option, Vector[(Int, String)]] ==>
          Some(Vector(1 -> "10", 2 -> "20"))

        Seq("1" -> "10", "2" -> "x").transformIntoF[Option, Map[Int, Int]] ==> None
        ArrayBuffer("1" -> "x", "2" -> "y").transformIntoF[Option, Map[String, Int]] ==> None
        Map("x" -> "10", "y" -> "z").transformIntoF[Option, List[(Int, Int)]] ==> None
        Map("1" -> "10", "x" -> "20").transformIntoF[Option, Vector[(Int, String)]] ==> None
      }

      test("when F = Either[List[String], +*]") {
        implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
          _.parseInt.toEitherList("bad int")

        Seq("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[Int, Int]] ==>
          Right(Map(1 -> 10, 2 -> 20))
        ArrayBuffer("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[String, Int]] ==>
          Right(Map("1" -> 10, "2" -> 20))
        Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], List[(Int, Int)]] ==>
          Right(List(1 -> 10, 2 -> 20))
        Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Vector[(Int, String)]] ==>
          Right(Vector(1 -> "10", 2 -> "20"))

        Seq("1" -> "10", "2" -> "x").transformIntoF[Either[List[String], +*], Map[Int, Int]] ==>
          Left(List("bad int"))
        ArrayBuffer("1" -> "x", "2" -> "y").transformIntoF[Either[List[String], +*], Map[String, Int]] ==>
          Left(List("bad int", "bad int"))
        Map("x" -> "10", "y" -> "z").transformIntoF[Either[List[String], +*], ArrayBuffer[(Int, Int)]] ==>
          Left(List("bad int", "bad int", "bad int"))
        Map("1" -> "10", "x" -> "20").transformIntoF[Either[List[String], +*], Vector[(Int, String)]] ==>
          Left(List("bad int"))
      }
    }

    test("transform between Arrays and Maps, using Total Transformer for inner type transformation") {

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test("when F = Option") {
        Array(1 -> 10, 2 -> 20).transformIntoF[Option, Map[String, String]] ==> Some(Map("1" -> "10", "2" -> "20"))
        Array(1 -> 10, 2 -> 20).transformIntoF[Option, Map[Int, String]] ==> Some(Map(1 -> "10", 2 -> "20"))
        Map(1 -> 10, 2 -> 20).transformIntoF[Option, Array[(String, String)]].get ==> Array("1" -> "10", "2" -> "20")
        Map(1 -> 10, 2 -> 20).transformIntoF[Option, Array[(String, Int)]].get ==> Array("1" -> 10, "2" -> 20)
      }

      test("when F = Either[List[String], +*]") {
        Array(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Map[String, String]] ==>
          Right(Map("1" -> "10", "2" -> "20"))
        Array(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Map[Int, String]] ==>
          Right(Map(1 -> "10", 2 -> "20"))
        Map(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Array[(String, String)]].toOption.get ==>
          Array("1" -> "10", "2" -> "20")
        Map(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Array[(String, Int)]].toOption.get ==>
          Array("1" -> 10, "2" -> 20)
      }
    }

    test("transform between Arrays and Maps, using Lifted Transformer for inner type transformation") {

      test("when F = Option") {
        implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

        Array("1" -> "10", "2" -> "20").transformIntoF[Option, Map[Int, Int]] ==> Some(Map(1 -> 10, 2 -> 20))
        Array("1" -> "10", "2" -> "20").transformIntoF[Option, Map[String, Int]] ==> Some(Map("1" -> 10, "2" -> 20))
        Map("1" -> "10", "2" -> "20").transformIntoF[Option, Array[(Int, Int)]].get ==> Array(1 -> 10, 2 -> 20)
        Map("1" -> "10", "2" -> "20").transformIntoF[Option, Array[(Int, String)]].get ==> Array(1 -> "10", 2 -> "20")

        Array("x" -> "y", "z" -> "v").transformIntoF[Option, Map[Int, Int]] ==> None
        Array("1" -> "x", "2" -> "y").transformIntoF[Option, Map[String, Int]] ==> None
        Map("1" -> "10", "x" -> "20").transformIntoF[Option, Array[(Int, Int)]] ==> None
        Map("x" -> "10", "y" -> "20").transformIntoF[Option, Array[(Int, String)]] ==> None
      }

      test("when F = Either[List[String], +*]") {
        implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
          _.parseInt.toEitherList("bad int")

        Array("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[Int, Int]] ==>
          Right(Map(1 -> 10, 2 -> 20))
        Array("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[String, Int]] ==>
          Right(Map("1" -> 10, "2" -> 20))
        Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Array[(Int, Int)]].toOption.get ==>
          Array(1 -> 10, 2 -> 20)
        Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Array[(Int, String)]].toOption.get ==>
          Array(1 -> "10", 2 -> "20")

        Array("x" -> "y", "z" -> "v").transformIntoF[Either[List[String], +*], Map[Int, Int]] ==>
          Left(List("bad int", "bad int", "bad int", "bad int"))
        Array("1" -> "x", "2" -> "y").transformIntoF[Either[List[String], +*], Map[String, Int]] ==>
          Left(List("bad int", "bad int"))
        Map("1" -> "10", "x" -> "20").transformIntoF[Either[List[String], +*], Array[(Int, Int)]] ==>
          Left(List("bad int"))
        Map("x" -> "10", "y" -> "20").transformIntoF[Either[List[String], +*], Array[(Int, String)]] ==>
          Left(List("bad int", "bad int"))
      }
    }

    test("flag .enableOptionDefaultsToNone") {

      case class Source(x: String)
      case class TargetWithOption(x: String, y: Option[Int])
      case class TargetWithOptionAndDefault(x: String, y: Option[Int] = Some(42))

      test("should be turned off by default and not allow compiling Option fields with missing source") {

        test("when F = Option") {
          compileError(
            """Source("foo").intoF[Option, TargetWithOption].transform ==> Some(TargetWithOption("foo", None))"""
          ).check(
            "",
            "Chimney can't derive transformation from Source to TargetWithOption",
            "io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.TargetWithOption",
            "y: scala.Option - no accessor named y in source type io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.Source",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }

        test("when F = Either[List[String], +*]") {
          @nowarn type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
          compileError(
            """Source("foo").intoF[EitherList, TargetWithOption].transform ==> Right(TargetWithOption("foo", None))"""
          ).check(
            "",
            "Chimney can't derive transformation from Source to TargetWithOption",
            "io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.TargetWithOption",
            "y: scala.Option - no accessor named y in source type io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.Source",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }
      }

      test("use None for fields without source nor default value when enabled") {

        test("when F = Option") {
          Source("foo").intoF[Option, TargetWithOption].enableOptionDefaultsToNone.transform ==> Some(
            TargetWithOption("foo", None)
          )
        }

        test("when F = Either[List[String], +*]") {
          Source("foo")
            .intoF[Either[List[String], +*], TargetWithOption]
            .enableOptionDefaultsToNone
            .transform ==> Right(
            TargetWithOption("foo", None)
          )
        }
      }

      test("use None for fields without source but with default value when enabled but default values disabled") {

        test("when F = Option") {
          Source("foo").intoF[Option, TargetWithOptionAndDefault].enableOptionDefaultsToNone.transform ==> Some(
            TargetWithOptionAndDefault("foo", None)
          )
        }

        test("when F = Either[List[String], +*]") {
          Source("foo")
            .intoF[Either[List[String], +*], TargetWithOptionAndDefault]
            .enableOptionDefaultsToNone
            .transform ==> Right(
            TargetWithOptionAndDefault("foo", None)
          )
        }
      }

      test("should be ignored when default value is set and default values enabled") {

        test("when F = Option") {
          Source("foo")
            .intoF[Option, TargetWithOption]
            .enableDefaultValues
            .enableOptionDefaultsToNone
            .transform ==> Some(
            TargetWithOption("foo", None)
          )
          Source("foo")
            .intoF[Option, TargetWithOptionAndDefault]
            .enableDefaultValues
            .enableOptionDefaultsToNone
            .transform ==> Some(
            TargetWithOptionAndDefault(
              "foo",
              Some(42)
            )
          )
        }

        test("when F = Either[List[String], +*]") {
          Source("foo")
            .intoF[Either[List[String], +*], TargetWithOption]
            .enableDefaultValues
            .enableOptionDefaultsToNone
            .transform ==> Right(TargetWithOption("foo", None))
          Source("foo")
            .intoF[Either[List[String], +*], TargetWithOptionAndDefault]
            .enableDefaultValues
            .enableOptionDefaultsToNone
            .transform ==> Right(
            TargetWithOptionAndDefault(
              "foo",
              Some(42)
            )
          )
        }
      }
    }

    test("flag .enableUnsafeOption") {

      case class Source(x: Option[Int])
      case class Target(x: String)

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test(
        "should be turned off by default and not allow compiling Option[T] to S without explicitly existing converter"
      ) {

        test("when F = Option") {
          compileError("""Source(Some(1)).intoF[Option, Target].transform ==> Some(Target("1"))""").check(
            "",
            "Chimney can't derive transformation from Source to Target",
            "java.lang.String",
            "derivation from source.x: scala.Option to java.lang.String is not supported in Chimney!",
            "io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.Target",
            "x: java.lang.String - can't derive transformation from x: scala.Option in source type io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.Source",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }

        test("when F = Either[List[String], +*]") {
          @nowarn type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
          compileError("""Source(Some(1)).intoF[EitherList, Target].transform ==> Right(Target("1"))""").check(
            "",
            "Chimney can't derive transformation from Source to Target",
            "java.lang.String",
            "derivation from source.x: scala.Option to java.lang.String is not supported in Chimney!",
            "io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.Target",
            "x: java.lang.String - can't derive transformation from x: scala.Option in source type io.scalaland.chimney.LiftedTransformerSdtLibTypesSpec.Source",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }
      }

      test("use .get to extract Option source when enabled") {

        test("when F = Option") {
          Option(10).intoF[Option, String].enableUnsafeOption.transform ==> Some("10")
          Source(Some(10)).intoF[Option, Target].enableUnsafeOption.transform ==> Some(Target("10"))
          intercept[NoSuchElementException] {
            Option.empty[Int].intoF[Option, String].enableUnsafeOption.transform
          }
          intercept[NoSuchElementException] {
            Source(None).intoF[Option, Target].enableUnsafeOption.transform
          }
        }

        test("when F = Either[List[String], +*]") {
          Option(10).intoF[Either[List[String], +*], String].enableUnsafeOption.transform ==> Right("10")
          Source(Some(10)).intoF[Either[List[String], +*], Target].enableUnsafeOption.transform ==> Right(Target("10"))
          intercept[NoSuchElementException] {
            Option.empty[Int].intoF[Either[List[String], +*], String].enableUnsafeOption.transform
          }
          intercept[NoSuchElementException] {
            Source(None).intoF[Either[List[String], +*], Target].enableUnsafeOption.transform
          }
        }
      }

      test("should be ignored if implicit (presumably safe) Total Transformer from Option exists") {
        implicit val optIntPrinter: Transformer[Option[Int], String] = _.map(_ * 2).fold("empty")(_.toString)

        test("when F = Option") {
          Option(10).intoF[Option, String].enableUnsafeOption.transform ==> Some("20")
          Source(Some(10)).intoF[Option, Target].enableUnsafeOption.transform ==> Some(Target("20"))
          Option.empty[Int].intoF[Option, String].enableUnsafeOption.transform ==> Some("empty")
          Source(None).intoF[Option, Target].enableUnsafeOption.transform ==> Some(Target("empty"))
        }

        test("when F = Either[List[String], +*]") {
          Option(10).intoF[Either[List[String], +*], String].enableUnsafeOption.transform ==> Right("20")
          Source(Some(10)).intoF[Either[List[String], +*], Target].enableUnsafeOption.transform ==> Right(Target("20"))
          Option.empty[Int].intoF[Either[List[String], +*], String].enableUnsafeOption.transform ==> Right("empty")
          Source(None).intoF[Either[List[String], +*], Target].enableUnsafeOption.transform ==> Right(Target("empty"))
        }
      }

      test("should be ignored if implicit (presumably safe) Lifted Transformer from Option exists") {

        test("when F = Option") {
          implicit val optIntPrinter: TransformerF[Option, Option[Int], String] = _.map(_ * 2).map(_.toString)

          Option(10).intoF[Option, String].enableUnsafeOption.transform ==> Some("20")
          Source(Some(10)).intoF[Option, Target].enableUnsafeOption.transform ==> Some(Target("20"))
          Option.empty[Int].intoF[Option, String].enableUnsafeOption.transform ==> None
          Source(None).intoF[Option, Target].enableUnsafeOption.transform ==> None
        }

        test("when F = Either[List[String], +*]") {
          implicit val optIntPrinter: TransformerF[Either[List[String], +*], Option[Int], String] =
            _.map(_ * 2).fold[Either[List[String], String]](Left(List("bad int")))(a => Right(a.toString))

          Option(10).intoF[Either[List[String], +*], String].enableUnsafeOption.transform ==> Right("20")
          Source(Some(10)).intoF[Either[List[String], +*], Target].enableUnsafeOption.transform ==> Right(Target("20"))
          Option.empty[Int].intoF[Either[List[String], +*], String].enableUnsafeOption.transform ==> Left(
            List("bad int")
          )
          Source(None).intoF[Either[List[String], +*], Target].enableUnsafeOption.transform ==> Left(List("bad int"))
        }
      }
    }
  }
}
