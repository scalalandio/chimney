package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.examples.trip._
import io.scalaland.chimney.utils.EitherUtils._
import io.scalaland.chimney.utils.OptionUtils._
import utest._

import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer

object DslFSpec extends TestSuite {

  val tests = Tests {

    "transform always succeeds" - {

      "option" - {
        Person("John", 10, 140).transformIntoF[Option, User] ==> Some(User("John", 10, 140))
      }

      "either" - {
        Person("John", 10, 140).transformIntoF[Either[Vector[String], +*], User] ==> Right(User("John", 10, 140))
      }
    }

    "transform always fails" - {

      "option" - {
        Person("John", 10, 140)
          .intoF[Option, User]
          .withFieldConstF(_.height, Option.empty[Double])
          .transform ==> None
      }

      "either" - {
        Person("John", 10, 140)
          .intoF[Either[List[String], +*], User]
          .withFieldConstF(_.height, Left(List("abc", "def")))
          .transform ==> Left(List("abc", "def"))
      }
    }

    "simple transform with validation" - {

      "success" - {
        val okForm = PersonForm("John", "10", "140")

        "1-arg" - {

          "option" - {
            okForm
              .into[Person]
              .withFieldConst(_.age, 7)
              .withFieldComputedF(_.height, _.height.parseDouble)
              .transform ==> Some(Person("John", 7, 140))
          }

          "either" - {
            okForm
              .into[Person]
              .withFieldConst(_.height, 200.5)
              .withFieldComputedF[Either[List[String], +*], Int, Int](_.age, _.age.parseInt.toEither("bad age"))
              .transform ==> Right(Person("John", 10, 200.5))
          }
        }

        "2-arg" - {

          "option" - {
            okForm
              .into[Person]
              .withFieldComputedF(_.age, _.age.parseInt)
              .withFieldComputedF(_.height, _.height.parseDouble)
              .transform ==> Some(Person("John", 10, 140))
          }

          "either" - {
            okForm
              .intoF[Either[List[String], +*], Person]
              .withFieldConst(_.name, "Joe")
              .withFieldComputedF(_.height, _.height.parseDouble.toEither("bad height"))
              .withFieldComputedF(_.age, _.age.parseInt.toEither("bad age"))
              .transform ==> Right(Person("Joe", 10, 140))
          }
        }

        "3-arg" - {

          "option" - {
            okForm
              .into[Person]
              .withFieldComputedF(_.name, pf => if (pf.name.isEmpty) None else Some(pf.name.toUpperCase()))
              .withFieldComputedF(_.age, _.age.parseInt)
              .withFieldComputedF(_.height, _.height.parseDouble)
              .transform ==> Some(Person("JOHN", 10, 140))
          }

          "either" - {
            okForm
              .intoF[Either[List[String], +*], Person]
              .withFieldComputedF(
                _.name,
                pf =>
                  if (pf.name.isEmpty) Left(List("empty name"))
                  else Right(pf.name.toUpperCase())
              )
              .withFieldComputedF(_.age, _.age.parseInt.toEither("bad age"))
              .withFieldComputedF(_.height, _.height.parseDouble.toEither("bad height"))
              .transform ==> Right(Person("JOHN", 10, 140))
          }
        }
      }

      "failure with error handling" - {
        val badForm = PersonForm("", "foo", "bar")

        "option" - {
          badForm
            .intoF[Option, Person]
            .withFieldComputedF(_.age, _.age.parseInt)
            .withFieldComputedF(_.height, _.age.parseDouble)
            .transform ==> None
        }

        "either" - {
          badForm
            .into[Person]
            .withFieldComputedF[Either[List[String], +*], String, String](
              _.name,
              pf =>
                if (pf.name.isEmpty) Left(List("empty name"))
                else Right(pf.name.toUpperCase())
            )
            .withFieldComputedF(_.age, _.age.parseInt.toEither("bad age"))
            .withFieldComputedF(_.height, _.age.parseDouble.toEither("bad double"))
            .transform ==> Left(List("empty name", "bad age", "bad double"))
        }
      }
    }

    "recursive transform with nested validation" - {

      implicit val personTransformerOpt: TransformerF[Option, PersonForm, Person] =
        Transformer
          .define[PersonForm, Person]
          .withFieldComputedF(_.age, _.age.parseInt)
          .withFieldComputedF(_.height, _.height.parseDouble)
          .buildTransformer

      implicit val personTransformerEithers: TransformerF[Either[List[String], +*], PersonForm, Person] =
        Transformer
          .defineF[Either[List[String], +*], PersonForm, Person]
          .withFieldComputedF(_.age, _.age.parseInt.toEither("bad age"))
          .withFieldComputedF(_.height, _.height.parseDouble.toEither("bad height"))
          .buildTransformer

      "success" - {

        val okTripForm = TripForm("100", List(PersonForm("John", "10", "140"), PersonForm("Caroline", "12", "155")))

        "option" - {

          okTripForm
            .into[Trip]
            .withFieldComputedF(_.id, _.tripId.parseInt)
            .transform ==> Some(Trip(100, Vector(Person("John", 10, 140), Person("Caroline", 12, 155))))
        }

        "either" - {
          okTripForm
            .intoF[Either[List[String], +*], Trip]
            .withFieldComputedF(_.id, tf => tf.tripId.parseInt.toEither("bad id"))
            .transform ==> Right(Trip(100, Vector(Person("John", 10, 140), Person("Caroline", 12, 155))))
        }
      }

      "failure with error handling" - {

        val badTripForm = TripForm("100", List(PersonForm("John", "10", "foo"), PersonForm("Caroline", "bar", "155")))

        "option" - {
          badTripForm
            .into[Trip]
            .withFieldComputedF(_.id, _.tripId.parseInt)
            .transform ==> None
        }

        "either" - {
          badTripForm
            .intoF[Either[List[String], +*], Trip]
            .withFieldComputedF(_.id, tf => tf.tripId.parseInt.toEither("bad id"))
            .transform ==> Left(List("bad height", "bad age"))
        }
      }
    }

    "wrapped subtype transformation" - {

      class Foo(val x: Int)
      case class Bar(override val x: Int) extends Foo(x)

      val optFoo: Option[Foo] = Bar(100).transformIntoF[Option, Foo]
      optFoo.get.x ==> 100

      val eitherFoo: Either[Vector[String], Foo] = Bar(200).transformIntoF[Either[Vector[String], +*], Foo]
      eitherFoo.map(_.x) ==> Right(200)
    }

    "wrapped value classes" - {

      "from value class" - {
        addressbook.Email("abc@def.com").transformIntoF[Option, String] ==> Some("abc@def.com")
      }

      "to value class" - {
        "abc@def.com".transformIntoF[Option, addressbook.Email] ==> Some(addressbook.Email("abc@def.com"))
      }
    }

    "wrapped options" - {

      "pure inner transformer" - {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {
          Option(123).transformIntoF[Option, Option[String]] ==> Some(Some("123"))
          Option.empty[Int].transformIntoF[Option, Option[String]] ==> Some(None)
        }

        "F = Either[List[String], +*]]" - {
          Option(123).transformIntoF[Either[List[String], +*], Option[String]] ==> Right(Some("123"))
          Option.empty[Int].transformIntoF[Either[List[String], +*], Option[String]] ==> Right(None)
        }
      }

      "wrapped inner transformer" - {

        "F = Option" - {
          implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          Option("123").transformIntoF[Option, Option[Int]] ==> Some(Some(123))
          Option("abc").transformIntoF[Option, Option[Int]] ==> None
          Option.empty[String].transformIntoF[Option, Option[Int]] ==> Some(None)
        }

        "F = Either[List[String], +*]]" - {
          implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
            _.parseInt.toEither("bad int")

          Option("123").transformIntoF[Either[List[String], +*], Option[Int]] ==> Right(Some(123))
          Option("abc").transformIntoF[Either[List[String], +*], Option[Int]] ==> Left(List("bad int"))
          Option.empty[String].transformIntoF[Either[List[String], +*], Option[Int]] ==> Right(None)
        }
      }
    }

    "wrapped T to Option[T]" - {

      "pure inner transformer" - {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {
          10.transformIntoF[Option, Option[String]] ==> Some(Some("10"))
          (null: String).transformIntoF[Option, Option[String]] ==> Some(None)
        }

        "F = Either[List[String], +*]]" - {
          10.transformIntoF[Either[List[String], +*], Option[String]] ==> Right(Some("10"))
          (null: String).transformIntoF[Either[List[String], +*], Option[String]] ==> Right(None)
        }
      }

      "wrapped inner transformer" - {

        "F = Option" - {
          implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          "10".transformIntoF[Option, Option[Int]] ==> Some(Some(10))
          (null: String).transformIntoF[Option, Option[Int]] ==> Some(None)
          "x".transformIntoF[Option, Option[Int]] ==> None
        }

        "F = Either[List[String], +*]]" - {
          implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
            _.parseInt.toEither("bad int")

          "10".transformIntoF[Either[List[String], +*], Option[Int]] ==> Right(Some(10))
          (null: String).transformIntoF[Either[List[String], +*], Option[Int]] ==> Right(None)
          "x".transformIntoF[Either[List[String], +*], Option[Int]] ==> Left(List("bad int"))
        }
      }
    }

    "wrapped .enableUnsafeOption" - {

      "pure inner transformer" - {
        implicit val intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {

          Option(10).intoF[Option, String].enableUnsafeOption.transform ==> Some("10")
          intercept[NoSuchElementException] {
            Option.empty[Int].intoF[Option, String].enableUnsafeOption.transform
          }
        }

        "F = Either[List[String], +*]]" - {

          Option(10).intoF[Either[List[String], +*], String].enableUnsafeOption.transform ==> Right("10")
          intercept[NoSuchElementException] {
            Option.empty[Int].intoF[Either[List[String], +*], String].enableUnsafeOption.transform
          }
        }
      }

      "wrapped inner transformer" - {
        "F = Option" - {
          implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          Option("10").intoF[Option, Int].enableUnsafeOption.transform ==> Some(10)
          Option("x").intoF[Option, Int].enableUnsafeOption.transform ==> None
          intercept[NoSuchElementException] {
            Option.empty[String].intoF[Option, Int].enableUnsafeOption.transform
          }
        }

        "F = Either[List[String], +*]]" - {
          implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
            _.parseInt.toEither("bad int")

          Option("10").intoF[Either[List[String], +*], Int].enableUnsafeOption.transform ==> Right(10)
          Option("x").intoF[Either[List[String], +*], Int].enableUnsafeOption.transform ==> Left(List("bad int"))
          intercept[NoSuchElementException] {
            Option.empty[String].intoF[Either[List[String], +*], Int].enableUnsafeOption.transform
          }
        }
      }
    }

    "wrapped iterables or arrays" - {

      "pure inner transformer" - {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {
          List(123, 456).transformIntoF[Option, List[String]] ==> Some(List("123", "456"))
          Vector(123, 456).transformIntoF[Option, Queue[String]] ==> Some(Queue("123", "456"))
          Array.empty[Int].transformIntoF[Option, Seq[String]] ==> Some(Seq.empty[String])
        }

        "F = Either[List[String], +*]]" - {
          List(123, 456).transformIntoF[Either[List[String], +*], List[String]] ==> Right(List("123", "456"))
          Vector(123, 456).transformIntoF[Either[List[String], +*], Queue[String]] ==> Right(Queue("123", "456"))
          Array.empty[Int].transformIntoF[Either[List[String], +*], Seq[String]] ==> Right(Seq.empty[String])
        }
      }

      "wrapped inner transformer" - {

        "F = Option" - {
          implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          List("123", "456").transformIntoF[Option, List[Int]] ==> Some(List(123, 456))
          Vector("123", "456").transformIntoF[Option, Queue[Int]] ==> Some(Queue(123, 456))
          Array.empty[String].transformIntoF[Option, Seq[Int]] ==> Some(Seq.empty[Int])
          Set("123", "456").transformIntoF[Option, Array[Int]].get.sorted ==> Array(123, 456)

          List("abc", "456").transformIntoF[Option, List[Int]] ==> None
          Vector("123", "def").transformIntoF[Option, Queue[Int]] ==> None
          Array("abc", "def").transformIntoF[Option, Seq[Int]] ==> None
          Set("123", "xyz").transformIntoF[Option, Array[Int]] ==> None
        }

        "F = Either[List[String], +*]]" - {
          implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
            _.parseInt.toEither("bad int")

          List("123", "456").transformIntoF[Either[List[String], +*], List[Int]] ==> Right(List(123, 456))
          Vector("123", "456").transformIntoF[Either[List[String], +*], Queue[Int]] ==> Right(Queue(123, 456))
          Array.empty[String].transformIntoF[Either[List[String], +*], Seq[Int]] ==> Right(Seq.empty[Int])
          Set("123", "456").transformIntoF[Either[List[String], +*], Array[Int]].toOption.get.sorted ==> Array(123, 456)

          List("abc", "456").transformIntoF[Either[List[String], +*], List[Int]] ==> Left(List("bad int"))
          Vector("123", "def").transformIntoF[Either[List[String], +*], Queue[Int]] ==> Left(List("bad int"))
          Array("abc", "def").transformIntoF[Either[List[String], +*], Seq[Int]] ==> Left(List("bad int", "bad int"))
          Set("123", "xyz").transformIntoF[Either[List[String], +*], Array[Int]] ==> Left(List("bad int"))
        }
      }
    }

    "wrapped maps" - {
      "pure inner transformer" - {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {
          Map(1 -> 10, 2 -> 20).transformIntoF[Option, Map[String, String]] ==> Some(Map("1" -> "10", "2" -> "20"))
          Map(1 -> 10, 2 -> 20).transformIntoF[Option, Map[String, Int]] ==> Some(Map("1" -> 10, "2" -> 20))
          Seq(1 -> 10, 2 -> 20).transformIntoF[Option, Map[String, String]] ==> Some(Map("1" -> "10", "2" -> "20"))
          ArrayBuffer(1 -> 10, 2 -> 20).transformIntoF[Option, Map[Int, String]] ==> Some(Map(1 -> "10", 2 -> "20"))
          Map(1 -> 10, 2 -> 20).transformIntoF[Option, List[(String, String)]] ==> Some(List("1" -> "10", "2" -> "20"))
          Map(1 -> 10, 2 -> 20).transformIntoF[Option, Vector[(String, Int)]] ==> Some(Vector("1" -> 10, "2" -> 20))
          Array(1 -> 10, 2 -> 20).transformIntoF[Option, Map[String, String]] ==> Some(Map("1" -> "10", "2" -> "20"))
          Array(1 -> 10, 2 -> 20).transformIntoF[Option, Map[Int, String]] ==> Some(Map(1 -> "10", 2 -> "20"))
          Map(1 -> 10, 2 -> 20).transformIntoF[Option, Array[(String, String)]].get ==> Array("1" -> "10", "2" -> "20")
          Map(1 -> 10, 2 -> 20).transformIntoF[Option, Array[(String, Int)]].get ==> Array("1" -> 10, "2" -> 20)
        }

        "F = Either[List[String], +*]]" - {
          Map(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Map[String, String]] ==>
            Right(Map("1" -> "10", "2" -> "20"))
          Map(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Map[String, Int]] ==>
            Right(Map("1" -> 10, "2" -> 20))
          Seq(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Map[String, String]] ==>
            Right(Map("1" -> "10", "2" -> "20"))
          ArrayBuffer(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Map[Int, String]] ==>
            Right(Map(1 -> "10", 2 -> "20"))
          Map(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], List[(String, String)]] ==>
            Right(List("1" -> "10", "2" -> "20"))
          Map(1 -> 10, 2 -> 20).transformIntoF[Either[List[String], +*], Vector[(String, Int)]] ==>
            Right(Vector("1" -> 10, "2" -> 20))
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

      "wrapped inner transformer" - {

        "F = Option" - {
          implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          Map("1" -> "10", "2" -> "20").transformIntoF[Option, Map[Int, Int]] ==> Some(Map(1 -> 10, 2 -> 20))
          Map("1" -> "10", "2" -> "20").transformIntoF[Option, Map[Int, String]] ==> Some(Map(1 -> "10", 2 -> "20"))
          Seq("1" -> "10", "2" -> "20").transformIntoF[Option, Map[Int, Int]] ==> Some(Map(1 -> 10, 2 -> 20))
          ArrayBuffer("1" -> "10", "2" -> "20").transformIntoF[Option, Map[String, Int]] ==>
            Some(Map("1" -> 10, "2" -> 20))
          Map("1" -> "10", "2" -> "20").transformIntoF[Option, List[(Int, Int)]] ==> Some(List(1 -> 10, 2 -> 20))
          Map("1" -> "10", "2" -> "20").transformIntoF[Option, Vector[(Int, String)]] ==>
            Some(Vector(1 -> "10", 2 -> "20"))
          Array("1" -> "10", "2" -> "20").transformIntoF[Option, Map[Int, Int]] ==> Some(Map(1 -> 10, 2 -> 20))
          Array("1" -> "10", "2" -> "20").transformIntoF[Option, Map[String, Int]] ==> Some(Map("1" -> 10, "2" -> 20))
          Map("1" -> "10", "2" -> "20").transformIntoF[Option, Array[(Int, Int)]].get ==> Array(1 -> 10, 2 -> 20)
          Map("1" -> "10", "2" -> "20").transformIntoF[Option, Array[(Int, String)]].get ==> Array(1 -> "10", 2 -> "20")

          Map("1" -> "x", "y" -> "20").transformIntoF[Option, Map[Int, Int]] ==> None
          Map("x" -> "10", "2" -> "20").transformIntoF[Option, Map[Int, String]] ==> None
          Seq("1" -> "10", "2" -> "x").transformIntoF[Option, Map[Int, Int]] ==> None
          ArrayBuffer("1" -> "x", "2" -> "y").transformIntoF[Option, Map[String, Int]] ==> None
          Map("x" -> "10", "y" -> "z").transformIntoF[Option, List[(Int, Int)]] ==> None
          Map("1" -> "10", "x" -> "20").transformIntoF[Option, Vector[(Int, String)]] ==> None
          Array("x" -> "y", "z" -> "v").transformIntoF[Option, Map[Int, Int]] ==> None
          Array("1" -> "x", "2" -> "y").transformIntoF[Option, Map[String, Int]] ==> None
          Map("1" -> "10", "x" -> "20").transformIntoF[Option, Array[(Int, Int)]] ==> None
          Map("x" -> "10", "y" -> "20").transformIntoF[Option, Array[(Int, String)]] ==> None
        }

        "F = Either[List[String], +*]]" - {
          implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
            _.parseInt.toEither("bad int")

          Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[Int, Int]] ==>
            Right(Map(1 -> 10, 2 -> 20))
          Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[Int, String]] ==>
            Right(Map(1 -> "10", 2 -> "20"))
          Seq("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[Int, Int]] ==>
            Right(Map(1 -> 10, 2 -> 20))
          ArrayBuffer("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[String, Int]] ==>
            Right(Map("1" -> 10, "2" -> 20))
          Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], List[(Int, Int)]] ==>
            Right(List(1 -> 10, 2 -> 20))
          Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Vector[(Int, String)]] ==>
            Right(Vector(1 -> "10", 2 -> "20"))
          Array("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[Int, Int]] ==>
            Right(Map(1 -> 10, 2 -> 20))
          Array("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[String, Int]] ==>
            Right(Map("1" -> 10, "2" -> 20))
          Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Array[(Int, Int)]].toOption.get ==>
            Array(1 -> 10, 2 -> 20)
          Map("1" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Array[(Int, String)]].toOption.get ==>
            Array(1 -> "10", 2 -> "20")

          Map("1" -> "x", "y" -> "20").transformIntoF[Either[List[String], +*], Map[Int, Int]] ==>
            Left(List("bad int", "bad int"))
          Map("x" -> "10", "2" -> "20").transformIntoF[Either[List[String], +*], Map[Int, String]] ==>
            Left(List("bad int"))
          Seq("1" -> "10", "2" -> "x").transformIntoF[Either[List[String], +*], Map[Int, Int]] ==>
            Left(List("bad int"))
          ArrayBuffer("1" -> "x", "2" -> "y").transformIntoF[Either[List[String], +*], Map[String, Int]] ==>
            Left(List("bad int", "bad int"))
          Map("x" -> "10", "y" -> "z").transformIntoF[Either[List[String], +*], ArrayBuffer[(Int, Int)]] ==>
            Left(List("bad int", "bad int", "bad int"))
          Map("1" -> "10", "x" -> "20").transformIntoF[Either[List[String], +*], Vector[(Int, String)]] ==>
            Left(List("bad int"))
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
    }

    "wrapped eithers" - {

      "pure inner transformer" - {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {

          (Left(1): Either[Int, Int]).transformIntoF[Option, Either[String, String]] ==> Some(Left("1"))
          (Right(1): Either[Int, Int]).transformIntoF[Option, Either[String, String]] ==> Some(Right("1"))
          Left(1).transformIntoF[Option, Either[String, String]] ==> Some(Left("1"))
          Right(1).transformIntoF[Option, Either[String, String]] ==> Some(Right("1"))
          Left(1).transformIntoF[Option, Left[String, String]] ==> Some(Left("1"))
          Right(1).transformIntoF[Option, Right[String, String]] ==> Some(Right("1"))
        }

        "F = Either[List[String], +*]]" - {

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

      "wrapped inner transformer" - {

        "F = Option" - {
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

        "F = Either[List[String], +*]]" - {
          implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
            _.parseInt.toEither("bad int")

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

      "mixed inner transformer" - {

        "F = Option" - {

          implicit val intPrinter: Transformer[Int, String] = _.toString
          implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          (Left("1"): Either[String, Int]).transformIntoF[Option, Either[Int, String]] ==> Some(Left(1))
          (Left("x"): Either[String, Int]).transformIntoF[Option, Either[Int, String]] ==> None
          (Right(100): Either[String, Int]).transformIntoF[Option, Either[Int, String]] ==> Some(Right("100"))

          (Left(100): Either[Int, String]).transformIntoF[Option, Either[String, Int]] ==> Some(Left("100"))
          (Right("1"): Either[Int, String]).transformIntoF[Option, Either[String, Int]] ==> Some(Right(1))
          (Right("x"): Either[Int, String]).transformIntoF[Option, Either[String, Int]] ==> None
        }

        "F = Either[List[String], +*]]" - {

          implicit val intPrinter: Transformer[Int, String] = _.toString
          implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
            _.parseInt.toEither("bad int")

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
    }

    "wrapped sealed families" - {
      import numbers._

      "pure inner transformer" - {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {
          import ScalesTransformer.shortToLongPureInner

          (short.Zero: short.NumScale[Int, Nothing])
            .transformIntoF[Option, long.NumScale[String]] ==> Some(long.Zero)
          (short.Million(4): short.NumScale[Int, Nothing])
            .transformIntoF[Option, long.NumScale[String]] ==> Some(long.Million("4"))
          (short.Billion(2): short.NumScale[Int, Nothing])
            .transformIntoF[Option, long.NumScale[String]] ==> Some(long.Milliard("2"))
          (short.Trillion(100): short.NumScale[Int, Nothing])
            .transformIntoF[Option, long.NumScale[String]] ==> Some(long.Billion("100"))
        }

        "F = Either[List[String], +*]]" - {
          import ScalesTransformer.shortToLongPureInner

          (short.Zero: short.NumScale[Int, Nothing])
            .transformIntoF[Either[List[String], +*], long.NumScale[String]] ==> Right(long.Zero)
          (short.Million(4): short.NumScale[Int, Nothing])
            .transformIntoF[Either[List[String], +*], long.NumScale[String]] ==> Right(long.Million("4"))
          (short.Billion(2): short.NumScale[Int, Nothing])
            .transformIntoF[Either[List[String], +*], long.NumScale[String]] ==> Right(long.Milliard("2"))
          (short.Trillion(100): short.NumScale[Int, Nothing])
            .transformIntoF[Either[List[String], +*], long.NumScale[String]] ==> Right(long.Billion("100"))
        }
      }

      "wrapped inner transformer" - {

        "F = Option" - {
          implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          import ScalesTransformer.shortToLongWrappedInner

          (short.Zero: short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> Some(long.Zero)
          (short.Million("4"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> Some(long.Million(4))
          (short.Billion("2"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> Some(long.Milliard(2))
          (short.Trillion("100"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> Some(long.Billion(100))

          (short.Million("x"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> None
          (short.Billion("x"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> None
          (short.Trillion("x"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> None
        }

        "F = Either[List[String], +*]]" - {
          implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
            _.parseInt.toEither("bad int")

          import ScalesTransformer.shortToLongWrappedInner

          (short.Zero: short.NumScale[String, Nothing])
            .transformIntoF[Either[List[String], +*], long.NumScale[Int]] ==> Right(long.Zero)
          (short.Million("4"): short.NumScale[String, Nothing])
            .transformIntoF[Either[List[String], +*], long.NumScale[Int]] ==> Right(long.Million(4))
          (short.Billion("2"): short.NumScale[String, Nothing])
            .transformIntoF[Either[List[String], +*], long.NumScale[Int]] ==> Right(long.Milliard(2))
          (short.Trillion("100"): short.NumScale[String, Nothing])
            .transformIntoF[Either[List[String], +*], long.NumScale[Int]] ==> Right(long.Billion(100))

          (short.Million("x"): short.NumScale[String, Nothing])
            .transformIntoF[Either[List[String], +*], long.NumScale[Int]] ==> Left(List("bad int"))
          (short.Billion("x"): short.NumScale[String, Nothing])
            .transformIntoF[Either[List[String], +*], long.NumScale[Int]] ==> Left(List("bad int"))
          (short.Trillion("x"): short.NumScale[String, Nothing])
            .transformIntoF[Either[List[String], +*], long.NumScale[Int]] ==> Left(List("bad int"))
        }
      }
    }

    "implicit conflict resolution" - {

      final case class InnerIn(value: String)
      final case class InnerOut(value: String)

      final case class OuterIn(value: InnerIn)
      final case class OuterOut(value: InnerOut)

      implicit val pureTransformer: Transformer[InnerIn, InnerOut] =
        in => InnerOut(s"pure: ${in.value}")

      implicit val liftedTransformer: TransformerF[Either[List[String], +*], InnerIn, InnerOut] =
        in => Right(InnerOut(s"lifted: ${in.value}"))

      "fail compilation if there is unresolved conflict" - {

        compileError("""
          type EitherListStr[+X] = Either[List[String], X]
          OuterIn(InnerIn("test")).transformIntoF[EitherListStr, OuterOut]
          """)
          .check(
            "",
            "Ambiguous implicits while resolving Chimney recursive transformation",
            "Please eliminate ambiguity from implicit scope or use withFieldComputed/withFieldComputedF to decide which one should be used"
          )
      }

      "resolve conflict explicitly using .withFieldComputed" - {
        OuterIn(InnerIn("test"))
          .intoF[Either[List[String], +*], OuterOut]
          .withFieldComputed(_.value, v => pureTransformer.transform(v.value))
          .transform ==> Right(OuterOut(InnerOut("pure: test")))
      }

      "resolve conflict explicitly using .withFieldComputedF" - {
        OuterIn(InnerIn("test"))
          .intoF[Either[List[String], +*], OuterOut]
          .withFieldComputedF(_.value, v => liftedTransformer.transform(v.value))
          .transform ==> Right(OuterOut(InnerOut("lifted: test")))
      }

      "resolve conflict explicitly prioritizing: last wins" - {
        OuterIn(InnerIn("test"))
          .intoF[Either[List[String], +*], OuterOut]
          .withFieldComputed(_.value, v => pureTransformer.transform(v.value))
          .withFieldComputedF(_.value, v => liftedTransformer.transform(v.value))
          .transform ==> Right(OuterOut(InnerOut("lifted: test")))

        OuterIn(InnerIn("test"))
          .intoF[Either[List[String], +*], OuterOut]
          .withFieldComputedF(_.value, v => liftedTransformer.transform(v.value))
          .withFieldComputed(_.value, v => pureTransformer.transform(v.value))
          .transform ==> Right(OuterOut(InnerOut("pure: test")))
      }
    }

    "safe option unwrapping" - {
      type F[+A] = Either[List[String], A]

      implicit val intParserEither: TransformerF[F, String, Int] =
        _.parseInt.toEither("bad int")

      implicit def optionUnwrapping[A, B](implicit underlying: TransformerF[F, A, B]): TransformerF[F, Option[A], B] = {
        case Some(value) => underlying.transform(value)
        case None        => Left(List("Expected value, got none"))
      }

      // Raw domain
      case class RawData(id: Option[String], inner: Option[RawInner])

      case class RawInner(id: Option[Int], str: Option[String])

      // Domain
      case class Data(id: Int, inner: Inner)

      case class Inner(id: Int, str: String)

      RawData(Some("1"), Some(RawInner(Some(2), Some("str")))).transformIntoF[F, Data] ==> Right(
        Data(1, Inner(2, "str"))
      )

      RawData(Some("a"), Some(RawInner(None, None))).transformIntoF[F, Data] ==> Left(
        List("bad int", "Expected value, got none", "Expected value, got none")
      )
    }

    "support scoped transformer configuration passed implicitly" - {

      class Source { def field1: Int = 100 }
      case class Target(field1: Int = 200, field2: Option[String] = Some("foo"))

      implicit val transformerConfiguration = {
        TransformerConfiguration.default.enableOptionDefaultsToNone.enableMethodAccessors.disableDefaultValues
      }

      "scoped config only" - {

        (new Source).transformIntoF[Option, Target] ==> Some(Target(100, None))
        (new Source).intoF[Option, Target].transform ==> Some(Target(100, None))
      }

      "scoped config overridden by instance flag" - {

        (new Source)
          .intoF[Option, Target]
          .disableMethodAccessors
          .enableDefaultValues
          .transform ==> Some(Target(200, Some("foo")))

        (new Source)
          .intoF[Option, Target]
          .enableDefaultValues
          .transform ==> Some(Target(100, Some("foo")))

        (new Source)
          .intoF[Option, Target]
          .disableOptionDefaultsToNone
          .withFieldConst(_.field2, Some("abc"))
          .transform ==> Some(Target(100, Some("abc")))

      }

      "compile error when optionDefaultsToNone were disabled locally" - {

        compileError("""
          (new Source).intoF[Option, Target].disableOptionDefaultsToNone.transform
        """)
          .check("", "Chimney can't derive transformation from Source to Target")
      }
    }
  }
}
