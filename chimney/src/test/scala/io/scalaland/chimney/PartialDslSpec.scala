package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.examples.trip._
import io.scalaland.chimney.utils.EitherUtils._
import io.scalaland.chimney.utils.OptionUtils._
import utest._

import scala.annotation.unused
import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer

object PartialDslSpec extends TestSuite {

  val tests = Tests {

    "transform always succeeds" - {

      "without modifiers" - {
        val result = Person("John", 10, 140).transformIntoPartial[User]
        val expected = User("John", 10, 140)

        result.asOption ==> Some(expected)
        result.asEither ==> Right(expected)
        result.asErrorPathMessagesStrings ==> Seq.empty
      }

      "field const override" - {
        val result = Person("John", 10, 140)
          .intoPartial[User]
          .withFieldConst(_.age, 20)
          .transform
        val expected = User("John", 20, 140)

        result.asOption ==> Some(expected)
        result.asEither ==> Right(expected)
        result.asErrorPathMessagesStrings ==> Seq.empty
      }

      "field partial const override" - {
        val result = Person("John", 10, 140)
          .intoPartial[User]
          .withFieldConstPartial(_.age, PartialTransformer.Result.fromValue(20))
          .transform
        val expected = User("John", 20, 140)

        result.asOption ==> Some(expected)
        result.asEither ==> Right(expected)
        result.asErrorPathMessagesStrings ==> Seq.empty
      }

      "field compute" - {
        val result = Person("John", 10, 140)
          .intoPartial[User]
          .withFieldComputed(_.age, _.age * 3)
          .transform
        val expected = User("John", 30, 140)

        result.asOption ==> Some(expected)
        result.asEither ==> Right(expected)
        result.asErrorPathMessagesStrings ==> Seq.empty
      }

      "field compute partial" - {
        val result = Person("John", 10, 140)
          .intoPartial[User]
          .withFieldComputedPartial(_.age, p => PartialTransformer.Result.fromValue(p.age * 3))
          .transform
        val expected = User("John", 30, 140)

        result.asOption ==> Some(expected)
        result.asEither ==> Right(expected)
        result.asErrorPathMessagesStrings ==> Seq.empty
      }
    }

    "transform always fails" - {

      "empty value" - {
        val result = Person("John", 10, 140)
          .intoPartial[User]
          .withFieldConstPartial(_.height, PartialTransformer.Result.fromEmpty)
          .transform

        result.asOption ==> None
        result.asEither ==> Left(
          PartialTransformer.Result.Errors
            .single(PartialTransformer.Error.ofEmptyValue)
            .wrapErrorPaths(PartialTransformer.ErrorPath.Accessor("height", _))
        )
        result.asErrorPathMessagesStrings ==> Iterable(
          "height" -> "empty value"
        )
      }

      "not defined at" - {
        val person = Person("John", 10, 140)
        val result = person
          .intoPartial[User]
          .withFieldComputedPartial(_.height, PartialTransformer.Result.fromPartialFunction {
            case Person(_, age, _) if age > 18 => 2.0 * age
          })
          .transform

        result.asOption ==> None
        result.asEither ==> Left(
          PartialTransformer.Result.Errors
            .single(PartialTransformer.Error.ofNotDefinedAt(person))
            .wrapErrorPaths(PartialTransformer.ErrorPath.Accessor("height", _))
        )
        result.asErrorPathMessagesStrings ==> Iterable(
          "height" -> s"not defined at $person"
        )
      }

      "custom string errors" - {
        val result = Person("John", 10, 140)
          .intoPartial[User]
          .withFieldConstPartial(_.height, PartialTransformer.Result.fromErrorStrings(Seq("abc", "def")))
          .transform

        result.asOption ==> None
        result.asEither == Left(
          PartialTransformer.Result
            .Errors(
              Iterable(
                PartialTransformer.Error.ofString("abc"),
                PartialTransformer.Error.ofString("def")
              )
            )
            .wrapErrorPaths(PartialTransformer.ErrorPath.Accessor("height", _))
        )
        result.asErrorPathMessagesStrings ==> Iterable(
          "height" -> "abc",
          "height" -> "def"
        )
      }

      "throwable error" - {
        case object MyException extends Exception("my exception")
        val result = Person("John", 10, 140)
          .intoPartial[User]
          .withFieldConstPartial(_.height, PartialTransformer.Result.fromErrorThrowable(MyException))
          .transform

        result.asOption ==> None
        result.asEither == Left(
          PartialTransformer.Result.Errors
            .single(
              PartialTransformer.Error.ofThrowable(MyException)
            )
            .wrapErrorPaths(PartialTransformer.ErrorPath.Accessor("height", _))
        )
        result.asErrorPathMessagesStrings ==> Iterable(
          "height" -> "my exception"
        )
      }
    }

    "partial transform validation" - {

      "success" - {
        val okForm = PersonForm("John", "10", "140")
        val expected = Person("JOHN", 10, 140)

        val result = okForm
          .intoPartial[Person]
          .withFieldComputedPartial(
            _.name,
            pf =>
              if (pf.name.isEmpty) PartialTransformer.Result.fromEmpty
              else PartialTransformer.Result.fromValue(pf.name.toUpperCase())
          )
          .withFieldComputed(_.age, _.age.toInt) // must catch exceptions
          .withFieldComputedPartial(
            _.height,
            pf => PartialTransformer.Result.fromOption(pf.height.parseDouble)
          )
          .transform

        result.asOption ==> Some(expected)
        result.asEither ==> Right(expected)
        result.asErrorPathMessagesStrings ==> Iterable.empty
      }

      "failure with error handling" - {
        val invalidForm = PersonForm("", "foo", "bar")

        val result = invalidForm
          .intoPartial[Person]
          .withFieldComputedPartial(
            _.name,
            pf =>
              if (pf.name.isEmpty) PartialTransformer.Result.fromEmpty
              else PartialTransformer.Result.fromValue(pf.name.toUpperCase())
          )
          .withFieldComputed(_.age, _.age.toInt) // must catch exceptions
          .withFieldComputedPartial(
            _.height,
            pf => PartialTransformer.Result.fromOption(pf.height.parseDouble)
          )
          .transform

        result.asOption ==> None
        result.asErrorPathMessagesStrings ==> Iterable(
          "name" -> "empty value",
          "age" -> "For input string: \"foo\"",
          "height" -> "empty value"
        )
      }
    }

    "recursive partial transform with nested validation" - {

      implicit val personPartialTransformer: PartialTransformer[PersonForm, Person] =
        Transformer
          .definePartial[PersonForm, Person]
          .withFieldComputedPartial(_.age, _.age.parseInt.toPartialTransformerResultOrString("bad age value"))
          .withFieldComputedPartial(
            _.height,
            _.height.parseDouble.toPartialTransformerResultOrString("bad height value")
          )
          .buildTransformer

      "success" - {

        val okTripForm = TripForm("100", List(PersonForm("John", "10", "140"), PersonForm("Caroline", "12", "155")))

        val result = okTripForm
          .intoPartial[Trip]
          .withFieldComputedPartial(_.id, _.tripId.parseInt.toPartialTransformerResultOrString("bad trip id"))
          .transform

        result.asOption ==> Some(Trip(100, Vector(Person("John", 10, 140), Person("Caroline", 12, 155))))
      }

      "failure with error handling" - {

        val badTripForm =
          TripForm("100xyz", List(PersonForm("John", "10", "foo"), PersonForm("Caroline", "bar", "155")))

        val result = badTripForm
          .intoPartial[Trip]
          .withFieldComputedPartial(_.id, _.tripId.parseInt.toPartialTransformerResultOrString("bad trip id"))
          .transform

        result.asOption ==> None
        result.asEither ==> Left(
          PartialTransformer.Result.Errors(
            Iterable(
              PartialTransformer.Error
                .ofString("bad trip id")
                .wrapErrorPath(PartialTransformer.ErrorPath.Accessor("id", _)),
              PartialTransformer.Error
                .ofString("bad height value")
                .wrapErrorPath(PartialTransformer.ErrorPath.Accessor("height", _))
                .wrapErrorPath(PartialTransformer.ErrorPath.Index(0, _))
                .wrapErrorPath(PartialTransformer.ErrorPath.Accessor("people", _)),
              PartialTransformer.Error
                .ofString("bad age value")
                .wrapErrorPath(PartialTransformer.ErrorPath.Accessor("age", _))
                .wrapErrorPath(PartialTransformer.ErrorPath.Index(1, _))
                .wrapErrorPath(PartialTransformer.ErrorPath.Accessor("people", _))
            )
          )
        )
        result.asErrorPathMessagesStrings ==> Iterable(
          "id" -> "bad trip id",
          "people(0).height" -> "bad height value",
          "people(1).age" -> "bad age value"
        )
      }
    }

    "partial subtype transform" - {
      class Foo(val x: Int)
      case class Bar(override val x: Int) extends Foo(x)

      val result = Bar(100).transformIntoPartial[Foo]

      result.asOption.map(_.x) ==> Some(100)
      result.asEither.map(_.x) ==> Right(100)
      result.asErrorPathMessagesStrings ==> Iterable.empty
    }

    "partial value class transform" - {

      "from value class" - {
        val result = addressbook.Email("abc@def.com").transformIntoPartial[String]

        result.asOption ==> Some("abc@def.com")
        result.asEither ==> Right("abc@def.com")
        result.asErrorPathMessagesStrings ==> Iterable.empty
      }

      "to value class" - {

        val result = "abc@def.com".transformIntoPartial[addressbook.Email]

        result.asOption ==> Some(addressbook.Email("abc@def.com"))
        result.asEither ==> Right(addressbook.Email("abc@def.com"))
        result.asErrorPathMessagesStrings ==> Iterable.empty
      }
    }

    "partial option transform" - {

      "total inner transform" - {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        "Option[T] to Option[U]" - {

          "optional value" - {
            val result = Option(123).transformIntoPartial[Option[String]]

            result.asOption ==> Some(Some("123"))
            result.asEither ==> Right(Some("123"))
            result.asErrorPathMessagesStrings ==> Iterable.empty
          }

          "empty option" - {
            val result = Option.empty[Int].transformIntoPartial[Option[String]]

            result.asOption ==> Some(None)
            result.asEither ==> Right(None)
            result.asErrorPathMessagesStrings ==> Iterable.empty
          }
        }

        "T to Option[U]" - {

          "value case" - {
            val result = 10.transformIntoPartial[Option[String]]

            result.asOption ==> Some(Some("10"))
            result.asEither ==> Right(Some("10"))
            result.asErrorPathMessagesStrings ==> Iterable.empty
          }

          "null case" - {
            implicit val integerPrinter: Transformer[Integer, String] = _.toString

            val result = (null: Integer).transformIntoPartial[Option[String]]

            result.asOption ==> Some(None)
            result.asEither ==> Right(None)
            result.asErrorPathMessagesStrings ==> Iterable.empty
          }
        }
      }

      "partial inner transform" - {

        implicit val intPartialParser: PartialTransformer[String, Int] =
          PartialTransformer(_.parseInt.toPartialTransformerResultOrString("bad int"))

        "Option[T] to Option[U]" - {
          "success case" - {
            val result = Option("123").transformIntoPartial[Option[Int]]

            result.asOption ==> Some(Some(123))
            result.asEither ==> Right(Some(123))
            result.asErrorPathMessagesStrings ==> Iterable.empty
          }

          "failure case" - {
            val result = Option("abc").transformIntoPartial[Option[Int]]

            result.asOption ==> None
            result.asEither ==> Left(
              PartialTransformer.Result.Errors.fromString("bad int")
            )
            result.asErrorPathMessagesStrings ==> Iterable(
              "" -> "bad int"
            )
          }

          "empty case" - {
            val result = Option.empty[String].transformIntoPartial[Option[Int]]

            result.asOption ==> Some(None)
            result.asEither ==> Right(None)
            result.asErrorPathMessagesStrings ==> Seq.empty
          }
        }

        "T to Option[U]" - {
          "success case" - {
            val result = "123".transformIntoPartial[Option[Int]]

            result.asOption ==> Some(Some(123))
            result.asEither ==> Right(Some(123))
            result.asErrorPathMessagesStrings ==> Iterable.empty
          }

          "failure case" - {
            val result = "abc".transformIntoPartial[Option[Int]]

            result.asOption ==> None
            result.asEither ==> Left(
              PartialTransformer.Result.Errors.fromString("bad int")
            )
            result.asErrorPathMessagesStrings ==> Iterable(
              "" -> "bad int"
            )
          }

          "null case" - {
            val result = (null: String).transformIntoPartial[Option[Int]]

            result.asOption ==> Some(None)
            result.asEither ==> Right(None)
            result.asErrorPathMessagesStrings ==> Iterable.empty
          }
        }
      }
    }

    "partial .enableUnsafeOption" - {

      "not supported for any case" - {

        @unused implicit val intPrinter: Transformer[Int, String] = _.toString

        @unused implicit val intPartialParser: PartialTransformer[String, Int] =
          PartialTransformer(_.parseInt.toPartialTransformerResultOrString("bad int"))

        compileError("Option(10).intoPartial[String].enableUnsafeOption.transform")
          .check("", "not supported")
        compileError("Option.empty[Int].intoPartial[String].enableUnsafeOption.transform")
          .check("", "not supported")
        compileError("""Option("x").intoPartial[Int].enableUnsafeOption.transform""")
          .check("", "not supported")
        compileError("""Option.empty[String].intoPartial[Int].enableUnsafeOption.transform""")
          .check("", "not supported")
      }
    }

    "safe option unwrapping" - {
      implicit val intParserEither: PartialTransformer[String, Int] =
        PartialTransformer(
          _.parseInt.toEitherList("bad int").toPartialTransformerResult
        )

      implicit def optionUnwrapping[A, B](
          implicit underlying: PartialTransformer[A, B]
      ): PartialTransformer[Option[A], B] = PartialTransformer {
        case Some(value) => underlying.transform(value)
        case None        => PartialTransformer.Result.fromErrorString("Expected a value, got none")
      }

      // Raw domain
      case class RawData(id: Option[String], inner: Option[RawInner])

      case class RawInner(id: Option[Int], str: Option[String])

      // Domain
      case class Data(id: Int, inner: Inner)

      case class Inner(id: Int, str: String)

      RawData(Some("1"), Some(RawInner(Some(2), Some("str"))))
        .transformIntoPartial[Data]
        .asEither ==> Right(
        Data(1, Inner(2, "str"))
      )

      RawData(Some("a"), Some(RawInner(None, None)))
        .transformIntoPartial[Data]
        .asErrorPathMessagesStrings ==> Iterable(
        "id" -> "bad int",
        "inner.id" -> "Expected a value, got none",
        "inner.str" -> "Expected a value, got none"
      )
    }

    "partial either transform" - {

      "total inner transform" - {
        implicit val intPrinter: Transformer[Int, String] = _.toString

        (Left(1): Either[Int, Int]).transformIntoPartial[Either[String, String]].asOption ==> Some(Left("1"))
        (Right(1): Either[Int, Int]).transformIntoPartial[Either[String, String]].asOption ==> Some(Right("1"))
        Left(1).transformIntoPartial[Either[String, String]].asOption ==> Some(Left("1"))
        Right(1).transformIntoPartial[Either[String, String]].asOption ==> Some(Right("1"))
        Left(1).transformIntoPartial[Left[String, String]].asOption ==> Some(Left("1"))
        Right(1).transformIntoPartial[Right[String, String]].asOption ==> Some(Right("1"))
      }

      "partial inner transform" - {
        implicit val intParserOpt: PartialTransformer[String, Int] =
          PartialTransformer(_.parseInt.toPartialTransformerResult)

        (Left("1"): Either[String, String]).transformIntoPartial[Either[Int, Int]].asOption ==> Some(Left(1))
        (Right("1"): Either[String, String]).transformIntoPartial[Either[Int, Int]].asOption ==> Some(Right(1))
        Left("1").transformIntoPartial[Either[Int, Int]].asOption ==> Some(Left(1))
        Right("1").transformIntoPartial[Either[Int, Int]].asOption ==> Some(Right(1))
        Left("1").transformIntoPartial[Left[Int, Int]].asOption ==> Some(Left(1))
        Right("1").transformIntoPartial[Right[Int, Int]].asOption ==> Some(Right(1))

        (Left("x"): Either[String, String]).transformIntoPartial[Either[Int, Int]].asOption ==> None
        (Right("x"): Either[String, String]).transformIntoPartial[Either[Int, Int]].asOption ==> None
        Left("x").transformIntoPartial[Either[Int, Int]].asOption ==> None
        Right("x").transformIntoPartial[Either[Int, Int]].asOption ==> None
        Left("x").transformIntoPartial[Left[Int, Int]].asOption ==> None
        Right("x").transformIntoPartial[Right[Int, Int]].asOption ==> None
      }
    }

    "partial collection transform" - {

      "total inner transform" - {
        implicit val intPrinter: Transformer[Int, String] = _.toString

        List(123, 456).transformIntoPartial[List[String]].asOption ==> Some(List("123", "456"))
        Vector(123, 456).transformIntoPartial[Queue[String]].asOption ==> Some(Queue("123", "456"))
        Array.empty[Int].transformIntoPartial[Seq[String]].asOption ==> Some(Seq.empty[String])
      }

      "partial inner transform" - {
        implicit val intParserOpt: PartialTransformer[String, Int] =
          PartialTransformer(_.parseInt.toPartialTransformerResult)

        List("123", "456").transformIntoPartial[List[Int]].asOption ==> Some(List(123, 456))
        Vector("123", "456").transformIntoPartial[Queue[Int]].asOption ==> Some(Queue(123, 456))
        Array.empty[String].transformIntoPartial[Seq[Int]].asOption ==> Some(Seq.empty[Int])
        Set("123", "456").transformIntoPartial[Array[Int]].asOption.get.sorted ==> Array(123, 456)

        List("abc", "456").transformIntoPartial[List[Int]].asOption ==> None
        Vector("123", "def").transformIntoPartial[Queue[Int]].asOption ==> None
        Array("abc", "def").transformIntoPartial[Seq[Int]].asOption ==> None
        Set("123", "xyz").transformIntoPartial[Array[Int]].asOption ==> None
      }
    }

    "partial map transform" - {

      "total inner transform" - {
        implicit val intPrinter: Transformer[Int, String] = _.toString

        Map(1 -> 10, 2 -> 20).transformIntoPartial[Map[String, String]].asOption ==> Some(Map("1" -> "10", "2" -> "20"))
        Map(1 -> 10, 2 -> 20).transformIntoPartial[Map[String, Int]].asOption ==> Some(Map("1" -> 10, "2" -> 20))
        Seq(1 -> 10, 2 -> 20).transformIntoPartial[Map[String, String]].asOption ==> Some(Map("1" -> "10", "2" -> "20"))
        ArrayBuffer(1 -> 10, 2 -> 20).transformIntoPartial[Map[Int, String]].asOption ==> Some(
          Map(1 -> "10", 2 -> "20")
        )
        Map(1 -> 10, 2 -> 20).transformIntoPartial[List[(String, String)]].asOption ==> Some(
          List("1" -> "10", "2" -> "20")
        )
        Map(1 -> 10, 2 -> 20).transformIntoPartial[Vector[(String, Int)]].asOption ==> Some(
          Vector("1" -> 10, "2" -> 20)
        )
        Array(1 -> 10, 2 -> 20).transformIntoPartial[Map[String, String]].asOption ==> Some(
          Map("1" -> "10", "2" -> "20")
        )
        Array(1 -> 10, 2 -> 20).transformIntoPartial[Map[Int, String]].asOption ==> Some(Map(1 -> "10", 2 -> "20"))
        Map(1 -> 10, 2 -> 20).transformIntoPartial[Array[(String, String)]].asOption.get ==> Array(
          "1" -> "10",
          "2" -> "20"
        )
        Map(1 -> 10, 2 -> 20).transformIntoPartial[Array[(String, Int)]].asOption.get ==> Array("1" -> 10, "2" -> 20)
      }

      "partial inner transform" - {
        implicit val intParserOpt: PartialTransformer[String, Int] =
          PartialTransformer(_.parseInt.toPartialTransformerResult)

        Map("1" -> "10", "2" -> "20").transformIntoPartial[Map[Int, Int]].asOption ==> Some(Map(1 -> 10, 2 -> 20))
        Map("1" -> "10", "2" -> "20").transformIntoPartial[Map[Int, String]].asOption ==> Some(
          Map(1 -> "10", 2 -> "20")
        )
        Seq("1" -> "10", "2" -> "20").transformIntoPartial[Map[Int, Int]].asOption ==> Some(Map(1 -> 10, 2 -> 20))
        ArrayBuffer("1" -> "10", "2" -> "20").transformIntoPartial[Map[String, Int]].asOption ==>
          Some(Map("1" -> 10, "2" -> 20))
        Map("1" -> "10", "2" -> "20").transformIntoPartial[List[(Int, Int)]].asOption ==> Some(List(1 -> 10, 2 -> 20))
        Map("1" -> "10", "2" -> "20").transformIntoPartial[Vector[(Int, String)]].asOption ==>
          Some(Vector(1 -> "10", 2 -> "20"))
        Array("1" -> "10", "2" -> "20").transformIntoPartial[Map[Int, Int]].asOption ==> Some(Map(1 -> 10, 2 -> 20))
        Array("1" -> "10", "2" -> "20").transformIntoPartial[Map[String, Int]].asOption ==> Some(
          Map("1" -> 10, "2" -> 20)
        )
        Map("1" -> "10", "2" -> "20").transformIntoPartial[Array[(Int, Int)]].asOption.get ==> Array(1 -> 10, 2 -> 20)
        Map("1" -> "10", "2" -> "20").transformIntoPartial[Array[(Int, String)]].asOption.get ==> Array(
          1 -> "10",
          2 -> "20"
        )

        Map("1" -> "x", "y" -> "20").transformIntoPartial[Map[Int, Int]].asOption ==> None
        Map("x" -> "10", "2" -> "20").transformIntoPartial[Map[Int, String]].asOption ==> None
        Seq("1" -> "10", "2" -> "x").transformIntoPartial[Map[Int, Int]].asOption ==> None
        ArrayBuffer("1" -> "x", "2" -> "y").transformIntoPartial[Map[String, Int]].asOption ==> None
        Map("x" -> "10", "y" -> "z").transformIntoPartial[List[(Int, Int)]].asOption ==> None
        Map("1" -> "10", "x" -> "20").transformIntoPartial[Vector[(Int, String)]].asOption ==> None
        Array("x" -> "y", "z" -> "v").transformIntoPartial[Map[Int, Int]].asOption ==> None
        Array("1" -> "x", "2" -> "y").transformIntoPartial[Map[String, Int]].asOption ==> None
        Map("1" -> "10", "x" -> "20").transformIntoPartial[Array[(Int, Int)]].asOption ==> None
        Map("x" -> "10", "y" -> "20").transformIntoPartial[Array[(Int, String)]].asOption ==> None
      }

      "transform with error paths support" - {
        implicit val intParserOpt: PartialTransformer[String, Int] =
          PartialTransformer(_.parseInt.toPartialTransformerResult)

        val result = Map("1" -> "x", "y" -> "20").transformIntoPartial[Map[Int, Int]]

        result.asOption ==> None
        result.asErrorPathMessagesStrings ==> Iterable(
          "(1)" -> "empty value",
          "keys(y)" -> "empty value",
        )

        "wrapped in case class" - {
          case class EnvelopeStr(map: Map[String, String])
          case class EnvelopeInt(map: Map[Int, Int])

          val result = EnvelopeStr(Map("1" -> "x", "y" -> "20")).transformIntoPartial[EnvelopeInt]

          result.asOption ==> None
          result.asErrorPathMessagesStrings ==> Iterable(
            "map(1)" -> "empty value",
            "map.keys(y)" -> "empty value",
          )
        }
      }
    }

    "partial sealed families" - {
      import numbers._

      "total inner transform" - {
        implicit val intPrinter: Transformer[Int, String] = _.toString
        import ScalesPartialTransformer.shortToLongTotalInner

        (short.Zero: short.NumScale[Int, Nothing])
          .transformIntoPartial[long.NumScale[String]]
          .asOption ==> Some(long.Zero)
        (short.Million(4): short.NumScale[Int, Nothing])
          .transformIntoPartial[long.NumScale[String]]
          .asOption ==> Some(long.Million("4"))
        (short.Billion(2): short.NumScale[Int, Nothing])
          .transformIntoPartial[long.NumScale[String]]
          .asOption ==> Some(long.Milliard("2"))
        (short.Trillion(100): short.NumScale[Int, Nothing])
          .transformIntoPartial[long.NumScale[String]]
          .asOption ==> Some(long.Billion("100"))
      }

      "partial inner transform" - {
        implicit val intParserOpt: PartialTransformer[String, Int] =
          PartialTransformer(_.parseInt.toPartialTransformerResult)
        import ScalesPartialTransformer.shortToLongPartialInner

        (short.Zero: short.NumScale[String, Nothing])
          .transformIntoPartial[long.NumScale[Int]]
          .asOption ==> Some(long.Zero)
        (short.Million("4"): short.NumScale[String, Nothing])
          .transformIntoPartial[long.NumScale[Int]]
          .asOption ==> Some(long.Million(4))
        (short.Billion("2"): short.NumScale[String, Nothing])
          .transformIntoPartial[long.NumScale[Int]]
          .asOption ==> Some(long.Milliard(2))
        (short.Trillion("100"): short.NumScale[String, Nothing])
          .transformIntoPartial[long.NumScale[Int]]
          .asOption ==> Some(long.Billion(100))

        (short.Million("x"): short.NumScale[String, Nothing])
          .transformIntoPartial[long.NumScale[Int]]
          .asOption ==> None
        (short.Billion("x"): short.NumScale[String, Nothing])
          .transformIntoPartial[long.NumScale[Int]]
          .asOption ==> None
        (short.Trillion("x"): short.NumScale[String, Nothing])
          .transformIntoPartial[long.NumScale[Int]]
          .asOption ==> None
      }
    }

    "support short circuit semantics" - {

      val result = Person("John", 10, 140)
        .intoPartial[User]
        .withFieldConstPartial(
          _.name,
          PartialTransformer.Result.fromErrors(
            Seq(PartialTransformer.Error.ofEmptyValue, PartialTransformer.Error.ofString("Bad name"))
          )
        )
        .withFieldConstPartial(_.height, PartialTransformer.Result.fromErrorString("Bad height"))
        .transformFailFast

      result.asOption ==> None
      result.asEither ==> Left(
        PartialTransformer.Result
          .Errors(
            Iterable(
              PartialTransformer.Error.ofEmptyValue,
              PartialTransformer.Error.ofString("Bad name")
            )
          )
          .wrapErrorPaths(PartialTransformer.ErrorPath.Accessor("name", _))
      )
    }
//
//    // TODO: "implicit conflict resolution" - {}
//
//    // TODO: "support scoped transformer configuration passed implicitly" - {}
//
    "support deriving partial transformer from pure" - {
      case class Foo(str: String)

      case class Bar(str: String, other: String)

      implicit val fooToBar: Transformer[Foo, Bar] =
        Transformer
          .define[Foo, Bar]
          .withFieldConst(_.other, "other")
          .buildTransformer

      val result = Foo("str").transformIntoPartial[Bar]

      result.asOption ==> Some(Bar("str", "other"))
      result.asEither ==> Right(Bar("str", "other"))
      result.asErrorPathMessagesStrings ==> Seq.empty
    }
  }
}