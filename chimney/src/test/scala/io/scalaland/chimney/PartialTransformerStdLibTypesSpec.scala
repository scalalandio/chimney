package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.partial.syntax.*
import io.scalaland.chimney.utils.OptionUtils.*

import scala.annotation.unused
import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer

class PartialTransformerStdLibTypesSpec extends ChimneySpec {

  test("not support converting non-Unit field to Unit field if there is no implicit converter allowing that") {
    @unused case class Buzz(value: String)
    @unused case class ConflictingFooBuzz(value: Unit)

    compileErrors("""Buzz("a").transformIntoPartial[ConflictingFooBuzz]""").check(
      "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Buzz to io.scalaland.chimney.PartialTransformerStdLibTypesSpec.ConflictingFooBuzz",
      "io.scalaland.chimney.PartialTransformerStdLibTypesSpec.ConflictingFooBuzz",
      "  value: scala.Unit - can't derive transformation from value: java.lang.String in source type io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Buzz",
      "scala.Unit",
      "  derivation from buzz.value: java.lang.String to scala.Unit is not supported in Chimney!",
      "Consult https://chimney.readthedocs.io for usage examples."
    )
  }

  test("support automatically filling of scala.Unit") {
    case class Buzz(value: String)
    case class NewBuzz(value: String, unit: Unit)
    @unused case class FooBuzz(unit: Unit)
    @unused case class ConflictingFooBuzz(value: Unit)

    Buzz("a").transformIntoPartial[NewBuzz].asOption ==> Some(NewBuzz("a", ()))
    Buzz("a").transformIntoPartial[FooBuzz].asOption ==> Some(FooBuzz(()))
    NewBuzz("a", null.asInstanceOf[Unit]).transformIntoPartial[FooBuzz].asOption ==> Some(
      FooBuzz(null.asInstanceOf[Unit])
    )
  }

  group("transform from Option-type into Option-type, using Total Transformer for inner type transformation") {

    implicit val intPrinter: Transformer[Int, String] = _.toString

    test("when inner value is non-empty") {
      val result = Option(123).transformIntoPartial[Option[String]]

      result.asOption ==> Some(Some("123"))
      result.asEither ==> Right(Some("123"))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when inner value is empty") {
      val result = Option.empty[Int].transformIntoPartial[Option[String]]

      result.asOption ==> Some(None)
      result.asEither ==> Right(None)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("transform from Option-type into Option-type, using Partial Transformer for inner type transformation") {

    implicit val intPartialParser: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.orStringAsResult("bad int"))

    test("when Result is success") {
      val result = Option("123").transformIntoPartial[Option[Int]]

      result.asOption ==> Some(Some(123))
      result.asEither ==> Right(Some(123))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when Result is failure") {
      val result = Option("abc").transformIntoPartial[Option[Int]]
      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result.Errors.fromString("bad int")
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "" -> "bad int"
      )
    }

    test("when Result is null") {
      val result = Option.empty[String].transformIntoPartial[Option[Int]]

      result.asOption ==> Some(None)
      result.asEither ==> Right(None)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("transform from non-Option-type into Option-type, using Total Transformer for inner type transformation") {

    implicit val intPrinter: Transformer[Int, String] = _.toString

    test("when inner value is non-null") {
      val result = 10.transformIntoPartial[Option[String]]

      result.asOption ==> Some(Some("10"))
      result.asEither ==> Right(Some("10"))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when inner value is null") {
      implicit val integerPrinter: Transformer[Integer, String] = _.toString

      val result = (null: Integer).transformIntoPartial[Option[String]]

      result.asOption ==> Some(None)
      result.asEither ==> Right(None)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("transform from non-Option-type into Option-type, using Partial Transformer for inner type transformation") {

    implicit val intPartialParser: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.orStringAsResult("bad int"))

    test("when Result is success") {
      val result = "123".transformIntoPartial[Option[Int]]

      result.asOption ==> Some(Some(123))
      result.asEither ==> Right(Some(123))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when Result is failure") {
      val result = "abc".transformIntoPartial[Option[Int]]
      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result.Errors.fromString("bad int")
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "" -> "bad int"
      )
    }

    test("when Result is null") {
      val result = (null: String).transformIntoPartial[Option[Int]]

      result.asOption ==> Some(None)
      result.asEither ==> Right(None)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("transform from Option-type into non-Option-type, using Total Transformer for inner type transformation") {

    implicit val intPrinter: Transformer[Int, String] = _.toString

    test("when option is non-empty") {
      val result = Option(10).transformIntoPartial[String]

      result.asOption ==> Some("10")
      result.asEither ==> Right("10")
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when option is empty") {
      val result = Option.empty[Int].transformIntoPartial[String]

      result.asOption ==> None
      result.asEither ==> Left(partial.Result.fromEmpty)
      result.asErrorPathMessageStrings ==> Iterable(("", "empty value"))
    }
  }

  group("transform from Option-type into non-Option-type, using Partial Transformer for inner type transformation") {

    implicit val intPartialParser: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.orStringAsResult("bad int"))

    test("when option is non-empty and inner is success") {
      val result = Option("10").transformIntoPartial[Int]

      result.asOption ==> Some(10)
      result.asEither ==> Right(10)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when option is non-empty and inner is failure") {
      val result = Some("abc").transformIntoPartial[Int]

      result.asOption ==> None
      result.asEither ==> Left(partial.Result.fromErrorString("bad int"))
      result.asErrorPathMessageStrings ==> Iterable("" -> "bad int")
    }

    test("when option is empty") {
      val result = (None: Option[String]).transformIntoPartial[Int]

      result.asOption ==> None
      result.asEither ==> Left(partial.Result.fromEmpty)
      result.asErrorPathMessageStrings ==> Iterable(("", "empty value"))
    }
  }

  test("transform into Option-type with an override") {
    "abc".intoPartial[Option[String]].withFieldConst(_.matchingSome, "def").transform.asOption ==> Some(Some("def"))
    "abc".intoPartial[Option[String]].withFieldConst(_.matching[Some[String]].value, "def").transform.asOption ==> Some(
      Some("def")
    )
    Option("abc")
      .intoPartial[Option[String]]
      .withFieldConst(_.matching[Some[String]].value, "def")
      .transform
      .asOption ==> Some(Some("def"))

    import fixtures.products.Renames.*

    Option(User(1, "Kuba", Some(28)))
      .intoPartial[Option[UserPLStd]]
      .withFieldRenamed(_.matchingSome.name, _.matchingSome.imie)
      .withFieldRenamed(_.matchingSome.age, _.matchingSome.wiek)
      .transform
      .asOption
      .get ==> Option(UserPLStd(1, "Kuba", Some(28)))

    Option(User(1, "Kuba", Some(28)))
      .intoPartial[Option[UserPLStd]]
      .withFieldRenamed(_.matching[Some[User]].value.name, _.matching[Some[UserPLStd]].value.imie)
      .withFieldRenamed(_.matching[Some[User]].value.age, _.matching[Some[UserPLStd]].value.wiek)
      .transform
      .asOption
      .get ==> Option(UserPLStd(1, "Kuba", Some(28)))
  }

  test("transform from Either-type into Either-type, using Total Transformer for inner types transformation") {
    implicit val intPrinter: Transformer[Int, String] = _.toString

    (Left(1): Either[Int, Int]).transformIntoPartial[Either[String, String]].asOption ==> Some(Left("1"))
    (Right(1): Either[Int, Int]).transformIntoPartial[Either[String, String]].asOption ==> Some(Right("1"))
    Left(1).transformIntoPartial[Either[String, String]].asOption ==> Some(Left("1"))
    Right(1).transformIntoPartial[Either[String, String]].asOption ==> Some(Right("1"))
    Left(1).transformIntoPartial[Left[String, String]].asOption ==> Some(Left("1"))
    Right(1).transformIntoPartial[Right[String, String]].asOption ==> Some(Right("1"))
  }

  test("transform from Either-type into Either-type, using Partial Transformer for inner types transformation") {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

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

  test("transform Either-type with an override") {
    (Left("a"): Either[String, String])
      .intoPartial[Either[String, String]]
      .withFieldConst(_.matchingLeft, "b")
      .withFieldConst(_.matchingRight, "c")
      .transform
      .asOption ==> Some(Left("b"))
    (Left("a"): Either[String, String])
      .intoPartial[Either[String, String]]
      .withFieldConst(_.matching[Left[String, String]].value, "b")
      .withFieldConst(_.matching[Right[String, String]].value, "c")
      .transform
      .asOption ==> Some(Left("b"))
    (Right("a"): Either[String, String])
      .intoPartial[Either[String, String]]
      .withFieldConst(_.matchingLeft, "b")
      .withFieldConst(_.matchingRight, "c")
      .transform
      .asOption ==> Some(Right("c"))
    (Right("a"): Either[String, String])
      .intoPartial[Either[String, String]]
      .withFieldConst(_.matching[Left[String, String]].value, "b")
      .withFieldConst(_.matching[Right[String, String]].value, "c")
      .transform
      .asOption ==> Some(Right("c"))
    Left("a")
      .intoPartial[Either[String, String]]
      .withFieldConst(_.matchingLeft, "b")
      .transform
      .asOption ==> Some(Left("b"))
    Left("a")
      .intoPartial[Either[String, String]]
      .withFieldConst(_.matching[Left[String, String]].value, "b")
      .transform
      .asOption ==> Some(Left("b"))
    Right("a")
      .intoPartial[Either[String, String]]
      .withFieldConst(_.matchingRight, "c")
      .transform
      .asOption ==> Some(Right("c"))
    Right("a")
      .intoPartial[Either[String, String]]
      .withFieldConst(_.matching[Right[String, String]].value, "c")
      .transform
      .asOption ==> Some(Right("c"))

    import fixtures.products.Renames.*

    (Left(User(1, "Kuba", Some(28))): Either[User, User])
      .intoPartial[Either[UserPLStd, UserPLStd]]
      .withFieldRenamed(_.matchingLeft.name, _.matchingLeft.imie)
      .withFieldRenamed(_.matchingLeft.age, _.matchingLeft.wiek)
      .withFieldRenamed(_.matchingRight.name, _.matchingRight.imie)
      .withFieldRenamed(_.matchingRight.age, _.matchingRight.wiek)
      .transform
      .asOption
      .get ==> Left(UserPLStd(1, "Kuba", Some(28)))
    (Left(User(1, "Kuba", Some(28))): Either[User, User])
      .intoPartial[Either[UserPLStd, UserPLStd]]
      .withFieldRenamed(_.matching[Left[User, User]].value.name, _.matching[Left[UserPLStd, UserPLStd]].value.imie)
      .withFieldRenamed(_.matching[Left[User, User]].value.age, _.matching[Left[UserPLStd, UserPLStd]].value.wiek)
      .withFieldRenamed(_.matching[Right[User, User]].value.name, _.matching[Right[UserPLStd, UserPLStd]].value.imie)
      .withFieldRenamed(_.matching[Right[User, User]].value.age, _.matching[Right[UserPLStd, UserPLStd]].value.wiek)
      .transform
      .asOption
      .get ==> Left(UserPLStd(1, "Kuba", Some(28)))
    (Right(User(1, "Kuba", Some(28))): Either[User, User])
      .intoPartial[Either[UserPLStd, UserPLStd]]
      .withFieldRenamed(_.matchingLeft.name, _.matchingLeft.imie)
      .withFieldRenamed(_.matchingLeft.age, _.matchingLeft.wiek)
      .withFieldRenamed(_.matchingRight.name, _.matchingRight.imie)
      .withFieldRenamed(_.matchingRight.age, _.matchingRight.wiek)
      .transform
      .asOption
      .get ==> Right(UserPLStd(1, "Kuba", Some(28)))
    (Right(User(1, "Kuba", Some(28))): Either[User, User])
      .intoPartial[Either[UserPLStd, UserPLStd]]
      .withFieldRenamed(_.matching[Left[User, User]].value.name, _.matching[Left[UserPLStd, UserPLStd]].value.imie)
      .withFieldRenamed(_.matching[Left[User, User]].value.age, _.matching[Left[UserPLStd, UserPLStd]].value.wiek)
      .withFieldRenamed(_.matching[Right[User, User]].value.name, _.matching[Right[UserPLStd, UserPLStd]].value.imie)
      .withFieldRenamed(_.matching[Right[User, User]].value.age, _.matching[Right[UserPLStd, UserPLStd]].value.wiek)
      .transform
      .asOption
      .get ==> Right(UserPLStd(1, "Kuba", Some(28)))
  }

  test("transform Iterable-type to Iterable-type, using Total Transformer for inner type transformation") {
    implicit val intPrinter: Transformer[Int, String] = _.toString

    List(123, 456).transformIntoPartial[Vector[String]].asOption ==> Some(Vector("123", "456"))
    Vector(123, 456).transformIntoPartial[Queue[String]].asOption ==> Some(Queue("123", "456"))
    Queue(123, 456).transformIntoPartial[List[String]].asOption ==> Some(List("123", "456"))
  }

  test("transform Iterable-type to Iterable-type, using Partial Transformer for inner type transformation") {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

    List("123", "456").transformIntoPartial[List[Int]].asOption ==> Some(Vector(123, 456))
    Vector("123", "456").transformIntoPartial[Queue[Int]].asOption ==> Some(Queue(123, 456))
    Queue("123", "456").transformIntoPartial[List[Int]].asOption ==> Some(List(123, 456))

    List("abc", "456").transformIntoPartial[Vector[Int]].asOption ==> None
    Vector("123", "def").transformIntoPartial[Queue[Int]].asOption ==> None
    Queue("123", "def").transformIntoPartial[List[Int]].asOption ==> None

    List("abc", "456", "ghi")
      .transformIntoPartial[Vector[Int]](failFast = false)
      .asErrorPathMessageStrings ==> Iterable("(0)" -> "empty value", "(2)" -> "empty value")
    Vector("123", "def", "ghi")
      .transformIntoPartial[Queue[Int]](failFast = false)
      .asErrorPathMessageStrings ==> Iterable("(1)" -> "empty value", "(2)" -> "empty value")
    Queue("123", "def", "ghi")
      .transformIntoPartial[List[Int]](failFast = false)
      .asErrorPathMessageStrings ==> Iterable("(1)" -> "empty value", "(2)" -> "empty value")

    List("abc", "456", "ghi")
      .transformIntoPartial[Vector[Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("(0)" -> "empty value")
    Vector("123", "def", "ghi")
      .transformIntoPartial[Queue[Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("(1)" -> "empty value")
    Queue("123", "def", "ghi")
      .transformIntoPartial[List[Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("(1)" -> "empty value")
  }

  test("transform Array-type to Array-type, using Total Transformer for inner type transformation") {
    implicit val intPrinter: Transformer[Int, String] = _.toString

    Array(123, 456).transformIntoPartial[Array[String]].asOption.get ==> Array("123", "456")
  }

  test("transform Array-type to Array-type, using Partial Transformer for inner type transformation") {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

    Array("123", "456").transformIntoPartial[Array[Int]].asOption.get ==> Array(123, 456)
    Array("abc", "456").transformIntoPartial[Array[Int]].asOption ==> None

    Array("abc", "456", "ghi")
      .transformIntoPartial[Array[Int]]
      .asErrorPathMessageStrings ==> Iterable("(0)" -> "empty value", "(2)" -> "empty value")

    Array("abc", "456", "ghi")
      .transformIntoPartial[Array[Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("(0)" -> "empty value")
  }

  test("transform between Array-type and Iterable-type, using Total Transformer for inner type transformation") {
    implicit val intPrinter: Transformer[Int, String] = _.toString

    Array(123, 456).transformIntoPartial[Set[String]].asOption ==> Some(Set("123", "456"))
    Array.empty[Int].transformIntoPartial[Set[String]].asOption ==> Some(Set.empty[String])
  }

  test("transform between Array-type and Iterable-type, using Partial Transformer for inner type transformation") {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

    Set("123", "456").transformIntoPartial[Array[Int]].asOption.get.sorted ==> Array(123, 456)
    Set("123", "xyz").transformIntoPartial[Array[Int]].asOption ==> None
    Set.empty[String].transformIntoPartial[Array[Int]].asOption.get ==> Array.empty[String]

    Array("123", "456").transformIntoPartial[Set[Int]].asOption ==> Some(Set(123, 456))
    Array("123", "xyz").transformIntoPartial[Set[Int]].asOption ==> None
    Array.empty[String].transformIntoPartial[Set[Int]].asOption ==> Some(Set.empty[Int])

    Array("123", "xyz", "ghi")
      .transformIntoPartial[Set[Int]]
      .asErrorPathMessageStrings ==> Iterable("(1)" -> "empty value", "(2)" -> "empty value")

    Array("123", "xyz", "ghi")
      .transformIntoPartial[Set[Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("(1)" -> "empty value")
  }

  test("transform into sequential type with an override") {
    import TotalTransformerStdLibTypesSpec.*

    Iterable(Foo("a")).intoPartial[Seq[Bar]].withFieldConst(_.everyItem.value, "override").transform.asOption ==>
      Some(Seq(Bar("override")))
    Iterable(Foo("a")).intoPartial[List[Bar]].withFieldConst(_.everyItem.value, "override").transform.asOption ==> Some(
      List(Bar("override"))
    )
    Iterable(Foo("a"))
      .intoPartial[Vector[Bar]]
      .withFieldConst(_.everyItem.value, "override")
      .transform
      .asOption ==> Some(Vector(Bar("override")))
    Iterable(Foo("a")).intoPartial[Set[Bar]].withFieldConst(_.everyItem.value, "override").transform.asOption ==> Some(
      Set(Bar("override"))
    )
    Iterable(Foo("a"))
      .intoPartial[Array[Bar]]
      .withFieldConst(_.everyItem.value, "override")
      .transform
      .asOption
      .get ==> Array(Bar("override"))

    import fixtures.products.Renames.*

    List(User(1, "Kuba", Some(28)))
      .intoPartial[List[UserPLStd]]
      .withFieldRenamed(_.everyItem.name, _.everyItem.imie)
      .withFieldRenamed(_.everyItem.age, _.everyItem.wiek)
      .transform
      .asOption
      .get ==> List(UserPLStd(1, "Kuba", Some(28)))
  }

  test("transform Map-type to Map-type, using Total Transformer for inner type transformation") {
    implicit val intPrinter: Transformer[Int, String] = _.toString

    Map(1 -> 10, 2 -> 20).transformIntoPartial[Map[String, String]].asOption ==> Some(Map("1" -> "10", "2" -> "20"))
    Map(1 -> 10, 2 -> 20).transformIntoPartial[Map[String, Int]].asOption ==> Some(Map("1" -> 10, "2" -> 20))
  }

  test("transform Map-type to Map-type, using Partial Transformer for inner type transformation") {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

    Map("1" -> "10", "2" -> "20").transformIntoPartial[Map[Int, Int]].asOption ==> Some(Map(1 -> 10, 2 -> 20))
    Map("1" -> "10", "2" -> "20").transformIntoPartial[Map[Int, String]].asOption ==> Some(
      Map(1 -> "10", 2 -> "20")
    )

    Map("1" -> "x", "y" -> "20").transformIntoPartial[Map[Int, Int]].asOption ==> None
    Map("x" -> "10", "2" -> "20").transformIntoPartial[Map[Int, String]].asOption ==> None

    Map("1" -> "x", "y" -> "20")
      .transformIntoPartial[Map[Int, Int]]
      .asErrorPathMessageStrings ==> Iterable("(1)" -> "empty value", "keys(y)" -> "empty value")

    Map("1" -> "x", "y" -> "20")
      .transformIntoPartial[Map[Int, Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("(1)" -> "empty value")
  }

  test("transform between Iterables and Maps, using Total Transformer for inner type transformation") {
    implicit val intPrinter: Transformer[Int, String] = _.toString

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
  }

  test("transform between Iterables and Maps, using Partial Transformer for inner type transformation") {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

    Seq("1" -> "10", "2" -> "20").transformIntoPartial[Map[Int, Int]].asOption ==> Some(Map(1 -> 10, 2 -> 20))
    ArrayBuffer("1" -> "10", "2" -> "20").transformIntoPartial[Map[String, Int]].asOption ==>
      Some(Map("1" -> 10, "2" -> 20))
    Map("1" -> "10", "2" -> "20").transformIntoPartial[List[(Int, Int)]].asOption ==> Some(List(1 -> 10, 2 -> 20))
    Map("1" -> "10", "2" -> "20").transformIntoPartial[Vector[(Int, String)]].asOption ==>
      Some(Vector(1 -> "10", 2 -> "20"))

    Seq("1" -> "10", "2" -> "x").transformIntoPartial[Map[Int, Int]].asOption ==> None
    ArrayBuffer("1" -> "x", "2" -> "y").transformIntoPartial[Map[String, Int]].asOption ==> None
    Map("x" -> "10", "y" -> "z").transformIntoPartial[List[(Int, Int)]].asOption ==> None
    Map("1" -> "10", "x" -> "20").transformIntoPartial[Vector[(Int, String)]].asOption ==> None

    Seq("1" -> "10", "2" -> "x").transformIntoPartial[Map[Int, Int]].asOption ==> None
    ArrayBuffer("1" -> "x", "2" -> "y").transformIntoPartial[Map[String, Int]].asOption ==> None
    Map("x" -> "10", "y" -> "z").transformIntoPartial[List[(Int, Int)]].asOption ==> None
    Map("1" -> "10", "x" -> "20").transformIntoPartial[Vector[(Int, String)]].asOption ==> None

    ArrayBuffer("1" -> "x", "2" -> "y")
      .transformIntoPartial[Map[String, Int]]
      .asErrorPathMessageStrings ==> Iterable("(0)._2" -> "empty value", "(1)._2" -> "empty value")
    Map("x" -> "10", "y" -> "z")
      .transformIntoPartial[List[(Int, Int)]]
      .asErrorPathMessageStrings ==> Iterable(
      "keys(x)" -> "empty value",
      "keys(y)" -> "empty value",
      "(y)" -> "empty value"
    )

    ArrayBuffer("1" -> "x", "2" -> "y")
      .transformIntoPartial[Map[String, Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("(0)._2" -> "empty value")
    Map("x" -> "10", "y" -> "z")
      .transformIntoPartial[List[(Int, Int)]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("keys(x)" -> "empty value")
  }

  test("transform between Arrays and Maps, using Total Transformer for inner type transformation") {
    implicit val intPrinter: Transformer[Int, String] = _.toString

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

  test("transform between Arrays and Maps, using Partial Transformer for inner type transformation") {
    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)

    Array("1" -> "10", "2" -> "20").transformIntoPartial[Map[Int, Int]].asOption ==> Some(Map(1 -> 10, 2 -> 20))
    Array("1" -> "10", "2" -> "20").transformIntoPartial[Map[String, Int]].asOption ==> Some(
      Map("1" -> 10, "2" -> 20)
    )
    Map("1" -> "10", "2" -> "20").transformIntoPartial[Array[(Int, Int)]].asOption.get ==> Array(1 -> 10, 2 -> 20)
    Map("1" -> "10", "2" -> "20").transformIntoPartial[Array[(Int, String)]].asOption.get ==> Array(
      1 -> "10",
      2 -> "20"
    )

    Array("x" -> "y", "z" -> "v").transformIntoPartial[Map[Int, Int]].asOption ==> None
    Array("1" -> "x", "2" -> "y").transformIntoPartial[Map[String, Int]].asOption ==> None
    Map("1" -> "10", "x" -> "20").transformIntoPartial[Array[(Int, Int)]].asOption ==> None
    Map("x" -> "10", "y" -> "20").transformIntoPartial[Array[(Int, String)]].asOption ==> None

    Array("1" -> "x", "2" -> "y")
      .transformIntoPartial[Map[String, Int]]
      .asErrorPathMessageStrings ==> Iterable("(0)._2" -> "empty value", "(1)._2" -> "empty value")
    Map("x" -> "10", "y" -> "20")
      .transformIntoPartial[Array[(Int, String)]]
      .asErrorPathMessageStrings ==> Iterable("keys(x)" -> "empty value", "keys(y)" -> "empty value")

    Array("1" -> "x", "2" -> "y")
      .transformIntoPartial[Map[String, Int]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("(0)._2" -> "empty value")
    Map("x" -> "10", "y" -> "20")
      .transformIntoPartial[Array[(Int, String)]](failFast = true)
      .asErrorPathMessageStrings ==> Iterable("keys(x)" -> "empty value")
  }

  test("transform into map type with an override") {
    import TotalTransformerStdLibTypesSpec.*

    Iterable(Foo("a") -> Foo("b"))
      .intoPartial[Map[Bar, Bar]]
      .withFieldConst(_.everyMapKey.value, "ov1")
      .withFieldConst(_.everyMapValue.value, "ov2")
      .transform
      .asOption ==> Some(Map(Bar("ov1") -> Bar("ov2")))
    Iterable(Foo("a") -> Foo("b"))
      .intoPartial[Map[Bar, Bar]]
      .withFieldRenamed(_.everyItem._1.value, _.everyMapKey.value)
      .withFieldRenamed(_.everyItem._2.value, _.everyMapValue.value)
      .transform
      .asOption ==> Some(Map(Bar("a") -> Bar("b")))

    import fixtures.products.Renames.*

    Map(User(1, "Kuba", Some(28)) -> User(1, "Kuba", Some(28)))
      .intoPartial[Map[UserPLStd, UserPLStd]]
      .withFieldRenamed(_.everyMapKey.name, _.everyMapKey.imie)
      .withFieldRenamed(_.everyMapKey.age, _.everyMapKey.wiek)
      .withFieldRenamed(_.everyMapValue.name, _.everyMapValue.imie)
      .withFieldRenamed(_.everyMapValue.age, _.everyMapValue.wiek)
      .transform
      .asOption
      .get ==> Map(UserPLStd(1, "Kuba", Some(28)) -> UserPLStd(1, "Kuba", Some(28)))
  }

  group("flag .enableOptionDefaultsToNone") {

    case class Source(x: String)
    case class TargetWithOption(x: String, y: Option[Int])
    case class TargetWithOptionAndDefault(x: String, y: Option[Int] = Some(42))

    test("should be turned off by default and not allow compiling Option fields with missing source") {
      compileErrors("""Source("foo").transformIntoPartial[TargetWithOption]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Source to io.scalaland.chimney.PartialTransformerStdLibTypesSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerStdLibTypesSpec.TargetWithOption",
        "  y: scala.Option[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Source",
        "There are default optional values available for y, the constructor argument/setter in io.scalaland.chimney.PartialTransformerStdLibTypesSpec.TargetWithOption. Consider using .enableOptionDefaultsToNone.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      compileErrors("""Source("foo").intoPartial[TargetWithOption].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Source to io.scalaland.chimney.PartialTransformerStdLibTypesSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerStdLibTypesSpec.TargetWithOption",
        "  y: scala.Option[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Source",
        "There are default optional values available for y, the constructor argument/setter in io.scalaland.chimney.PartialTransformerStdLibTypesSpec.TargetWithOption. Consider using .enableOptionDefaultsToNone.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should use None for fields without source nor default value when enabled") {
      Source("foo").intoPartial[TargetWithOption].enableOptionDefaultsToNone.transform.asOption ==> Some(
        TargetWithOption("foo", None)
      )
      locally {
        implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone
        Source("foo").transformIntoPartial[TargetWithOption].asOption ==> Some(TargetWithOption("foo", None))
      }
    }

    test("should use None for fields without source but with default value when enabled but default values disabled") {
      Source("foo").intoPartial[TargetWithOptionAndDefault].enableOptionDefaultsToNone.transform.asOption ==> Some(
        TargetWithOptionAndDefault("foo", None)
      )
      locally {
        implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone

        Source("foo").transformIntoPartial[TargetWithOptionAndDefault].asOption ==> Some(
          TargetWithOptionAndDefault("foo", None)
        )
      }
    }

    test("should be ignored when default value is set and default values enabled") {
      Source("foo")
        .intoPartial[TargetWithOption]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform
        .asOption ==> Some(
        TargetWithOption("foo", None)
      )
      Source("foo")
        .intoPartial[TargetWithOptionAndDefault]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform
        .asOption ==> Some(
        TargetWithOptionAndDefault(
          "foo",
          Some(42)
        )
      )
    }

    test(
      "use None for fields without source but with default value when enabled only for a single field when scoped using .withTargetFlag(_.field)"
    ) {
      Source("foo")
        .intoPartial[TargetWithOptionAndDefault]
        .withTargetFlag(_.y)
        .enableOptionDefaultsToNone
        .transform
        .asOption ==> Some(TargetWithOptionAndDefault("foo", None))
    }
  }

  group("flag .disableOptionDefaultsToNone") {

    @unused case class Source(x: String)
    @unused case class TargetWithOption(x: String, y: Option[Int])

    test("should disable globally enabled .enableOptionDefaultsToNone") {
      @unused implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone

      compileErrors("""Source("foo").intoPartial[TargetWithOption].disableOptionDefaultsToNone.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Source to io.scalaland.chimney.PartialTransformerStdLibTypesSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerStdLibTypesSpec.TargetWithOption",
        "  y: scala.Option[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Source",
        "There are default optional values available for y, the constructor argument/setter in io.scalaland.chimney.PartialTransformerStdLibTypesSpec.TargetWithOption. Consider using .enableOptionDefaultsToNone.",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

  group("flag .enablePartialUnwrapsOption") {

    case class Source(a: Option[String])
    case class Target(a: String)

    test("should be turned on by default") {
      Source(Some("value")).transformIntoPartial[Target].asOption ==> Some(Target("value"))
      Source(Some("value")).intoPartial[Target].transform.asOption ==> Some(Target("value"))
    }

    test("should re-enable globally disabled .disablePartialUnwrapsOption") {
      implicit val config = TransformerConfiguration.default.disablePartialUnwrapsOption

      Source(Some("value")).intoPartial[Target].enablePartialUnwrapsOption.transform.asOption ==> Some(Target("value"))
    }

    test("should be turned on only for a single field when scoped using .withTargetFlag(_.field)") {
      implicit val config = TransformerConfiguration.default.disablePartialUnwrapsOption

      Source(Some("value"))
        .intoPartial[Target]
        .withTargetFlag(_.a)
        .enablePartialUnwrapsOption
        .transform
        .asOption ==> Some(Target("value"))
    }
  }

  group("flag .disablePartialUnwrapsOption") {

    @unused case class Source(a: Option[String])
    @unused case class Target(a: String)

    test("should fail compilation if Option unwrapping is not provided when disabled") {
      compileErrors("""Source(Some("value")).intoPartial[Target].disablePartialUnwrapsOption.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Source to io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Target",
        "io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Target",
        "  a: java.lang.String - can't derive transformation from a: scala.Option[java.lang.String] in source type io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Source",
        "java.lang.String (transforming from: a into: a)",
        "  derivation from source.a: scala.Option[java.lang.String] to java.lang.String is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      locally {
        @unused implicit val config = TransformerConfiguration.default.disablePartialUnwrapsOption

        compileErrors("""Source(Some("value")).transformIntoPartial[Target]""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Source to io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Target",
          "io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Target",
          "  a: java.lang.String - can't derive transformation from a: scala.Option[java.lang.String] in source type io.scalaland.chimney.PartialTransformerStdLibTypesSpec.Source",
          "java.lang.String (transforming from: a into: a)",
          "  derivation from source.a: scala.Option[java.lang.String] to java.lang.String is not supported in Chimney!",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
      }
    }
  }
}
