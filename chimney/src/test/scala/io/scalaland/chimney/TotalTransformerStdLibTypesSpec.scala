package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import utest._

object TotalTransformerStdLibTypesSpec extends TestSuite {

  val tests = Tests {


    test("not support converting non-Unit field to Unit field if there is no implicit converter allowing that") {
      case class Buzz(value: String)
      case class ConflictingFooBuzz(value: Unit)

      compileError("""Buzz("a").transformInto[ConflictingFooBuzz]""").check(
        "",
        "Chimney can't derive transformation from Buzz to ConflictingFooBuzz",
        "io.scalaland.chimney.TotalTransformerStdLibTypesSpec.ConflictingFooBuzz",
        "value: scala.Unit - can't derive transformation from value: java.lang.String in source type io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Buzz",
        "scala.Unit",
        "derivation from buzz.value: java.lang.String to scala.Unit is not supported in Chimney!",
        "Consult https://scalalandio.github.io/chimney for usage examples."
      )
    }

    test("support automatically filling of scala.Unit") {
      case class Buzz(value: String)
      case class NewBuzz(value: String, unit: Unit)
      case class FooBuzz(unit: Unit)
      case class ConflictingFooBuzz(value: Unit)

      Buzz("a").transformInto[NewBuzz] ==> NewBuzz("a", ())
      Buzz("a").transformInto[FooBuzz] ==> FooBuzz(())
      NewBuzz("a", null.asInstanceOf[Unit]).transformInto[FooBuzz] ==> FooBuzz(null.asInstanceOf[Unit])
    }

    test("transform from Option-type into Option-type") {
      Option(Foo("a")).transformInto[Option[Bar]] ==> Option(Bar("a"))
      (Some(Foo("a")): Option[Foo]).transformInto[Option[Bar]] ==> Option(Bar("a"))
      Some(Foo("a")).transformInto[Option[Bar]] ==> Some(Bar("a"))
      (None: Option[Foo]).transformInto[Option[Bar]] ==> None
      (None: Option[String]).transformInto[Option[String]] ==> None
      Option("abc").transformInto[Option[String]] ==> Some("abc")
      compileError("""Some("foobar").into[None.type].transform""").check(
        "",
        "Chimney can't derive transformation from Some[String] to None.type",
        "scala.None",
        "derivation from some: scala.Some to scala.None is not supported in Chimney!",
        "Consult https://scalalandio.github.io/chimney for usage examples."
      )
      case class BarNone(value: None.type)
      compileError("""Foo("a").into[BarNone].transform""").check(
        "",
        "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Foo to BarNone",
        "io.scalaland.chimney.TotalTransformerStdLibTypesSpec.BarNone",
        "value: scala.None - can't derive transformation from value: java.lang.String in source type io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Foo",
        "scala.None",
        "derivation from foo.value: java.lang.String to scala.None is not supported in Chimney!",
        "Consult https://scalalandio.github.io/chimney for usage examples."
      )
    }

    test("transform from non-Option-type into Option-type") {
      "abc".transformInto[Option[String]] ==> Some("abc")
      (null: String).transformInto[Option[String]] ==> None
    }

    test("transform from Either-type into Either-type") {
      (Left(Foo("a")): Either[Foo, Foo]).transformInto[Either[Bar, Bar]] ==> Left(Bar("a"))
      (Right(Foo("a")): Either[Foo, Foo]).transformInto[Either[Bar, Bar]] ==> Right(Bar("a"))
      Left(Foo("a")).transformInto[Either[Bar, Bar]] ==> Left(Bar("a"))
      Right(Foo("a")).transformInto[Either[Bar, Bar]] ==> Right(Bar("a"))
      Left(Foo("a")).transformInto[Left[Bar, Bar]] ==> Left(Bar("a"))
      Right(Foo("a")).transformInto[Right[Bar, Bar]] ==> Right(Bar("a"))
      (Left("a"): Either[String, String]).transformInto[Either[String, String]] ==> Left("a")
      (Right("a"): Either[String, String]).transformInto[Either[String, String]] ==> Right("a")
    }

    // TODO: transform from non-either to either

    test("transform from Iterable-type to Iterable-type") {
      Seq(Foo("a")).transformInto[Seq[Bar]] ==> Seq(Bar("a"))
      List(Foo("a")).transformInto[List[Bar]] ==> List(Bar("a"))
      Vector(Foo("a")).transformInto[Vector[Bar]] ==> Vector(Bar("a"))
      Set(Foo("a")).transformInto[Set[Bar]] ==> Set(Bar("a"))

      Seq("a").transformInto[Seq[String]] ==> Seq("a")
      List("a").transformInto[List[String]] ==> List("a")
      Vector("a").transformInto[Vector[String]] ==> Vector("a")
      Set("a").transformInto[Set[String]] ==> Set("a")

      List(Foo("a")).transformInto[Seq[Bar]] ==> Seq(Bar("a"))
      Vector(Foo("a")).transformInto[Seq[Bar]] ==> Seq(Bar("a"))

      List("a").transformInto[Seq[String]] ==> Seq("a")
      Vector("a").transformInto[Seq[String]] ==> Seq("a")
    }

    test("transform from Array-type to Array-type") {
      Array(Foo("a")).transformInto[Array[Foo]] ==> Array(Foo("a"))
      Array(Foo("a")).transformInto[Array[Bar]] ==> Array(Bar("a"))
      Array("a").transformInto[Array[String]] ==> Array("a")
    }

    test("transform between Array-type and Iterable-type") {
      Array(Foo("a")).transformInto[List[Bar]] ==> List(Bar("a"))
      Array("a", "b").transformInto[Seq[String]] ==> Seq("a", "b")
      Array(3, 2, 1).transformInto[Vector[Int]] ==> Vector(3, 2, 1)

      Vector("a").transformInto[Array[String]] ==> Array("a")
      List(1, 6, 3).transformInto[Array[Int]] ==> Array(1, 6, 3)
      Seq(Bar("x"), Bar("y")).transformInto[Array[Foo]] ==> Array(Foo("x"), Foo("y"))
    }

    test("transform from Map-type to Map-type") {
      Map("test" -> Foo("a")).transformInto[Map[String, Bar]] ==> Map("test" -> Bar("a"))
      Map("test" -> "a").transformInto[Map[String, String]] ==> Map("test" -> "a")
      Map(Foo("test") -> "x").transformInto[Map[Bar, String]] ==> Map(Bar("test") -> "x")
      Map(Foo("test") -> Foo("x")).transformInto[Map[Bar, Bar]] ==> Map(Bar("test") -> Bar("x"))
    }

    test("transform between Iterables and Maps") {
      Seq(Foo("10") -> Bar("20"), Foo("20") -> Bar("40")).transformInto[Map[Bar, Foo]] ==>
        Map(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))
      Map(Foo("10") -> Bar("20"), Foo("20") -> Bar("40")).transformInto[List[(Bar, Foo)]] ==>
        List(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))
    }

    test("transform between Arrays and Maps") {
      Array(Foo("10") -> Bar("20"), Foo("20") -> Bar("40")).transformInto[Map[Bar, Foo]] ==>
        Map(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))
      Map(Foo("10") -> Bar("20"), Foo("20") -> Bar("40")).transformInto[Array[(Bar, Foo)]] ==>
        Array(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))
    }

    test("flag .enableOptionDefaultsToNone") {

      case class Source(x: String)
      case class TargetWithOption(x: String, y: Option[Int])
      case class TargetWithOptionAndDefault(x: String, y: Option[Int] = Some(42))

      test("should be turned off by default and not allow compiling Option fields with missing source") {
        compileError("""Source("foo").into[TargetWithOption].transform""").check(
          "",
          "Chimney can't derive transformation from Source to TargetWithOption",
          "io.scalaland.chimney.TotalTransformerStdLibTypesSpec.TargetWithOption",
          "y: scala.Option - no accessor named y in source type io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Source",
          "Consult https://scalalandio.github.io/chimney for usage examples."
        )
      }

      test("use None for fields without source nor default value when enabled") {
        Source("foo").into[TargetWithOption].enableOptionDefaultsToNone.transform ==> TargetWithOption("foo", None)
      }

      test("use None for fields without source but with default value when enabled but default values disabled") {
        Source("foo")
          .into[TargetWithOptionAndDefault]
          .enableOptionDefaultsToNone
          .transform ==> TargetWithOptionAndDefault("foo", None)
      }

      test("should be ignored when default value is set and default values enabled") {
        Source("foo")
          .into[TargetWithOption]
          .enableDefaultValues
          .enableOptionDefaultsToNone
          .transform ==> TargetWithOption("foo", None)
        Source("foo")
          .into[TargetWithOptionAndDefault]
          .enableDefaultValues
          .enableOptionDefaultsToNone
          .transform ==> TargetWithOptionAndDefault(
          "foo",
          Some(42)
        )
      }
    }

    test("flag .enableUnsafeOption") {

      case class Source(x: Option[Int])
      case class Target(x: String)

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test(
        "should be turned off by default and not allow transforming Option[T] to S without explicitly existing converter"
      ) {
        compileError("""Source(Some(1)).into[Target].transform""").check(
          "",
          "Chimney can't derive transformation from Source to Target",
          "java.lang.String",
          "derivation from source.x: scala.Option to java.lang.String is not supported in Chimney!",
          "io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Target",
          "x: java.lang.String - can't derive transformation from x: scala.Option in source type io.scalaland.chimney.TotalTransformerStdLibTypesSpec.Source",
          "Consult https://scalalandio.github.io/chimney for usage examples."
        )
      }

      test("use .get to extract Option source when enabled") {
        Option(10).into[String].enableUnsafeOption.transform ==> "10"
        Source(Some(10)).into[Target].enableUnsafeOption.transform ==> Target("10")
        intercept[NoSuchElementException] {
          Option.empty[Int].into[String].enableUnsafeOption.transform
        }
        intercept[NoSuchElementException] {
          Source(None).into[Target].enableUnsafeOption.transform
        }
      }

      test("should be ignored if implicit (presumably safe) Transformer from Option exists") {
        implicit val optIntPrinter: Transformer[Option[Int], String] = _.map(_ * 2).fold("empty")(_.toString)

        Option(10).into[String].enableUnsafeOption.transform ==> "20"
        Source(Some(10)).into[Target].enableUnsafeOption.transform ==> Target("20")
        Option.empty[Int].into[String].enableUnsafeOption.transform ==> "empty"
        Source(None).into[Target].enableUnsafeOption.transform ==> Target("empty")
      }
    }
  }

  case class Foo(value: String)
  case class Bar(value: String)
}
