package io.scalaland.chimney

import org.scalatest.{MustMatchers, WordSpec}
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._

class DslSpec extends WordSpec with MustMatchers {

  "A Chimney DSL" should {

    "use implicit transformer directly" in {

      import Domain1._

      implicit val _ = userNameToStringTransformer

      UserName("Batman").into[String].transform mustBe "BatmanT"
      UserName("Batman").transformInto[String] mustBe "BatmanT"
    }

    "use implicit transformer for nested field" in {

      import Domain1._

      implicit val _ = userNameToStringTransformer

      val batman = User("123", UserName("Batman"))
      val batmanDTO = batman.transformInto[UserDTO]

      batmanDTO.id mustBe "123"
      batmanDTO.name mustBe "BatmanT"
    }

    "support different set of fields of source and target" when {

      case class Foo(x: Int, y: String, z: (Double, Double))
      case class Bar(x: Int, z: (Double, Double))
      case class HaveY(y: String)
      val haveY = HaveY("")

      "field is dropped in the target" in {
        Foo(3, "pi", (3.14, 3.14)).transformInto[Bar] mustBe Bar(3, (3.14, 3.14))
      }

      "field is added to the target" should {

        "not compile if source for the target fields is not provided" in {

          assertTypeError("Bar(3, (3.14, 3.14)).transformInto[Foo]")
        }

        "fill the field with provided default value" should {

          "pass when selector is valid" in {

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldConst(_.y, "pi")
              .transform mustBe
              Foo(3, "pi", (3.14, 3.14))

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldConst(cc => cc.y, "pi")
              .transform mustBe
              Foo(3, "pi", (3.14, 3.14))
          }

          "not compile when the selector is invalid" in {

            assertTypeError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(_.y, "pi")
                  .withFieldConst(_.z._1, 0.0)
                  .transform
                """)

            assertTypeError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(_.y + "abc", "pi")
                  .transform
                """)

            assertTypeError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(cc => haveY.y, "pi")
                  .transform
                """)
          }
        }

        "fill the field with provided generator function" should {

          "pass when selector is valid" in {

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldComputed(_.y, _.x.toString)
              .transform mustBe
              Foo(3, "3", (3.14, 3.14))

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldComputed(cc => cc.y, _.x.toString)
              .transform mustBe
              Foo(3, "3", (3.14, 3.14))
          }

          "not compile when the selector is invalid" in {

            assertTypeError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(_.y, _.x.toString)
                  .withFieldComputed(_.z._1, _.z._1 * 10.0)
                  .transform
                """)

            assertTypeError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(_.y + "abc", _.x.toString)
                  .transform
                """)

            assertTypeError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(cc => haveY.y, _.x.toString)
                  .transform
                """)
          }
        }
      }
    }

    "support default parameters" should {
      case class Foo(x: Int)
      case class Bar(x: Int, y: Long = 30L)
      case class Baz(x: Int = 5, y: Long = 100L)
      case class Baah(x: Int, y: Foo = Foo(0))
      case class Baahr(x: Int, y: Bar)

      "use default parameter value" when {

        "field does not exists in the source" in {
          Foo(10).transformInto[Bar] mustBe Bar(10, 30L)
          Seq(Foo(30), Foo(40)).transformInto[Seq[Bar]] mustBe Seq(Bar(30, 30L), Bar(40, 30L))
        }

        "field does not exists in nested object" in {
          Baah(10, Foo(300)).transformInto[Baahr] mustBe
            Baahr(10, Bar(300, 30L))
        }
      }

      "not use default parameter value" when {

        "field exists in the source" in {
          Bar(100, 200L).transformInto[Baz] mustBe Baz(100, 200L)
          Seq(Bar(100, 200L), Bar(300, 400L)).transformInto[Seq[Baz]] mustBe Seq(Baz(100, 200L), Baz(300, 400L))
        }

        "another modifier is provided" in {
          Foo(10)
            .into[Bar]
            .withFieldConst(_.y, 45L)
            .transform mustBe
            Bar(10, 45L)
        }

        "default values are disabled and another modifier is provided" in {
          Foo(10)
            .into[Bar]
            .disableDefaultValues
            .withFieldConst(_.y, 45L)
            .transform mustBe
            Bar(10, 45L)

          Foo(10)
            .into[Bar]
            .withFieldConst(_.y, 48L)
            .disableDefaultValues
            .transform mustBe
            Bar(10, 48L)
        }

        "local transformer for default value exists" in {

          implicit val localTransformer: Transformer[Long, Foo] = { l: Long =>
            Foo(l.toInt * 10)
          }

          Bar(100, 300L).transformInto[Baah] mustBe Baah(100, Foo(3000))
        }

        "local transformer for the whole entity exists" in {

          implicit val fooBarTransformer: Transformer[Foo, Bar] = { foo: Foo =>
            Bar(foo.x, 333L)
          }

          Foo(333).transformInto[Bar] mustBe Bar(333, 333L)
        }
      }

      "not compile when default parameter values are disabled" in {
        assertTypeError("""
          Foo(10).into[Bar].disableDefaultValues.transform
        """)

        assertTypeError("""
          Baah(10, Foo(300)).into[Baahr].disableDefaultValues.transform
        """)
      }
    }

    "support relabelling of fields" should {

      case class Foo(x: Int, y: String)
      case class Bar(x: Int, z: String)
      case class HaveY(y: String)
      val haveY = HaveY("")
      case class HaveZ(z: String)
      val haveZ = HaveZ("")

      "not compile if relabelling modifier is not provided" in {

        assertTypeError("""Foo(10, "something").transformInto[Bar]""")
      }

      "relabel fields with relabelling modifier" in {
        Foo(10, "something")
          .into[Bar]
          .withFieldRenamed(_.y, _.z)
          .transform mustBe
          Bar(10, "something")
      }

      "not compile if relabelling selectors are invalid" in {

        assertTypeError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y + "abc", _.z)
              .transform
          """)

        assertTypeError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(cc => haveY.y, _.z)
              .transform
          """)

        assertTypeError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y, _.z + "abc")
              .transform
          """)

        assertTypeError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y, cc => haveZ.z)
              .transform
          """)

        assertTypeError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y + "abc", _.z + "abc")
              .transform
          """)

        assertTypeError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(cc => haveY.y, cc => haveZ.z)
              .transform
          """)
      }

      "not compile if relabelled in a wrong way" in {

        assertTypeError("""Foo(10, "something").into[Bar].withFieldRenamed('y, 'ne).transform""")

        assertTypeError("""Foo(10, "something").into[Bar].withFieldRenamed('ne, 'z).transform""")
      }
    }

    "support value classes" when {

      import VCDomain1._

      "transforming value class to a value" in {

        UserName("Batman").transformInto[String] mustBe "Batman"
        User("100", UserName("abc")).transformInto[UserDTO] mustBe
          UserDTO("100", "abc")
      }

      "transforming value to a value class" in {

        "Batman".transformInto[UserName] mustBe UserName("Batman")
        UserDTO("100", "abc").transformInto[User] mustBe
          User("100", UserName("abc"))
      }
    }

    "support common data types" should {

      case class Foo(value: String)
      case class Bar(value: String)

      "support scala.Option" in {
        Option(Foo("a")).transformInto[Option[Bar]] mustBe Option(Bar("a"))
        (Some(Foo("a")): Option[Foo]).transformInto[Option[Bar]] mustBe Option(Bar("a"))
        Some(Foo("a")).transformInto[Option[Bar]] mustBe Some(Bar("a"))
        (None: Option[Foo]).transformInto[Option[Bar]] mustBe None
        Some(Foo("a")).transformInto[Some[Bar]] mustBe Some(Bar("a"))
        None.transformInto[None.type] mustBe None
        (None: Option[String]).transformInto[Option[String]] mustBe None
        Option("abc").transformInto[Option[String]] mustBe Some("abc")
      }

      "support scala.util.Either" in {
        (Left(Foo("a")): Either[Foo, Foo]).transformInto[Either[Bar, Bar]] mustBe Left(Bar("a"))
        (Right(Foo("a")): Either[Foo, Foo]).transformInto[Either[Bar, Bar]] mustBe Right(Bar("a"))
        Left(Foo("a")).transformInto[Either[Bar, Bar]] mustBe Left(Bar("a"))
        Right(Foo("a")).transformInto[Either[Bar, Bar]] mustBe Right(Bar("a"))
        Left(Foo("a")).transformInto[Left[Bar, Bar]] mustBe Left(Bar("a"))
        Right(Foo("a")).transformInto[Right[Bar, Bar]] mustBe Right(Bar("a"))
        (Left("a"): Either[String, String]).transformInto[Either[String, String]] mustBe Left("a")
        (Right("a"): Either[String, String]).transformInto[Either[String, String]] mustBe Right("a")
      }

      "support Traversable collections" in {
        Seq(Foo("a")).transformInto[Seq[Bar]] mustBe Seq(Bar("a"))
        List(Foo("a")).transformInto[List[Bar]] mustBe List(Bar("a"))
        Vector(Foo("a")).transformInto[Vector[Bar]] mustBe Vector(Bar("a"))
        Set(Foo("a")).transformInto[Set[Bar]] mustBe Set(Bar("a"))

        Seq("a").transformInto[Seq[String]] mustBe Seq("a")
        List("a").transformInto[List[String]] mustBe List("a")
        Vector("a").transformInto[Vector[String]] mustBe Vector("a")
        Set("a").transformInto[Set[String]] mustBe Set("a")

        List(Foo("a")).transformInto[Seq[Bar]] mustBe Seq(Bar("a"))
        Vector(Foo("a")).transformInto[Seq[Bar]] mustBe Seq(Bar("a"))

        List("a").transformInto[Seq[String]] mustBe Seq("a")
        Vector("a").transformInto[Seq[String]] mustBe Seq("a")
      }

      "support Arrays" in {
        Array(Foo("a")).transformInto[Array[Foo]] mustBe Array(Foo("a"))
        Array(Foo("a")).transformInto[Array[Bar]] mustBe Array(Bar("a"))
        Array("a").transformInto[Array[String]] mustBe Array("a")
      }

      "support conversion between Traversables and Arrays" in {

        Array(Foo("a")).transformInto[List[Bar]] mustBe List(Bar("a"))
        Array("a", "b").transformInto[Seq[String]] mustBe Seq("a", "b")
        Array(3, 2, 1).transformInto[Vector[Int]] mustBe Vector(3, 2, 1)

        Vector("a").transformInto[Array[String]] mustBe Array("a")
        List(1, 6, 3).transformInto[Array[Int]] mustBe Array(1, 6, 3)
        Seq(Bar("x"), Bar("y")).transformInto[Array[Foo]] mustBe Array(Foo("x"), Foo("y"))
      }

      "support Map" in {
        Map("test" -> Foo("a")).transformInto[Map[String, Bar]] mustBe Map("test" -> Bar("a"))
        Map("test" -> "a").transformInto[Map[String, String]] mustBe Map("test" -> "a")
        Map(Foo("test") -> "x").transformInto[Map[Bar, String]] mustBe Map(Bar("test") -> "x")
        Map(Foo("test") -> Foo("x")).transformInto[Map[Bar, Bar]] mustBe Map(Bar("test") -> Bar("x"))
      }
    }

    "support sealed hierarchies" when {

      "enum types encoded as sealed hierarchies of case objects" when {
        "transforming from smaller to bigger enum" in {

          (colors1.Red: colors1.Color)
            .transformInto[colors2.Color] mustBe colors2.Red
          (colors1.Green: colors1.Color)
            .transformInto[colors2.Color] mustBe colors2.Green
          (colors1.Blue: colors1.Color)
            .transformInto[colors2.Color] mustBe colors2.Blue
        }

        "transforming from bigger to smaller enum" in {

          def blackIsRed(b: colors2.Black.type): colors1.Color =
            colors1.Red

          (colors2.Black: colors2.Color)
            .into[colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform mustBe colors1.Red

          (colors2.Red: colors2.Color)
            .into[colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform mustBe colors1.Red

          (colors2.Green: colors2.Color)
            .into[colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform mustBe colors1.Green

          (colors2.Blue: colors2.Color)
            .into[colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform mustBe colors1.Blue
        }
      }

      "transforming non-isomorphic domains" in {

        def triangleToPolygon(t: shapes1.Triangle): shapes2.Shape =
          shapes2.Polygon(
            List(
              t.p1.transformInto[shapes2.Point],
              t.p2.transformInto[shapes2.Point],
              t.p3.transformInto[shapes2.Point]
            )
          )

        def rectangleToPolygon(r: shapes1.Rectangle): shapes2.Shape =
          shapes2.Polygon(
            List(
              r.p1.transformInto[shapes2.Point],
              shapes2.Point(r.p1.x, r.p2.y),
              r.p2.transformInto[shapes2.Point],
              shapes2.Point(r.p2.x, r.p1.y)
            )
          )

        val triangle: shapes1.Shape =
          shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))

        triangle
          .into[shapes2.Shape]
          .withCoproductInstance(triangleToPolygon)
          .withCoproductInstance(rectangleToPolygon)
          .transform mustBe shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0)))

        val rectangle: shapes1.Shape =
          shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))

        rectangle
          .into[shapes2.Shape]
          .withCoproductInstance[shapes1.Shape] {
            case r: shapes1.Rectangle => rectangleToPolygon(r)
            case t: shapes1.Triangle  => triangleToPolygon(t)
          }
          .transform mustBe shapes2.Polygon(
          List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
        )
      }

      "transforming isomorphic domains that differ a detail" in {

        implicit val intToDoubleTransformer: Transformer[Int, Double] =
          (_: Int).toDouble

        (shapes1
          .Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0)): shapes1.Shape)
          .transformInto[shapes3.Shape] mustBe
          shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0))

        (shapes1
          .Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4)): shapes1.Shape)
          .transformInto[shapes3.Shape] mustBe
          shapes3.Rectangle(shapes3.Point(0.0, 0.0), shapes3.Point(6.0, 4.0))
      }
    }

    "support polymorphic source/target objects and modifiers" when {

      import Poly._

      "monomorphic source to polymorphic target" in {

        def transform[T]: (String => T) => MonoSource => PolyTarget[T] =
          fun => _.into[PolyTarget[T]].withFieldComputed(_.poly, src => fun(src.poly)).transform

        transform[String](identity)(monoSource) mustBe polyTarget
      }

      "polymorphic source to monomorphic target" in {

        def transform[T]: PolySource[T] => MonoTarget =
          _.into[MonoTarget].withFieldComputed(_.poly, _.poly.toString).transform

        transform[String](polySource) mustBe monoTarget
      }

      "polymorphic source to polymorphic target" in {

        def transform[T]: PolySource[T] => PolyTarget[T] =
          _.transformInto[PolyTarget[T]]

        transform[String](polySource) mustBe polyTarget
      }

      "handle type-inference for polymorphic computation" in {

        def fun[T]: PolySource[T] => String = _.poly.toString

        def transform[T]: PolySource[T] => MonoTarget =
          _.into[MonoTarget].withFieldComputed(_.poly, fun).transform

        transform[String](polySource) mustBe monoTarget
      }
    }

    "support abstracting over a value in dsl opertions" in {

      case class Foo(x: String)
      case class Bar(z: Double, y: Int, x: String)

      val partialTransformer = Foo("abc")
        .into[Bar]
        .withFieldComputed(_.y, _.x.length)

      val transformer1 = partialTransformer.withFieldConst(_.z, 1.0)
      val transformer2 = partialTransformer.withFieldComputed(_.z, _.x.length * 2.0)

      transformer1.transform mustBe Bar(1.0, 3, "abc")
      transformer2.transform mustBe Bar(6.0, 3, "abc")
    }

    "transform from non-case class to case class" when {
      import NonCaseDomain._

      "support non-case classes inputs" in {
        val source = new ClassSource("test-id", "test-name")
        val target = source.transformInto[CasesTarget]

        target.id mustBe source.id
        target.name mustBe source.name
      }

      "support trait inputs" in {
        val source: TraitSource = new TraitSourceImpl("test-id", "test-name")
        val target = source.transformInto[CasesTarget]

        target.id mustBe source.id
        target.name mustBe source.name
      }

      "support java beans" in {
        val source = new JavaBeanSource("test-id", "test-name")
        val target = source
          .into[CasesTarget]
          .withFieldRenamed(_.getId, _.id)
          .withFieldRenamed(_.getName, _.name)
          .transform

        target.id mustBe source.getId
        target.name mustBe source.getName
      }
    }

  }
}

object Domain1 {

  case class UserName(value: String)

  val userNameToStringTransformer: Transformer[UserName, String] =
    (userName: UserName) => userName.value + "T"

  case class UserDTO(id: String, name: String)

  case class User(id: String, name: UserName)

}

object VCDomain1 {

  case class UserName(value: String) extends AnyVal

  case class UserDTO(id: String, name: String)

  case class User(id: String, name: UserName)

}

object Poly {

  case class MonoSource(poly: String, other: String)

  case class PolySource[T](poly: T, other: String)

  case class MonoTarget(poly: String, other: String)

  case class PolyTarget[T](poly: T, other: String)

  val monoSource = MonoSource("test", "test")
  val polySource = PolySource("test", "test")
  val monoTarget = MonoTarget("test", "test")
  val polyTarget = PolyTarget("test", "test")
}

object NonCaseDomain {
  case class CasesTarget(val id: String, val name: String)

  class ClassSource(val id: String, val name: String)

  trait TraitSource {
    val id: String
    val name: String
  }
  class TraitSourceImpl(val id: String, val name: String) extends TraitSource

  class JavaBeanSource(id: String, name: String) {
    def getId: String = id
    def getName: String = name
  }
}
