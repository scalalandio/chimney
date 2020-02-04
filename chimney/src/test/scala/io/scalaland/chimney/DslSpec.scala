package io.scalaland.chimney

import utest._
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._

object DslSpec extends TestSuite {

  val tests = Tests {

    "use implicit transformer directly" - {

      import Domain1._

      implicit def trans: Transformer[UserName, String] = userNameToStringTransformer

      UserName("Batman").into[String].transform ==> "BatmanT"
      UserName("Batman").transformInto[String] ==> "BatmanT"
    }

    "use implicit transformer for nested field" - {

      import Domain1._

      implicit def trans: Transformer[UserName, String] = userNameToStringTransformer

      val batman = User("123", UserName("Batman"))
      val batmanDTO = batman.transformInto[UserDTO]

      batmanDTO.id ==> "123"
      batmanDTO.name ==> "BatmanT"
    }

    "support different set of fields of source and target" - {

      case class Foo(x: Int, y: String, z: (Double, Double))
      case class Bar(x: Int, z: (Double, Double))
      case class HaveY(y: String)

      "field is dropped - the target" - {
        Foo(3, "pi", (3.14, 3.14)).transformInto[Bar] ==> Bar(3, (3.14, 3.14))
      }

      "field is added to the target" - {

        "not compile if source for the target fields is not provided" - {

          compileError("Bar(3, (3.14, 3.14)).transformInto[Foo]")
            .check("", "no accessor named y in source type io.scalaland.chimney.DslSpec.Bar")
        }

        "fill the field with provided default value" - {

          "pass when selector is valid" - {

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldConst(_.y, "pi")
              .transform ==>
              Foo(3, "pi", (3.14, 3.14))

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldConst(cc => cc.y, "pi")
              .transform ==>
              Foo(3, "pi", (3.14, 3.14))
          }

          "not compile when selector is invalid" - {

            compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(_.y, "pi")
                  .withFieldConst(_.z._1, 0.0)
                  .transform
                """)
              .check("", "Invalid selector!")

            compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(_.y + "abc", "pi")
                  .transform
                """)
              .check("", "Invalid selector!")

            compileError("""
                val haveY = HaveY("")
                Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(cc => haveY.y, "pi")
                  .transform
                """)
              .check("", "Invalid selector!")
          }
        }

        "support default values for Options" - {
          case class SomeFoo(x: String)
          case class Foobar(x: String, y: Option[Int])
          case class Foobar2(x: String, y: Option[Int] = Some(42))

          "use None when .enableOptionDefaultsToNone" - {
            SomeFoo("foo").into[Foobar].enableOptionDefaultsToNone.transform ==> Foobar("foo", None)
          }

          "target has default value, but default values are disabled and .enableOptionDefaultsToNone" - {
            SomeFoo("foo").into[Foobar2].disableDefaultValues.enableOptionDefaultsToNone.transform ==>
              Foobar2("foo", None)
          }

          "not use None as default when other default value is set" - {
            SomeFoo("foo").into[Foobar2].transform ==> Foobar2("foo", Some(42))
            SomeFoo("foo").into[Foobar2].enableOptionDefaultsToNone.transform ==> Foobar2("foo", Some(42))
          }

          "not compile if default value is missing and no .enableOptionDefaultsToNone" - {
            compileError("""SomeFoo("foo").into[Foobar].transform""")
              .check("", "Chimney can't derive transformation from SomeFoo to Foobar")
          }

          "not compile if default values are disabled and no .enableOptionDefaultsToNone" - {
            compileError("""SomeFoo("foo").into[Foobar2].disableDefaultValues.transform""")
              .check("", "Chimney can't derive transformation from SomeFoo to Foobar2")
          }
        }

        "use implicit transformer for option when .enableUnsafeOption" - {
          case class Foobar(x: Option[Int])
          case class Foobar2(x: String)

          implicit val stringToIntTransformer: Transformer[Int, String] = _.toString

          "use transformer when .enableUnsafeOption" - {
            Foobar(Some(1)).into[Foobar2].enableUnsafeOption.transform ==> Foobar2("1")
          }
        }

        "fill the field with provided generator function" - {

          "pass when selector is valid" - {

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldComputed(_.y, _.x.toString)
              .transform ==>
              Foo(3, "3", (3.14, 3.14))

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldComputed(cc => cc.y, _.x.toString)
              .transform ==>
              Foo(3, "3", (3.14, 3.14))
          }

          "not compile when selector is invalid" - {

            compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(_.y, _.x.toString)
                  .withFieldComputed(_.z._1, _.z._1 * 10.0)
                  .transform
                """)
              .check("", "Invalid selector!")

            compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(_.y + "abc", _.x.toString)
                  .transform
                """)
              .check("", "Invalid selector!")

            compileError("""
                val haveY = HaveY("")
                Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(cc => haveY.y, _.x.toString)
                  .transform
                """)
              .check("", "Invalid selector!")
          }
        }
      }
    }

    "support default parameters" - {
      case class Foo(x: Int)
      case class Bar(x: Int, y: Long = 30L)
      case class Baz(x: Int = 5, y: Long = 100L)
      case class Baah(x: Int, y: Foo = Foo(0))
      case class Baahr(x: Int, y: Bar)

      "use default parameter value" - {

        "field does not exists - the source" - {
          Foo(10).transformInto[Bar] ==> Bar(10, 30L)
          Seq(Foo(30), Foo(40)).transformInto[Seq[Bar]] ==> Seq(Bar(30, 30L), Bar(40, 30L))
        }

        "field does not exists - nested object" - {
          Baah(10, Foo(300)).transformInto[Baahr] ==>
            Baahr(10, Bar(300, 30L))
        }
      }

      "not use default parameter value" - {

        "field exists - the source" - {
          Bar(100, 200L).transformInto[Baz] ==> Baz(100, 200L)
          Seq(Bar(100, 200L), Bar(300, 400L)).transformInto[Seq[Baz]] ==> Seq(Baz(100, 200L), Baz(300, 400L))
        }

        "another modifier is provided" - {
          Foo(10)
            .into[Bar]
            .withFieldConst(_.y, 45L)
            .transform ==>
            Bar(10, 45L)
        }

        "default values are disabled and another modifier is provided" - {
          Foo(10)
            .into[Bar]
            .disableDefaultValues
            .withFieldConst(_.y, 45L)
            .transform ==>
            Bar(10, 45L)

          Foo(10)
            .into[Bar]
            .withFieldConst(_.y, 48L)
            .disableDefaultValues
            .transform ==>
            Bar(10, 48L)
        }

        "local transformer for default value exists" - {

          implicit val localTransformer: Transformer[Long, Foo] = { l: Long =>
            Foo(l.toInt * 10)
          }

          Bar(100, 300L).transformInto[Baah] ==> Baah(100, Foo(3000))
        }

        "local transformer for the whole entity exists" - {

          implicit val fooBarTransformer: Transformer[Foo, Bar] = { foo: Foo =>
            Bar(foo.x, 333L)
          }

          Foo(333).transformInto[Bar] ==> Bar(333, 333L)
        }
      }

      "not compile when default parameter values are disabled" - {
        compileError("""
          Foo(10).into[Bar].disableDefaultValues.transform
        """)
          .check("", "Chimney can't derive transformation from Foo to Bar")

        compileError("""
          Baah(10, Foo(300)).into[Baahr].disableDefaultValues.transform
        """)
          .check("", "Chimney can't derive transformation from Baah to Baahr")
      }
    }

    "transform with rename" - {
      case class User(id: Int, name: String, age: Option[Int])
      case class UserPL(id: Int, imie: String, wiek: Either[Unit, Int])
      def ageToWiekTransformer: Transformer[Option[Int], Either[Unit, Int]] =
        new Transformer[Option[Int], Either[Unit, Int]] {
          def transform(obj: Option[Int]): Either[Unit, Int] =
            obj.fold[Either[Unit, Int]](Left(()))(Right.apply)
        }

      "between different types: correct" - {
        implicit def trans: Transformer[Option[Int], Either[Unit, Int]] = ageToWiekTransformer

        val user: User = User(1, "Kuba", Some(28))
        val userPl = UserPL(1, "Kuba", Right(28))
        user
          .into[UserPL]
          .withFieldRenamed(_.name, _.imie)
          .withFieldRenamed(_.age, _.wiek)
          .transform ==> userPl

      }

      "between different types: incorrect" - {
        implicit def trans: Transformer[Option[Int], Either[Unit, Int]] = ageToWiekTransformer

        val user: User = User(1, "Kuba", None)
        val userPl = UserPL(1, "Kuba", Left(()))
        user
          .into[UserPL]
          .withFieldRenamed(_.name, _.imie)
          .withFieldRenamed(_.age, _.wiek)
          .transform ==> userPl

      }

      "between different types: without implicit" - {
        compileError("""
            val user: User = User(1, "Kuba", None)
            user.into[UserPL].withFieldRenamed(_.name, _.imie)
                .withFieldRenamed(_.age, _.wiek)
                .transform
          """)
          .check("", "Chimney can't derive transformation from User to UserPL")
      }
    }

    "support relabelling of fields" - {

      case class Foo(x: Int, y: String)
      case class Bar(x: Int, z: String)
      case class HaveY(y: String)
      case class HaveZ(z: String)

      "not compile if relabelling modifier is not provided" - {

        compileError("""Foo(10, "something").transformInto[Bar]""")
          .check("", "Chimney can't derive transformation from Foo to Bar")
      }

      "relabel fields with relabelling modifier" - {
        Foo(10, "something")
          .into[Bar]
          .withFieldRenamed(_.y, _.z)
          .transform ==>
          Bar(10, "something")
      }

      "not compile if relabelling selectors are invalid" - {

        compileError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y + "abc", _.z)
              .transform
          """)
          .check("", "Selector of type Foo => String is not valid")

        compileError("""
            val haveY = HaveY("")
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(cc => haveY.y, _.z)
              .transform
          """)
          .check("", "Selector of type Foo => String is not valid")

        compileError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y, _.z + "abc")
              .transform
          """)
          .check("", "Selector of type Bar => String is not valid")

        compileError("""
            val haveZ = HaveZ("")
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y, cc => haveZ.z)
              .transform
          """)
          .check("", "Selector of type Bar => String is not valid")

        compileError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y + "abc", _.z + "abc")
              .transform
          """)
          .check("", "Selector of type Bar => String is not valid")

        compileError("""
            val haveY = HaveY("")
            val haveZ = HaveZ("")
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(cc => haveY.y, cc => haveZ.z)
              .transform
          """)
          .check("", "Selector of type Foo => String is not valid")
      }

      "not compile if relabelled - a wrong way" - {

        compileError("""Foo(10, "something").into[Bar].withFieldRenamed('y, 'ne).transform""")
          .check("", "type mismatch")

        compileError("""Foo(10, "something").into[Bar].withFieldRenamed('ne, 'z).transform""")
          .check("", "type mismatch")
      }

    }

    "support value classes" - {

      import VCDomain1._

      "transforming value class to a value" - {

        UserName("Batman").transformInto[String] ==> "Batman"
        User("100", UserName("abc")).transformInto[UserDTO] ==>
          UserDTO("100", "abc")
      }

      "transforming value to a value class" - {

        "Batman".transformInto[UserName] ==> UserName("Batman")
        UserDTO("100", "abc").transformInto[User] ==>
          User("100", UserName("abc"))
      }
    }

    "support common data types" - {

      case class Foo(value: String)
      case class Bar(value: String)

      "support scala.Option" - {
        Option(Foo("a")).transformInto[Option[Bar]] ==> Option(Bar("a"))
        (Some(Foo("a")): Option[Foo]).transformInto[Option[Bar]] ==> Option(Bar("a"))
        Some(Foo("a")).transformInto[Option[Bar]] ==> Some(Bar("a"))
        (None: Option[Foo]).transformInto[Option[Bar]] ==> None
        Some(Foo("a")).transformInto[Some[Bar]] ==> Some(Bar("a"))
        None.transformInto[None.type] ==> None
        (None: Option[String]).transformInto[Option[String]] ==> None
        Option("abc").transformInto[Option[String]] ==> Some("abc")
        compileError(""""Some(foobar)".into[None.type].transform""")
          .check("", "derivation from string: java.lang.String to scala.None is not supported in Chimney!")
        case class BarNone(value: None.type)
        compileError("""Foo("a").into[BarNone].transform""")
          .check("", "derivation from foo.value: java.lang.String to scala.None is not supported in Chimney!")
      }

      "support automatically filling of scala.Unit" - {
        case class Buzz(value: String)
        case class NewBuzz(value: String, unit: Unit)
        case class FooBuzz(unit: Unit)
        case class ConflictingFooBuzz(value: Unit)

        Buzz("a").transformInto[NewBuzz] ==> NewBuzz("a", ())
        Buzz("a").transformInto[FooBuzz] ==> FooBuzz(())
        NewBuzz("a", null: Unit).transformInto[FooBuzz] ==> FooBuzz(null: Unit)
        compileError("""Buzz("a").transformInto[ConflictingFooBuzz]""")
          .check("", "value: scala.Unit - can't derive transformation from value: java.lang.String in source type io.scalaland.chimney.DslSpec.Buzz")
      }

      "support scala.util.Either" - {
        (Left(Foo("a")): Either[Foo, Foo]).transformInto[Either[Bar, Bar]] ==> Left(Bar("a"))
        (Right(Foo("a")): Either[Foo, Foo]).transformInto[Either[Bar, Bar]] ==> Right(Bar("a"))
        Left(Foo("a")).transformInto[Either[Bar, Bar]] ==> Left(Bar("a"))
        Right(Foo("a")).transformInto[Either[Bar, Bar]] ==> Right(Bar("a"))
        Left(Foo("a")).transformInto[Left[Bar, Bar]] ==> Left(Bar("a"))
        Right(Foo("a")).transformInto[Right[Bar, Bar]] ==> Right(Bar("a"))
        (Left("a"): Either[String, String]).transformInto[Either[String, String]] ==> Left("a")
        (Right("a"): Either[String, String]).transformInto[Either[String, String]] ==> Right("a")
      }

      "support Traversable collections" - {
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

      "support Arrays" - {
        Array(Foo("a")).transformInto[Array[Foo]] ==> Array(Foo("a"))
        Array(Foo("a")).transformInto[Array[Bar]] ==> Array(Bar("a"))
        Array("a").transformInto[Array[String]] ==> Array("a")
      }

      "support conversion between Traversables and Arrays" - {

        Array(Foo("a")).transformInto[List[Bar]] ==> List(Bar("a"))
        Array("a", "b").transformInto[Seq[String]] ==> Seq("a", "b")
        Array(3, 2, 1).transformInto[Vector[Int]] ==> Vector(3, 2, 1)

        Vector("a").transformInto[Array[String]] ==> Array("a")
        List(1, 6, 3).transformInto[Array[Int]] ==> Array(1, 6, 3)
        Seq(Bar("x"), Bar("y")).transformInto[Array[Foo]] ==> Array(Foo("x"), Foo("y"))
      }

      "support Map" - {
        Map("test" -> Foo("a")).transformInto[Map[String, Bar]] ==> Map("test" -> Bar("a"))
        Map("test" -> "a").transformInto[Map[String, String]] ==> Map("test" -> "a")
        Map(Foo("test") -> "x").transformInto[Map[Bar, String]] ==> Map(Bar("test") -> "x")
        Map(Foo("test") -> Foo("x")).transformInto[Map[Bar, Bar]] ==> Map(Bar("test") -> Bar("x"))
      }
    }

    "support with .enableUnsafeOption" - {
      implicit val stringToIntTransformer: Transformer[Int, String] = _.toString

      "use implicit transformer" - {
        case class Foobar(x: Option[Int])
        case class Foobar2(x: String)

        case class NestedFoobar(foobar: Option[Foobar])
        case class NestedFoobar2(foobar: Foobar2)

        Foobar(Some(1)).into[Foobar2].enableUnsafeOption.transform ==> Foobar2("1")
        NestedFoobar(Some(Foobar(Some(1)))).into[NestedFoobar2].enableUnsafeOption.transform ==> NestedFoobar2(
          Foobar2("1")
        )
      }

      "preserve option to option mapping" - {
        case class Foobar(x: Option[Int], y: Option[String])
        case class Foobar2(x: String, y: Option[String])

        Foobar(Some(1), Some("foobar")).into[Foobar2].enableUnsafeOption.transform ==> Foobar2("1", Some("foobar"))
        Foobar(Some(1), None).into[Foobar2].enableUnsafeOption.transform ==> Foobar2("1", None)
      }

      "transforming None leads to NoSuchElementException" - {
        case class Foobar(x: Option[Int])
        case class Foobar2(x: String)

        intercept[NoSuchElementException] {
          Foobar(None).into[Foobar2].enableUnsafeOption.transform
        }
      }

      "transforming fixed None type does not compile" - {
        compileError("""None.into[String].enableUnsafeOption.transform""")
          .check("", "derivation from none: scala.None to java.lang.String is not supported in Chimney!")

        case class Foobar(x: None.type)
        case class Foobar2(x: String)
        compileError("""Foobar(None).into[Foobar2].enableUnsafeOption.transform""")
          .check("", "derivation from foobar.x: scala.None to java.lang.String is not supported in Chimney!")
      }
    }

    "support sealed hierarchies" - {

      "enum types encoded as sealed hierarchies of case objects" - {
        "transforming from smaller to bigger enum" - {

          (colors1.Red: colors1.Color)
            .transformInto[colors2.Color] ==> colors2.Red
          (colors1.Green: colors1.Color)
            .transformInto[colors2.Color] ==> colors2.Green
          (colors1.Blue: colors1.Color)
            .transformInto[colors2.Color] ==> colors2.Blue
        }

        "transforming from bigger to smaller enum" - {

          def blackIsRed(b: colors2.Black.type): colors1.Color =
            colors1.Red

          (colors2.Black: colors2.Color)
            .into[colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> colors1.Red

          (colors2.Red: colors2.Color)
            .into[colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> colors1.Red

          (colors2.Green: colors2.Color)
            .into[colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> colors1.Green

          (colors2.Blue: colors2.Color)
            .into[colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> colors1.Blue
        }
      }

      "transforming non-isomorphic domains" - {

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
          .transform ==> shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0)))

        val rectangle: shapes1.Shape =
          shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))

        rectangle
          .into[shapes2.Shape]
          .withCoproductInstance[shapes1.Shape] {
            case r: shapes1.Rectangle => rectangleToPolygon(r)
            case t: shapes1.Triangle  => triangleToPolygon(t)
          }
          .transform ==> shapes2.Polygon(
          List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
        )
      }

      "transforming isomorphic domains that differ a detail" - {

        implicit val intToDoubleTransformer: Transformer[Int, Double] =
          (_: Int).toDouble

        (shapes1
          .Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0)): shapes1.Shape)
          .transformInto[shapes3.Shape] ==>
          shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0))

        (shapes1
          .Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4)): shapes1.Shape)
          .transformInto[shapes3.Shape] ==>
          shapes3.Rectangle(shapes3.Point(0.0, 0.0), shapes3.Point(6.0, 4.0))
      }
    }

    "support polymorphic source/target objects and modifiers" - {

      import Poly._

      "monomorphic source to polymorphic target" - {

        monoSource.transformInto[PolyTarget[String]] ==> polyTarget

        def transform[T]: (String => T) => MonoSource => PolyTarget[T] =
          fun => _.into[PolyTarget[T]].withFieldComputed(_.poly, src => fun(src.poly)).transform

        transform[String](identity)(monoSource) ==> polyTarget
      }

      "polymorphic source to monomorphic target" - {

        def transform[T]: PolySource[T] => MonoTarget =
          _.into[MonoTarget].withFieldComputed(_.poly, _.poly.toString).transform

        transform[String](polySource) ==> monoTarget
      }

      "polymorphic source to polymorphic target" - {

        def transform[T]: PolySource[T] => PolyTarget[T] =
          _.transformInto[PolyTarget[T]]

        transform[String](polySource) ==> polyTarget
      }

      "handle type-inference for polymorphic computation" - {

        def fun[T]: PolySource[T] => String = _.poly.toString

        def transform[T]: PolySource[T] => MonoTarget =
          _.into[MonoTarget].withFieldComputed(_.poly, fun).transform

        transform[String](polySource) ==> monoTarget
      }

      "automatically fill Unit parameters" - {
        case class Foo(value: String)
        case class Bar[T](value: String, poly: T)
        type UnitBar = Bar[Unit]

        Foo("test").transformInto[UnitBar] ==> Bar("test", ())
        Foo("test").transformInto[Bar[Unit]] ==> Bar("test", ())
      }
    }

    "support abstracting over a value in dsl operations" - {

      case class Foo(x: String)
      case class Bar(z: Double, y: Int, x: String)

      val partialTransformer = Foo("abc")
        .into[Bar]
        .withFieldComputed(_.y, _.x.length)

      val transformer1 = partialTransformer.withFieldConst(_.z, 1.0)
      val transformer2 = partialTransformer.withFieldComputed(_.z, _.x.length * 2.0)

      transformer1.transform ==> Bar(1.0, 3, "abc")
      transformer2.transform ==> Bar(6.0, 3, "abc")
    }

    "transform from non-case class to case class" - {
      import NonCaseDomain._

      "support non-case classes inputs" - {
        val source = new ClassSource("test-id", "test-name")
        val target = source.transformInto[CaseClassNoFlag]

        target.id ==> source.id
        target.name ==> source.name
      }

      "support trait inputs" - {
        val source: TraitSource = new TraitSourceImpl("test-id", "test-name")
        val target = source.transformInto[CaseClassNoFlag]

        target.id ==> source.id
        target.name ==> source.name
      }
    }

    "transform T to Option[T]" - {

      "abc".transformInto[Option[String]] ==> Some("abc")
      (null: String).transformInto[Option[String]] ==> None
    }

    "transform between case classes and tuples" - {

      case class Foo(field1: Int, field2: Double, field3: String)

      val expected = (0, 3.14, "pi")

      Foo(0, 3.14, "pi")
        .transformInto[(Int, Double, String)] ==> expected

      (0, 3.14, "pi").transformInto[Foo]

      "even recursively" - {

        case class Bar(foo: Foo, baz: Boolean)

        val expected = ((100, 2.71, "e"), false)

        Bar(Foo(100, 2.71, "e"), baz = false)
          .transformInto[((Int, Double, String), Boolean)] ==> expected

        ((100, 2.71, "e"), true).transformInto[Bar] ==>
          Bar(Foo(100, 2.71, "e"), baz = true)
      }

      "handle tuple transformation errors" - {

        compileError("""
          (0, "test").transformInto[Foo]
        """)
          .check(
            "",
            "source tuple scala.Tuple2 is of arity 2, while target type io.scalaland.chimney.DslSpec.Foo is of arity 3; they need to be equal!"
          )

        compileError("""
          (10.5, "abc", 6).transformInto[Foo]
        """)
          .check("", "can't derive transformation")

        compileError("""
          Foo(10, 36.6, "test").transformInto[(Double, String, Int, Float, Boolean)]
        """)
          .check(
            "",
            "source tuple io.scalaland.chimney.DslSpec.Foo is of arity 3, while target type scala.Tuple5 is of arity 5; they need to be equal!"
          )

        compileError("""
          Foo(10, 36.6, "test").transformInto[(Int, Double, Boolean)]
        """)
          .check("", "can't derive transformation")
      }
    }

    "support recursive data structures" - {

      case class Foo(x: Option[Foo])
      case class Bar(x: Option[Bar])

      "defined by hand" - {
        implicit def fooToBarTransformer: Transformer[Foo, Bar] = (foo: Foo) => {
          Bar(foo.x.map(fooToBarTransformer.transform))
        }

        Foo(Some(Foo(None))).transformInto[Bar] ==> Bar(Some(Bar(None)))
      }

      "generated automatically" - {
        implicit def fooToBarTransformer: Transformer[Foo, Bar] = Transformer.derive[Foo, Bar]

        Foo(Some(Foo(None))).transformInto[Bar] ==> Bar(Some(Bar(None)))
      }

      "support mutual recursion" - {

        case class Baz[T](bar: Option[T])
        case class Bar1(x: Int, foo: Baz[Bar1])
        case class Bar2(foo: Baz[Bar2])

        implicit def bar1ToBar2Transformer: Transformer[Bar1, Bar2] = Transformer.derive[Bar1, Bar2]

        Bar1(1, Baz(Some(Bar1(2, Baz(None))))).transformInto[Bar2] ==> Bar2(Baz(Some(Bar2(Baz(None)))))
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

  class ClassSource(val id: String, val name: String)

  trait TraitSource {
    val id: String
    val name: String
  }

  class TraitSourceImpl(val id: String, val name: String) extends TraitSource
}
