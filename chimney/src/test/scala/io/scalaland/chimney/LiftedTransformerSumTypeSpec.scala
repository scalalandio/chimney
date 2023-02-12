package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.utils.EitherUtils._
import io.scalaland.chimney.utils.OptionUtils._
import utest._

object LiftedTransformerSumTypeSpec extends TestSuite {

  val tests = Tests {

    test(
      """transform flat sealed hierarchies from "subset" of case objects to "superset" of case objects without modifiers"""
    ) {

      test("when F = Option") {
        (colors1.Red: colors1.Color).transformIntoF[Option, colors2.Color] ==> Some(colors2.Red)
        (colors1.Green: colors1.Color).transformIntoF[Option, colors2.Color] ==> Some(colors2.Green)
        (colors1.Blue: colors1.Color).transformIntoF[Option, colors2.Color] ==> Some(colors2.Blue)
      }

      test("when F = Either[List[String], +*]") {
        (colors1.Red: colors1.Color).transformIntoF[Either[List[String], +*], colors2.Color] ==> Right(colors2.Red)
        (colors1.Green: colors1.Color).transformIntoF[Either[List[String], +*], colors2.Color] ==> Right(colors2.Green)
        (colors1.Blue: colors1.Color).transformIntoF[Either[List[String], +*], colors2.Color] ==> Right(colors2.Blue)
      }
    }

    test(
      """transform nested sealed hierarchies between flat and nested hierarchies of case objects without modifiers"""
    ) {

      test("when F = Option") {
        (colors2.Red: colors2.Color).transformIntoF[Option, colors3.Color] ==> Some(colors3.Red)
        (colors2.Green: colors2.Color).transformIntoF[Option, colors3.Color] ==> Some(colors3.Green)
        (colors2.Blue: colors2.Color).transformIntoF[Option, colors3.Color] ==> Some(colors3.Blue)
        (colors2.Black: colors2.Color).transformIntoF[Option, colors3.Color] ==> Some(colors3.Black)

        (colors3.Red: colors3.Color).transformIntoF[Option, colors2.Color] ==> Some(colors2.Red)
        (colors3.Green: colors3.Color).transformIntoF[Option, colors2.Color] ==> Some(colors2.Green)
        (colors3.Blue: colors3.Color).transformIntoF[Option, colors2.Color] ==> Some(colors2.Blue)
        (colors3.Black: colors3.Color).transformIntoF[Option, colors2.Color] ==> Some(colors2.Black)
      }

      test("when F = Either[List[String], +*]") {
        (colors2.Red: colors2.Color).transformIntoF[Either[List[String], +*], colors3.Color] ==> Right(colors3.Red)
        (colors2.Green: colors2.Color).transformIntoF[Either[List[String], +*], colors3.Color] ==> Right(colors3.Green)
        (colors2.Blue: colors2.Color).transformIntoF[Either[List[String], +*], colors3.Color] ==> Right(colors3.Blue)
        (colors2.Black: colors2.Color).transformIntoF[Either[List[String], +*], colors3.Color] ==> Right(colors3.Black)

        (colors3.Red: colors3.Color).transformIntoF[Either[List[String], +*], colors2.Color] ==> Right(colors2.Red)
        (colors3.Green: colors3.Color).transformIntoF[Either[List[String], +*], colors2.Color] ==> Right(colors2.Green)
        (colors3.Blue: colors3.Color).transformIntoF[Either[List[String], +*], colors2.Color] ==> Right(colors2.Blue)
        (colors3.Black: colors3.Color).transformIntoF[Either[List[String], +*], colors2.Color] ==> Right(colors2.Black)
      }
    }

    test(
      """transforming flat hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable with Total Transformer"""
    ) {
      implicit val intToDoubleTransformer: Transformer[Int, Double] = (_: Int).toDouble

      implicit val intPrinter: Transformer[Int, String] = _.toString

      test("when F = Option") {
        (shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0)): shapes1.Shape)
          .transformIntoF[Option, shapes3.Shape] ==>
          Some(shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)))
        (shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4)): shapes1.Shape)
          .transformIntoF[Option, shapes3.Shape] ==>
          Some(shapes3.Rectangle(shapes3.Point(0.0, 0.0), shapes3.Point(6.0, 4.0)))

        import numbers._, ScalesTransformerF.shortToLongPureInner

        (short.Zero: short.NumScale[Int, Nothing])
          .transformIntoF[Option, long.NumScale[String]] ==> Some(long.Zero)
        (short.Million(4): short.NumScale[Int, Nothing])
          .transformIntoF[Option, long.NumScale[String]] ==> Some(long.Million("4"))
        (short.Billion(2): short.NumScale[Int, Nothing])
          .transformIntoF[Option, long.NumScale[String]] ==> Some(long.Milliard("2"))
        (short.Trillion(100): short.NumScale[Int, Nothing])
          .transformIntoF[Option, long.NumScale[String]] ==> Some(long.Billion("100"))
      }

      test("when F = Either[List[String], +*]") {
        (shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0)): shapes1.Shape)
          .transformIntoF[Either[List[String], +*], shapes3.Shape] ==>
          Right(shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)))
        (shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4)): shapes1.Shape)
          .transformIntoF[Either[List[String], +*], shapes3.Shape] ==>
          Right(shapes3.Rectangle(shapes3.Point(0.0, 0.0), shapes3.Point(6.0, 4.0)))

        import numbers._, ScalesTransformerF.shortToLongPureInner

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

    test(
      """transforming flat hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable with Partial Transformer"""
    ) {

      test("when F = Option") {
        implicit val intToDoubleTransformer: TransformerF[Option, Int, Double] = (a: Int) => Option(a.toDouble)

        (shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0)): shapes1.Shape)
          .transformIntoF[Option, shapes3.Shape] ==>
          Some(shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)))
        (shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4)): shapes1.Shape)
          .transformIntoF[Option, shapes3.Shape] ==>
          Some(shapes3.Rectangle(shapes3.Point(0.0, 0.0), shapes3.Point(6.0, 4.0)))

        implicit val intParserOpt: TransformerF[Option, String, Int] = _.parseInt
        import numbers._, ScalesTransformerF.shortToLongWrappedInner

        (short.Zero: short.NumScale[String, Nothing])
          .transformIntoF[Option, long.NumScale[Int]] ==> Some(long.Zero)
        (short.Million("4"): short.NumScale[String, Nothing])
          .transformIntoF[Option, long.NumScale[Int]] ==> Some(long.Million(4))
        (short.Billion("2"): short.NumScale[String, Nothing])
          .transformIntoF[Option, long.NumScale[Int]] ==> Some(long.Milliard(2))
        (short.Trillion("100"): short.NumScale[String, Nothing])
          .transformIntoF[Option, long.NumScale[Int]] ==> Some(long.Billion(100))

        (short.Million("x"): short.NumScale[String, Nothing]).transformIntoF[Option, long.NumScale[Int]] ==> None
        (short.Billion("x"): short.NumScale[String, Nothing]).transformIntoF[Option, long.NumScale[Int]] ==> None
        (short.Trillion("x"): short.NumScale[String, Nothing]).transformIntoF[Option, long.NumScale[Int]] ==> None
      }

      test("when F = Either[List[String], +*]") {
        implicit val intToDoubleTransformer: TransformerF[Either[List[String], +*], Int, Double] =
          (a: Int) => Right(a.toDouble)

        (shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0)): shapes1.Shape)
          .transformIntoF[Either[List[String], +*], shapes3.Shape] ==>
          Right(shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)))
        (shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4)): shapes1.Shape)
          .transformIntoF[Either[List[String], +*], shapes3.Shape] ==>
          Right(shapes3.Rectangle(shapes3.Point(0.0, 0.0), shapes3.Point(6.0, 4.0)))

        implicit val intParserEither: TransformerF[Either[List[String], +*], String, Int] =
          _.parseInt.toEitherList("bad int")
        import numbers._, ScalesTransformerF.shortToLongWrappedInner

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

    test(
      """transforming nested sealed hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable"""
    ) {

      test("when F = Option") {
        (shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)): shapes3.Shape)
          .transformIntoF[Option, shapes4.Shape] ==>
          Some(shapes4.Triangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0), shapes4.Point(0.0, 0.0)))
        (shapes3.Rectangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0)): shapes3.Shape)
          .transformIntoF[Option, shapes4.Shape] ==>
          Some(shapes4.Rectangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0)))
        (shapes4.Triangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0), shapes4.Point(0.0, 0.0)): shapes4.Shape)
          .transformIntoF[Option, shapes3.Shape] ==>
          Some(shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)))
        (shapes4.Rectangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0)): shapes4.Shape)
          .transformIntoF[Option, shapes3.Shape] ==>
          Some(shapes3.Rectangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0)))
      }

      test("when F = Either[List[String], +*]") {
        (shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)): shapes3.Shape)
          .transformIntoF[Either[List[String], +*], shapes4.Shape] ==>
          Right(shapes4.Triangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0), shapes4.Point(0.0, 0.0)))
        (shapes3.Rectangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0)): shapes3.Shape)
          .transformIntoF[Either[List[String], +*], shapes4.Shape] ==>
          Right(shapes4.Rectangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0)))
        (shapes4.Triangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0), shapes4.Point(0.0, 0.0)): shapes4.Shape)
          .transformIntoF[Either[List[String], +*], shapes3.Shape] ==>
          Right(shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)))
        (shapes4.Rectangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0)): shapes4.Shape)
          .transformIntoF[Either[List[String], +*], shapes3.Shape] ==>
          Right(shapes3.Rectangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0)))
      }
    }

    test("setting .withCoproductInstance[Subtype](mapping)") {

      test(
        """should be absent by default and not allow transforming "superset" of case class to "subset" of case objects"""
      ) {

        test("when F = Option") {
          compileError("""(colors2.Black: colors2.Color).transformIntoF[Option, colors1.Color]""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.colors2.Color to io.scalaland.chimney.examples.colors1.Color",
            "io.scalaland.chimney.examples.colors1.Color",
            "can't transform coproduct instance io.scalaland.chimney.examples.colors2.Black to io.scalaland.chimney.examples.colors1.Color",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
          compileError("""(colors2.Black: colors2.Color).transformIntoF[EitherList, colors1.Color]""").check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.colors2.Color to io.scalaland.chimney.examples.colors1.Color",
            "io.scalaland.chimney.examples.colors1.Color",
            "can't transform coproduct instance io.scalaland.chimney.examples.colors2.Black to io.scalaland.chimney.examples.colors1.Color",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
        }
      }

      test(
        """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
      ) {

        def blackIsRed(b: colors2.Black.type): colors1.Color =
          colors1.Red

        test("when F = Option") {
          (colors2.Black: colors2.Color)
            .intoF[Option, colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> Some(colors1.Red)

          (colors2.Red: colors2.Color)
            .intoF[Option, colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> Some(colors1.Red)

          (colors2.Green: colors2.Color)
            .intoF[Option, colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> Some(colors1.Green)

          (colors2.Blue: colors2.Color)
            .intoF[Option, colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> Some(colors1.Blue)
        }

        test("when F = Either[List[String], +*]") {
          (colors2.Black: colors2.Color)
            .intoF[Either[List[String], +*], colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> Right(colors1.Red)

          (colors2.Red: colors2.Color)
            .intoF[Either[List[String], +*], colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> Right(colors1.Red)

          (colors2.Green: colors2.Color)
            .intoF[Either[List[String], +*], colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> Right(colors1.Green)

          (colors2.Blue: colors2.Color)
            .intoF[Either[List[String], +*], colors1.Color]
            .withCoproductInstance(blackIsRed)
            .transform ==> Right(colors1.Blue)
        }
      }

      test(
        """transform sealed hierarchies from "superset" of case classes to "subset" of case classes when user-provided mapping handled non-trivial cases"""
      ) {

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

        val rectangle: shapes1.Shape =
          shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))

        test("when F = Option") {
          triangle
            .intoF[Option, shapes2.Shape]
            .withCoproductInstance(triangleToPolygon)
            .withCoproductInstance(rectangleToPolygon)
            .transform ==> Some(shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0))))

          rectangle
            .intoF[Option, shapes2.Shape]
            .withCoproductInstance[shapes1.Shape] {
              case r: shapes1.Rectangle => rectangleToPolygon(r)
              case t: shapes1.Triangle  => triangleToPolygon(t)
            }
            .transform ==> Some(
            shapes2.Polygon(
              List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
            )
          )
        }

        test("when F = Either[List[String], +*]") {
          triangle
            .intoF[Either[List[String], +*], shapes2.Shape]
            .withCoproductInstance(triangleToPolygon)
            .withCoproductInstance(rectangleToPolygon)
            .transform ==> Right(shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0))))

          rectangle
            .intoF[Either[List[String], +*], shapes2.Shape]
            .withCoproductInstance[shapes1.Shape] {
              case r: shapes1.Rectangle => rectangleToPolygon(r)
              case t: shapes1.Triangle  => triangleToPolygon(t)
            }
            .transform ==> Right(
            shapes2.Polygon(
              List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
            )
          )
        }
      }
    }

    test("setting .withCoproductInstanceF(mapping)") {

      test(
        """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
      ) {

        test("when F = Option") {
          def blackIsRed(b: colors2.Black.type): Option[colors1.Color] = None

          (colors2.Black: colors2.Color)
            .intoF[Option, colors1.Color]
            .withCoproductInstanceF(blackIsRed)
            .transform ==> None

          (colors2.Red: colors2.Color)
            .intoF[Option, colors1.Color]
            .withCoproductInstanceF(blackIsRed)
            .transform ==> Some(colors1.Red)

          (colors2.Green: colors2.Color)
            .intoF[Option, colors1.Color]
            .withCoproductInstanceF(blackIsRed)
            .transform ==> Some(colors1.Green)

          (colors2.Blue: colors2.Color)
            .intoF[Option, colors1.Color]
            .withCoproductInstanceF(blackIsRed)
            .transform ==> Some(colors1.Blue)
        }

        test("when F = Either[List[String], +*]") {
          def blackIsRed(b: colors2.Black.type): Either[List[String], colors1.Color] = Left(List("bad color"))

          (colors2.Black: colors2.Color)
            .intoF[Either[List[String], +*], colors1.Color]
            .withCoproductInstanceF(blackIsRed)
            .transform ==> Left(List("bad color"))

          (colors2.Red: colors2.Color)
            .intoF[Either[List[String], +*], colors1.Color]
            .withCoproductInstanceF(blackIsRed)
            .transform ==> Right(colors1.Red)

          (colors2.Green: colors2.Color)
            .intoF[Either[List[String], +*], colors1.Color]
            .withCoproductInstanceF(blackIsRed)
            .transform ==> Right(colors1.Green)

          (colors2.Blue: colors2.Color)
            .intoF[Either[List[String], +*], colors1.Color]
            .withCoproductInstanceF(blackIsRed)
            .transform ==> Right(colors1.Blue)
        }
      }

      test(
        """transform sealed hierarchies from "superset" of case classes to "subset" of case classes when user-provided mapping handled non-trivial cases"""
      ) {

        val triangle: shapes1.Shape =
          shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))

        val rectangle: shapes1.Shape =
          shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))

        test("when F = Option") {
          def triangleToPolygon(t: shapes1.Triangle): Option[shapes2.Shape] =
            Some(
              shapes2.Polygon(
                List(
                  t.p1.transformInto[shapes2.Point],
                  t.p2.transformInto[shapes2.Point],
                  t.p3.transformInto[shapes2.Point]
                )
              )
            )

          def rectangleToPolygon(r: shapes1.Rectangle): Option[shapes2.Shape] =
            Some(
              shapes2.Polygon(
                List(
                  r.p1.transformInto[shapes2.Point],
                  shapes2.Point(r.p1.x, r.p2.y),
                  r.p2.transformInto[shapes2.Point],
                  shapes2.Point(r.p2.x, r.p1.y)
                )
              )
            )

          triangle
            .intoF[Option, shapes2.Shape]
            .withCoproductInstanceF(triangleToPolygon)
            .withCoproductInstanceF(rectangleToPolygon)
            .transform ==> Some(shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0))))

          rectangle
            .intoF[Option, shapes2.Shape]
            .withCoproductInstanceF[shapes1.Shape] {
              case r: shapes1.Rectangle => rectangleToPolygon(r)
              case t: shapes1.Triangle  => triangleToPolygon(t)
            }
            .transform ==> Some(
            shapes2.Polygon(
              List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
            )
          )
        }

        test("when F = Option") {
          def triangleToPolygon(t: shapes1.Triangle): Either[List[String], shapes2.Shape] =
            Right(
              shapes2.Polygon(
                List(
                  t.p1.transformInto[shapes2.Point],
                  t.p2.transformInto[shapes2.Point],
                  t.p3.transformInto[shapes2.Point]
                )
              )
            )

          def rectangleToPolygon(r: shapes1.Rectangle): Either[List[String], shapes2.Shape] =
            Right(
              shapes2.Polygon(
                List(
                  r.p1.transformInto[shapes2.Point],
                  shapes2.Point(r.p1.x, r.p2.y),
                  r.p2.transformInto[shapes2.Point],
                  shapes2.Point(r.p2.x, r.p1.y)
                )
              )
            )

          triangle
            .intoF[Either[List[String], +*], shapes2.Shape]
            .withCoproductInstanceF(triangleToPolygon)
            .withCoproductInstanceF(rectangleToPolygon)
            .transform ==> Right(shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0))))

          rectangle
            .intoF[Either[List[String], +*], shapes2.Shape]
            .withCoproductInstanceF[shapes1.Shape] {
              case r: shapes1.Rectangle => rectangleToPolygon(r)
              case t: shapes1.Triangle  => triangleToPolygon(t)
            }
            .transform ==> Right(
            shapes2.Polygon(
              List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
            )
          )
        }
      }
    }
  }
}
