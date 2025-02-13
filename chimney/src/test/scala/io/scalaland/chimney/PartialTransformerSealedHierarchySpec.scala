package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*
import io.scalaland.chimney.partial.syntax.*
import io.scalaland.chimney.utils.OptionUtils.*

import scala.annotation.unused

class PartialTransformerSealedHierarchySpec extends ChimneySpec {

  test(
    """transform sealed hierarchies from "subset" of case objects to "superset" of case objects without modifiers"""
  ) {
    (colors1.Red: colors1.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Red)
    (colors1.Green: colors1.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Green)
    (colors1.Blue: colors1.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Blue)
  }

  test(
    """transform nested sealed hierarchies between flat and nested hierarchies of case objects without modifiers"""
  ) {
    (colors2.Red: colors2.Color).transformIntoPartial[colors3.Color].asOption ==> Some(colors3.Red)
    (colors2.Green: colors2.Color).transformIntoPartial[colors3.Color].asOption ==> Some(colors3.Green)
    (colors2.Blue: colors2.Color).transformIntoPartial[colors3.Color].asOption ==> Some(colors3.Blue)
    (colors2.Black: colors2.Color).transformIntoPartial[colors3.Color].asOption ==> Some(colors3.Black)

    (colors3.Red: colors3.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Red)
    (colors3.Green: colors3.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Green)
    (colors3.Blue: colors3.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Blue)
    (colors3.Black: colors3.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Black)
  }

  test(
    """transforming flat hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable with Total Transformer"""
  ) {
    implicit val intToDoubleTransformer: Transformer[Int, Double] = (_: Int).toDouble

    (shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0)): shapes1.Shape)
      .transformIntoPartial[shapes3.Shape]
      .asOption ==>
      Some(shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)))
    (shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4)): shapes1.Shape)
      .transformIntoPartial[shapes3.Shape]
      .asOption ==>
      Some(shapes3.Rectangle(shapes3.Point(0.0, 0.0), shapes3.Point(6.0, 4.0)))

    implicit val intToStringTransformer: Transformer[Int, String] = (_: Int).toString
    import numbers.*, ScalesPartialTransformer.shortToLongTotalInner

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

  test(
    """transforming flat hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable with Partial Transformer"""
  ) {
    implicit val intToDoubleTransformer: PartialTransformer[Int, Double] =
      (a: Int, _) => partial.Result.fromValue(a.toDouble)

    (shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0)): shapes1.Shape)
      .transformIntoPartial[shapes3.Shape]
      .asOption ==>
      Some(shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)))
    (shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4)): shapes1.Shape)
      .transformIntoPartial[shapes3.Shape]
      .asOption ==>
      Some(shapes3.Rectangle(shapes3.Point(0.0, 0.0), shapes3.Point(6.0, 4.0)))

    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.asResult)
    import numbers.*, ScalesPartialTransformer.shortToLongPartialInner

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

  test(
    """transforming nested sealed hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable"""
  ) {
    (shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)): shapes3.Shape)
      .transformIntoPartial[shapes4.Shape]
      .asOption ==>
      Some(shapes4.Triangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0), shapes4.Point(0.0, 0.0)))
    (shapes3.Rectangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0)): shapes3.Shape)
      .transformIntoPartial[shapes4.Shape]
      .asOption ==>
      Some(shapes4.Rectangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0)))
    (shapes4.Triangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0), shapes4.Point(0.0, 0.0)): shapes4.Shape)
      .transformIntoPartial[shapes3.Shape]
      .asOption ==>
      Some(shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)))
    (shapes4.Rectangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0)): shapes4.Shape)
      .transformIntoPartial[shapes3.Shape]
      .asOption ==>
      Some(shapes3.Rectangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0)))
  }

  test(
    "not allow transformation of of sealed hierarchies when the transformation would be ambiguous".withTags(
      if (isScala3) Set(munit.Ignore)
      else Set.empty // ignore only on Scala 3 until https://github.com/lampepfl/dotty/issues/18484 is fixed
    )
  ) {
    val error = compileErrorsScala2(
      """
      (shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0)): shapes1.Shape)
        .transformIntoPartial[shapes5.Shape]
      """
    )

    error.check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.shapes1.Shape to io.scalaland.chimney.fixtures.shapes5.Shape",
      "io.scalaland.chimney.fixtures.shapes5.Shape",
      "  coproduct instance io.scalaland.chimney.fixtures.shapes1.Triangle of io.scalaland.chimney.fixtures.shapes1.Shape has ambiguous matches in io.scalaland.chimney.fixtures.shapes5.Shape: io.scalaland.chimney.fixtures.shapes5.Inner.Triangle, io.scalaland.chimney.fixtures.shapes5.Triangle",
      "  coproduct instance io.scalaland.chimney.fixtures.shapes1.Rectangle of io.scalaland.chimney.fixtures.shapes1.Shape has ambiguous matches in io.scalaland.chimney.fixtures.shapes5.Shape: io.scalaland.chimney.fixtures.shapes5.Inner.Rectangle, io.scalaland.chimney.fixtures.shapes5.Rectangle",
      "io.scalaland.chimney.fixtures.shapes5.Shape (transforming from: matching[io.scalaland.chimney.fixtures.shapes1.Triangle])",
      "  derivation from triangle: io.scalaland.chimney.fixtures.shapes1.Triangle to io.scalaland.chimney.fixtures.shapes5.Shape is not supported in Chimney!",
      "io.scalaland.chimney.fixtures.shapes5.Shape (transforming from: matching[io.scalaland.chimney.fixtures.shapes1.Rectangle])",
      "  derivation from rectangle: io.scalaland.chimney.fixtures.shapes1.Rectangle to io.scalaland.chimney.fixtures.shapes5.Shape is not supported in Chimney!",
      "Consult https://chimney.readthedocs.io for usage examples."
    )

    error.checkNot(
      "io.scalaland.chimney.fixtures.shapes1.Circle"
    )
  }

  group("setting .withSealedSubtypeHandled[Subtype](mapping)") {

    test(
      """should be absent by default and not allow transforming "superset" of case class to "subset" of case objects"""
    ) {
      compileErrors("""(colors2.Black: colors2.Color).transformIntoPartial[colors1.Color]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.colors2.Color to io.scalaland.chimney.fixtures.colors1.Color",
        "io.scalaland.chimney.fixtures.colors1.Color",
        "  can't transform coproduct instance io.scalaland.chimney.fixtures.colors2.Black to io.scalaland.chimney.fixtures.colors1.Color",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test(
      """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
    ) {
      def blackIsRed(@unused b: colors2.Black.type): colors1.Color =
        colors1.Red

      (colors2.Black: colors2.Color)
        .intoPartial[colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .transform
        .asOption ==> Some(colors1.Red)

      (colors2.Red: colors2.Color)
        .intoPartial[colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .transform
        .asOption ==> Some(colors1.Red)

      (colors2.Green: colors2.Color)
        .intoPartial[colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .transform
        .asOption ==> Some(colors1.Green)

      (colors2.Blue: colors2.Color)
        .intoPartial[colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .transform
        .asOption ==> Some(colors1.Blue)
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

      triangle
        .intoPartial[shapes2.Shape]
        .withSealedSubtypeHandled(triangleToPolygon)
        .withSealedSubtypeHandled(rectangleToPolygon)
        .transform
        .asOption ==> Some(shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0))))

      val rectangle: shapes1.Shape =
        shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))

      rectangle
        .intoPartial[shapes2.Shape]
        .withSealedSubtypeHandled[shapes1.Shape] {
          case r: shapes1.Rectangle => rectangleToPolygon(r)
          case t: shapes1.Triangle  => triangleToPolygon(t)
        }
        .transform
        .asOption ==> Some(
        shapes2.Polygon(
          List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
        )
      )
    }

    test("should work with semiautomatic derivation") {
      def blackIsRed(@unused b: colors2.Black.type): colors1.Color =
        colors1.Red

      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .buildTransformer
        .transform(colors2.Black)
        .asOption ==> Some(colors1.Red)
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .buildTransformer
        .transform(colors2.Red)
        .asOption ==> Some(colors1.Red)
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .buildTransformer
        .transform(colors2.Green)
        .asOption ==> Some(colors1.Green)
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .buildTransformer
        .transform(colors2.Blue)
        .asOption ==> Some(colors1.Blue)
    }
  }

  group("setting .withEnumCaseHandled[Subtype](mapping)") {

    test(
      """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
    ) {
      def blackIsRed(@unused b: colors2.Black.type): colors1.Color =
        colors1.Red

      (colors2.Black: colors2.Color)
        .intoPartial[colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .transform
        .asOption ==> Some(colors1.Red)

      (colors2.Red: colors2.Color)
        .intoPartial[colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .transform
        .asOption ==> Some(colors1.Red)

      (colors2.Green: colors2.Color)
        .intoPartial[colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .transform
        .asOption ==> Some(colors1.Green)

      (colors2.Blue: colors2.Color)
        .intoPartial[colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .transform
        .asOption ==> Some(colors1.Blue)
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

      triangle
        .intoPartial[shapes2.Shape]
        .withEnumCaseHandled(triangleToPolygon)
        .withEnumCaseHandled(rectangleToPolygon)
        .transform
        .asOption ==> Some(shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0))))

      val rectangle: shapes1.Shape =
        shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))

      rectangle
        .intoPartial[shapes2.Shape]
        .withEnumCaseHandled[shapes1.Shape] {
          case r: shapes1.Rectangle => rectangleToPolygon(r)
          case t: shapes1.Triangle  => triangleToPolygon(t)
        }
        .transform
        .asOption ==> Some(
        shapes2.Polygon(
          List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
        )
      )
    }

    test("should work with semiautomatic derivation") {
      def blackIsRed(@unused b: colors2.Black.type): colors1.Color =
        colors1.Red

      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .buildTransformer
        .transform(colors2.Black)
        .asOption ==> Some(colors1.Red)
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .buildTransformer
        .transform(colors2.Red)
        .asOption ==> Some(colors1.Red)
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .buildTransformer
        .transform(colors2.Green)
        .asOption ==> Some(colors1.Green)
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .buildTransformer
        .transform(colors2.Blue)
        .asOption ==> Some(colors1.Blue)
    }
  }

  test(
    "transform sealed hierarchies of single value wrapping case classes to sealed hierarchy of flat case classes subtypes"
  ) {
    val triangle: shapes1.Shape = shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))
    triangle.transformIntoPartial[shapes6.Shape].asOption ==> Some(
      shapes6.Triangle(shapes6.Shape.Triangle(shapes6.Point(0, 0), shapes6.Point(2, 2), shapes6.Point(2, 0)))
    )

    val rectangle: shapes1.Shape = shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(2, 2))
    rectangle.transformIntoPartial[shapes6.Shape].asOption ==> Some(
      shapes6.Rectangle(shapes6.Shape.Rectangle(shapes6.Point(0, 0), shapes6.Point(2, 2)))
    )
  }

  test(
    "transform sealed hierarchies of flat case classes subtypes to sealed hierarchy of single value wrapping case classes"
  ) {
    val triangle: shapes6.Shape =
      shapes6.Triangle(shapes6.Shape.Triangle(shapes6.Point(0, 0), shapes6.Point(2, 2), shapes6.Point(2, 0)))
    triangle.transformIntoPartial[shapes1.Shape].asOption ==> Some(
      shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))
    )

    val rectangle: shapes6.Shape = shapes6.Rectangle(shapes6.Shape.Rectangle(shapes6.Point(0, 0), shapes6.Point(2, 2)))
    rectangle.transformIntoPartial[shapes1.Shape].asOption ==> Some(
      shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(2, 2))
    )
  }

  group("setting .withSealedSubtypeHandledPartial[Subtype](mapping)") {

    test(
      """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
    ) {
      def blackIsFail(b: colors2.Black.type): partial.Result[colors1.Color] =
        partial.Result.fromEmpty

      (colors2.Black: colors2.Color)
        .intoPartial[colors1.Color]
        .withSealedSubtypeHandledPartial(blackIsFail)
        .transform
        .asOption ==> None

      (colors2.Red: colors2.Color)
        .intoPartial[colors1.Color]
        .withSealedSubtypeHandledPartial(blackIsFail)
        .transform
        .asOption ==> Some(colors1.Red)

      (colors2.Green: colors2.Color)
        .intoPartial[colors1.Color]
        .withSealedSubtypeHandledPartial(blackIsFail)
        .transform
        .asOption ==> Some(colors1.Green)

      (colors2.Blue: colors2.Color)
        .intoPartial[colors1.Color]
        .withSealedSubtypeHandledPartial(blackIsFail)
        .transform
        .asOption ==> Some(colors1.Blue)
    }

    test(
      """transform sealed hierarchies from "superset" of case classes to "subset" of case classes when user-provided mapping handled non-trivial cases"""
    ) {
      def triangleToPolygon(t: shapes1.Triangle): partial.Result[shapes2.Shape] =
        partial.Result.fromValue(
          shapes2.Polygon(
            List(
              t.p1.transformInto[shapes2.Point],
              t.p2.transformInto[shapes2.Point],
              t.p3.transformInto[shapes2.Point]
            )
          )
        )

      def rectangleToPolygon(r: shapes1.Rectangle): partial.Result[shapes2.Shape] =
        partial.Result.fromValue(
          shapes2.Polygon(
            List(
              r.p1.transformInto[shapes2.Point],
              shapes2.Point(r.p1.x, r.p2.y),
              r.p2.transformInto[shapes2.Point],
              shapes2.Point(r.p2.x, r.p1.y)
            )
          )
        )

      val triangle: shapes1.Shape =
        shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))

      triangle
        .intoPartial[shapes2.Shape]
        .withSealedSubtypeHandledPartial(triangleToPolygon)
        .withSealedSubtypeHandledPartial(rectangleToPolygon)
        .transform
        .asOption ==> Some(shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0))))

      val rectangle: shapes1.Shape =
        shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))

      rectangle
        .intoPartial[shapes2.Shape]
        .withSealedSubtypeHandledPartial[shapes1.Shape] {
          case r: shapes1.Rectangle => rectangleToPolygon(r)
          case t: shapes1.Triangle  => triangleToPolygon(t)
        }
        .transform
        .asOption ==> Some(
        shapes2.Polygon(
          List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
        )
      )
    }

    test("should work with semiautomatic derivation") {
      def blackIsFail(b: colors2.Black.type): partial.Result[colors1.Color] =
        partial.Result.fromEmpty

      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withSealedSubtypeHandledPartial(blackIsFail)
        .buildTransformer
        .transform(colors2.Black)
        .asOption ==> None
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withSealedSubtypeHandledPartial(blackIsFail)
        .buildTransformer
        .transform(colors2.Red)
        .asOption ==> Some(colors1.Red)
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withSealedSubtypeHandledPartial(blackIsFail)
        .buildTransformer
        .transform(colors2.Green)
        .asOption ==> Some(colors1.Green)
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withSealedSubtypeHandledPartial(blackIsFail)
        .buildTransformer
        .transform(colors2.Blue)
        .asOption ==> Some(colors1.Blue)
    }
  }

  group("setting .withEnumCaseHandledPartial[Subtype](mapping)") {

    test(
      """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
    ) {
      def blackIsFail(b: colors2.Black.type): partial.Result[colors1.Color] =
        partial.Result.fromEmpty

      (colors2.Black: colors2.Color)
        .intoPartial[colors1.Color]
        .withEnumCaseHandledPartial(blackIsFail)
        .transform
        .asOption ==> None

      (colors2.Red: colors2.Color)
        .intoPartial[colors1.Color]
        .withEnumCaseHandledPartial(blackIsFail)
        .transform
        .asOption ==> Some(colors1.Red)

      (colors2.Green: colors2.Color)
        .intoPartial[colors1.Color]
        .withEnumCaseHandledPartial(blackIsFail)
        .transform
        .asOption ==> Some(colors1.Green)

      (colors2.Blue: colors2.Color)
        .intoPartial[colors1.Color]
        .withEnumCaseHandledPartial(blackIsFail)
        .transform
        .asOption ==> Some(colors1.Blue)
    }

    test(
      """transform sealed hierarchies from "superset" of case classes to "subset" of case classes when user-provided mapping handled non-trivial cases"""
    ) {
      def triangleToPolygon(t: shapes1.Triangle): partial.Result[shapes2.Shape] =
        partial.Result.fromValue(
          shapes2.Polygon(
            List(
              t.p1.transformInto[shapes2.Point],
              t.p2.transformInto[shapes2.Point],
              t.p3.transformInto[shapes2.Point]
            )
          )
        )

      def rectangleToPolygon(r: shapes1.Rectangle): partial.Result[shapes2.Shape] =
        partial.Result.fromValue(
          shapes2.Polygon(
            List(
              r.p1.transformInto[shapes2.Point],
              shapes2.Point(r.p1.x, r.p2.y),
              r.p2.transformInto[shapes2.Point],
              shapes2.Point(r.p2.x, r.p1.y)
            )
          )
        )

      val triangle: shapes1.Shape =
        shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))

      triangle
        .intoPartial[shapes2.Shape]
        .withEnumCaseHandledPartial(triangleToPolygon)
        .withEnumCaseHandledPartial(rectangleToPolygon)
        .transform
        .asOption ==> Some(shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0))))

      val rectangle: shapes1.Shape =
        shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))

      rectangle
        .intoPartial[shapes2.Shape]
        .withEnumCaseHandledPartial[shapes1.Shape] {
          case r: shapes1.Rectangle => rectangleToPolygon(r)
          case t: shapes1.Triangle  => triangleToPolygon(t)
        }
        .transform
        .asOption ==> Some(
        shapes2.Polygon(
          List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
        )
      )
    }

    test("should work with semiautomatic derivation") {
      def blackIsFail(b: colors2.Black.type): partial.Result[colors1.Color] =
        partial.Result.fromEmpty

      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withEnumCaseHandledPartial(blackIsFail)
        .buildTransformer
        .transform(colors2.Black)
        .asOption ==> None
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withEnumCaseHandledPartial(blackIsFail)
        .buildTransformer
        .transform(colors2.Red)
        .asOption ==> Some(colors1.Red)
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withEnumCaseHandledPartial(blackIsFail)
        .buildTransformer
        .transform(colors2.Green)
        .asOption ==> Some(colors1.Green)
      PartialTransformer
        .define[colors2.Color, colors1.Color]
        .withEnumCaseHandledPartial(blackIsFail)
        .buildTransformer
        .transform(colors2.Blue)
        .asOption ==> Some(colors1.Blue)
    }
  }

  group("setting .withSealedSubtypeRenamed[FromSubtype, ToSubtype]") {

    import fixtures.renames.Subtypes.*

    test(
      """should be absent by default and not allow transforming "superset" of case class to "subset" of case objects"""
    ) {

      compileErrors("""(Foo3.Baz: Foo3).intoPartial[Bar].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Foo3 to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "  can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Foo3.Bazz to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar (transforming from: matching[io.scalaland.chimney.fixtures.renames.Subtypes.Foo3.Bazz])",
        "  derivation from bazz: io.scalaland.chimney.fixtures.renames.Subtypes.Foo3.Bazz to io.scalaland.chimney.fixtures.renames.Subtypes.Bar is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("transform sealed hierarchy's subtype into user-provided subtype") {

      (Foo3.Baz: Foo3)
        .intoPartial[Bar]
        .withSealedSubtypeRenamed[Foo3.Bazz.type, Bar.Baz.type]
        .transform
        .asOption ==> Some(Bar.Baz)
      (Foo3.Bazz: Foo3)
        .intoPartial[Bar]
        .withSealedSubtypeRenamed[Foo3.Bazz.type, Bar.Baz.type]
        .transform
        .asOption ==> Some(Bar.Baz)
    }

    test("should work with semiautomatic derivation") {

      PartialTransformer
        .define[Foo3, Bar]
        .withSealedSubtypeRenamed[Foo3.Bazz.type, Bar.Baz.type]
        .buildTransformer
        .transform(Foo3.Baz)
        .asOption ==> Some(Bar.Baz)
      PartialTransformer
        .define[Foo3, Bar]
        .withSealedSubtypeRenamed[Foo3.Bazz.type, Bar.Baz.type]
        .buildTransformer
        .transform(Foo3.Bazz)
        .asOption ==> Some(Bar.Baz)
    }
  }

  group("setting .withEnumCaseRenamed[FromSubtype, ToSubtype]") {

    import fixtures.renames.Subtypes.*

    test("transform sealed hierarchy's subtype into user-provided subtype") {

      (Foo3.Baz: Foo3)
        .intoPartial[Bar]
        .withEnumCaseRenamed[Foo3.Bazz.type, Bar.Baz.type]
        .transform
        .asOption ==> Some(Bar.Baz)
      (Foo3.Bazz: Foo3)
        .intoPartial[Bar]
        .withEnumCaseRenamed[Foo3.Bazz.type, Bar.Baz.type]
        .transform
        .asOption ==> Some(Bar.Baz)
    }

    test("should work with semiautomatic derivation") {

      PartialTransformer
        .define[Foo3, Bar]
        .withEnumCaseRenamed[Foo3.Bazz.type, Bar.Baz.type]
        .buildTransformer
        .transform(Foo3.Baz)
        .asOption ==> Some(Bar.Baz)
      PartialTransformer
        .define[Foo3, Bar]
        .withEnumCaseRenamed[Foo3.Bazz.type, Bar.Baz.type]
        .buildTransformer
        .transform(Foo3.Bazz)
        .asOption ==> Some(Bar.Baz)
    }
  }

  group("setting .withSealedSubtypeUnmatched(_.from)") {

    test("should fail derivarion if the subtype is required") {
      compileErrors(
        """(colors1.Red: colors1.Color).intoPartial[colors2.Color].withSealedSubtypeUnmatched(_.matching[colors2.Green.type]).transform"""
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.colors1.Color to io.scalaland.chimney.fixtures.colors2.Color",
        "io.scalaland.chimney.fixtures.colors2.Color",
        "  can't transform coproduct instance io.scalaland.chimney.fixtures.colors1.Green to io.scalaland.chimney.fixtures.colors2.Color",
        "io.scalaland.chimney.fixtures.colors2.Color",
        "  derivation from green: io.scalaland.chimney.fixtures.colors1.Green to io.scalaland.chimney.fixtures.colors2.Color is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should suppress error if UnusedFieldPolicy is used") {
      compileErrors(
        """(colors1.Red: colors1.Color).intoPartial[colors2.Color].enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype).transform"""
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.colors1.Color to io.scalaland.chimney.fixtures.colors2.Color",
        "io.scalaland.chimney.fixtures.colors2.Color",
        "  FailOnUnmatchedTargetSubtype policy check failed at _, offenders: io.scalaland.chimney.fixtures.colors2.Black!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      (colors1.Red: colors1.Color)
        .intoPartial[colors2.Color]
        // FIXME: if we swap these 2 it's assertion error in -Xcheck-macros on Scala 3 o_0
        .withSealedSubtypeUnmatched(_.matching[colors2.Black.type])
        .enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype)
        .transform
        .asOption ==> Some(colors2.Red)
    }

    test("should work with semiautomatic derivation") {

      PartialTransformer
        .define[colors1.Color, colors2.Color]
        // FIXME: if we swap these 2 it's assertion error in -Xcheck-macros on Scala 3 o_0
        .withSealedSubtypeUnmatched(_.matching[colors2.Black.type])
        .enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype)
        .buildTransformer
        .transform(colors1.Red)
        .asOption ==> Some(colors2.Red)
    }
  }

  group("setting .withEnumCaseUnmatched(_.from)") {

    test("should fail derivarion if the subtype is required") {
      compileErrors(
        """(colors1.Red: colors1.Color).intoPartial[colors2.Color].withEnumCaseUnmatched(_.matching[colors2.Green.type]).transform"""
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.colors1.Color to io.scalaland.chimney.fixtures.colors2.Color",
        "io.scalaland.chimney.fixtures.colors2.Color",
        "  can't transform coproduct instance io.scalaland.chimney.fixtures.colors1.Green to io.scalaland.chimney.fixtures.colors2.Color",
        "io.scalaland.chimney.fixtures.colors2.Color",
        "  derivation from green: io.scalaland.chimney.fixtures.colors1.Green to io.scalaland.chimney.fixtures.colors2.Color is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should suppress error if UnusedFieldPolicy is used") {
      compileErrors(
        """(colors1.Red: colors1.Color).intoPartial[colors2.Color].enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype).transform"""
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.colors1.Color to io.scalaland.chimney.fixtures.colors2.Color",
        "io.scalaland.chimney.fixtures.colors2.Color",
        "  FailOnUnmatchedTargetSubtype policy check failed at _, offenders: io.scalaland.chimney.fixtures.colors2.Black!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      (colors1.Red: colors1.Color)
        .intoPartial[colors2.Color]
        // FIXME: if we swap these 2 it's assertion error in -Xcheck-macros on Scala 3 o_0
        .withEnumCaseUnmatched(_.matching[colors2.Black.type])
        .enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype)
        .transform
        .asOption ==> Some(colors2.Red)
    }

    test("should work with semiautomatic derivation") {

      PartialTransformer
        .define[colors1.Color, colors2.Color]
        // FIXME: if we swap these 2 it's assertion error in -Xcheck-macros on Scala 3 o_0
        .withEnumCaseUnmatched(_.matching[colors2.Black.type])
        .enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype)
        .buildTransformer
        .transform(colors1.Red)
        .asOption ==> Some(colors2.Red)
    }
  }

  group("setting .withFieldComputedFrom(selectorFrom)(selectorTo, mapping)") {

    test("should provide support for withSealedSubtypeHandled/withEnumCaseHandled cases but nested") {
      def blackIsRed(@unused b: colors2.Black.type): colors1.Color =
        colors1.Red

      (Some(colors2.Black): Option[colors2.Color])
        .intoPartial[Option[colors1.Color]]
        .withFieldComputedFrom(_.matchingSome.matching[colors2.Black.type])(_.matchingSome, blackIsRed)
        .transform
        .asOption ==> Some(Some(colors1.Red))
      (Some(colors2.Red): Option[colors2.Color])
        .intoPartial[Option[colors1.Color]]
        .withFieldComputedFrom(_.matchingSome.matching[colors2.Black.type])(_.matchingSome, blackIsRed)
        .transform
        .asOption ==> Some(Some(colors1.Red))
      (Some(colors2.Green): Option[colors2.Color])
        .intoPartial[Option[colors1.Color]]
        .withFieldComputedFrom(_.matchingSome.matching[colors2.Black.type])(_.matchingSome, blackIsRed)
        .transform
        .asOption ==> Some(Some(colors1.Green))
      (Some(colors2.Blue): Option[colors2.Color])
        .intoPartial[Option[colors1.Color]]
        .withFieldComputedFrom(_.matchingSome.matching[colors2.Black.type])(_.matchingSome, blackIsRed)
        .transform
        .asOption ==> Some(Some(colors1.Blue))

      (Left(colors2.Black): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsRed)
        .withFieldComputedFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsRed)
        .transform
        .asOption ==> Some(Left(colors1.Red))
      (Left(colors2.Red): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsRed)
        .withFieldComputedFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsRed)
        .transform
        .asOption ==> Some(Left(colors1.Red))
      (Left(colors2.Green): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsRed)
        .withFieldComputedFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsRed)
        .transform
        .asOption ==> Some(Left(colors1.Green))
      (Left(colors2.Blue): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsRed)
        .withFieldComputedFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsRed)
        .transform
        .asOption ==> Some(Left(colors1.Blue))
      (Right(colors2.Black): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsRed)
        .withFieldComputedFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsRed)
        .transform
        .asOption ==> Some(Right(colors1.Red))
      (Right(colors2.Red): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsRed)
        .withFieldComputedFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsRed)
        .transform
        .asOption ==> Some(Right(colors1.Red))
      (Right(colors2.Green): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsRed)
        .withFieldComputedFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsRed)
        .transform
        .asOption ==> Some(Right(colors1.Green))
      (Right(colors2.Blue): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsRed)
        .withFieldComputedFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsRed)
        .transform
        .asOption ==> Some(Right(colors1.Blue))

      (List(colors2.Black, colors2.Red, colors2.Green, colors2.Blue): List[colors2.Color])
        .intoPartial[List[colors1.Color]]
        .withFieldComputedFrom(_.everyItem.matching[colors2.Black.type])(_.everyItem, blackIsRed)
        .transform
        .asOption ==> Some(List(colors1.Red, colors1.Red, colors1.Green, colors1.Blue))

      (Map(
        colors2.Black -> colors2.Black,
        colors2.Red -> colors2.Red,
        colors2.Green -> colors2.Green,
        colors2.Blue -> colors2.Blue
      ): Map[
        colors2.Color,
        colors2.Color
      ])
        .intoPartial[Map[colors1.Color, colors1.Color]]
        .withFieldComputedFrom(_.everyMapKey.matching[colors2.Black.type])(_.everyMapKey, blackIsRed)
        .withFieldComputedFrom(_.everyMapValue.matching[colors2.Black.type])(_.everyMapValue, blackIsRed)
        .transform
        .asOption ==> Some(
        Map(
          colors1.Red -> colors1.Red,
          colors1.Red -> colors1.Red,
          colors1.Green -> colors1.Green,
          colors1.Blue -> colors1.Blue
        )
      )
    }
  }

  group("setting .withFieldComputedPartialFrom(selectorFrom)(selectorTo, mapping)") {

    test("should provide support for withSealedSubtypeHandledPartial/withEnumCaseHandledPartial cases but nested") {
      def blackIsFail(b: colors2.Black.type): partial.Result[colors1.Color] =
        partial.Result.fromEmpty

      (Some(colors2.Black): Option[colors2.Color])
        .intoPartial[Option[colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingSome.matching[colors2.Black.type])(_.matchingSome, blackIsFail)
        .transform
        .asOption ==> None
      (Some(colors2.Red): Option[colors2.Color])
        .intoPartial[Option[colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingSome.matching[colors2.Black.type])(_.matchingSome, blackIsFail)
        .transform
        .asOption ==> Some(Some(colors1.Red))
      (Some(colors2.Green): Option[colors2.Color])
        .intoPartial[Option[colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingSome.matching[colors2.Black.type])(_.matchingSome, blackIsFail)
        .transform
        .asOption ==> Some(Some(colors1.Green))
      (Some(colors2.Blue): Option[colors2.Color])
        .intoPartial[Option[colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingSome.matching[colors2.Black.type])(_.matchingSome, blackIsFail)
        .transform
        .asOption ==> Some(Some(colors1.Blue))

      (Left(colors2.Black): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsFail)
        .withFieldComputedPartialFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsFail)
        .transform
        .asOption ==> None
      (Left(colors2.Red): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsFail)
        .withFieldComputedPartialFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsFail)
        .transform
        .asOption ==> Some(Left(colors1.Red))
      (Left(colors2.Green): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsFail)
        .withFieldComputedPartialFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsFail)
        .transform
        .asOption ==> Some(Left(colors1.Green))
      (Left(colors2.Blue): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsFail)
        .withFieldComputedPartialFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsFail)
        .transform
        .asOption ==> Some(Left(colors1.Blue))
      (Right(colors2.Black): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsFail)
        .withFieldComputedPartialFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsFail)
        .transform
        .asOption ==> None
      (Right(colors2.Red): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsFail)
        .withFieldComputedPartialFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsFail)
        .transform
        .asOption ==> Some(Right(colors1.Red))
      (Right(colors2.Green): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsFail)
        .withFieldComputedPartialFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsFail)
        .transform
        .asOption ==> Some(Right(colors1.Green))
      (Right(colors2.Blue): Either[colors2.Color, colors2.Color])
        .intoPartial[Either[colors1.Color, colors1.Color]]
        .withFieldComputedPartialFrom(_.matchingLeft.matching[colors2.Black.type])(_.matchingLeft, blackIsFail)
        .withFieldComputedPartialFrom(_.matchingRight.matching[colors2.Black.type])(_.matchingRight, blackIsFail)
        .transform
        .asOption ==> Some(Right(colors1.Blue))

      (List(colors2.Black): List[colors2.Color])
        .intoPartial[List[colors1.Color]]
        .withFieldComputedPartialFrom(_.everyItem.matching[colors2.Black.type])(_.everyItem, blackIsFail)
        .transform
        .asOption ==> None
      (List(colors2.Red, colors2.Green, colors2.Blue): List[colors2.Color])
        .intoPartial[List[colors1.Color]]
        .withFieldComputedPartialFrom(_.everyItem.matching[colors2.Black.type])(_.everyItem, blackIsFail)
        .transform
        .asOption ==> Some(List(colors1.Red, colors1.Green, colors1.Blue))

      (Map(colors2.Black -> colors2.Red): Map[colors2.Color, colors2.Color])
        .intoPartial[Map[colors1.Color, colors1.Color]]
        .withFieldComputedPartialFrom(_.everyMapKey.matching[colors2.Black.type])(_.everyMapKey, blackIsFail)
        .withFieldComputedPartialFrom(_.everyMapValue.matching[colors2.Black.type])(_.everyMapValue, blackIsFail)
        .transform
        .asOption ==> None
      (Map(colors2.Blue -> colors2.Black): Map[colors2.Color, colors2.Color])
        .intoPartial[Map[colors1.Color, colors1.Color]]
        .withFieldComputedPartialFrom(_.everyMapKey.matching[colors2.Black.type])(_.everyMapKey, blackIsFail)
        .withFieldComputedPartialFrom(_.everyMapValue.matching[colors2.Black.type])(_.everyMapValue, blackIsFail)
        .transform
        .asOption ==> None
      (Map(colors2.Red -> colors2.Red, colors2.Green -> colors2.Green, colors2.Blue -> colors2.Blue): Map[
        colors2.Color,
        colors2.Color
      ])
        .intoPartial[Map[colors1.Color, colors1.Color]]
        .withFieldComputedPartialFrom(_.everyMapKey.matching[colors2.Black.type])(_.everyMapKey, blackIsFail)
        .withFieldComputedPartialFrom(_.everyMapValue.matching[colors2.Black.type])(_.everyMapValue, blackIsFail)
        .transform
        .asOption ==> Some(
        Map(colors1.Red -> colors1.Red, colors1.Green -> colors1.Green, colors1.Blue -> colors1.Blue)
      )
    }
  }

  group("setting .withFieldRenamed(selectorFrom, selectorTo)") {

    import fixtures.renames.Subtypes.*

    test("should provide support for withSealedSubtypeRenamed/withEnumCaseRenamed cases but nested") {
      (Some(Foo3.Baz): Option[Foo3])
        .intoPartial[Option[Bar]]
        .withFieldRenamed(_.matchingSome.matching[Foo3.Bazz.type], _.matchingSome.matching[Bar.Baz.type])
        .transform
        .asOption ==> Some(Some(Bar.Baz))
      (Some(Foo3.Bazz): Option[Foo3])
        .intoPartial[Option[Bar]]
        .withFieldRenamed(_.matchingSome.matching[Foo3.Bazz.type], _.matchingSome.matching[Bar.Baz.type])
        .transform
        .asOption ==> Some(Some(Bar.Baz))

      (Left(Foo3.Baz): Either[Foo3, Foo3])
        .intoPartial[Either[Bar, Bar]]
        .withFieldRenamed(_.matchingLeft.matching[Foo3.Bazz.type], _.matchingLeft.matching[Bar.Baz.type])
        .withFieldRenamed(_.matchingRight.matching[Foo3.Bazz.type], _.matchingRight.matching[Bar.Baz.type])
        .transform
        .asOption ==> Some(Left(Bar.Baz))
      (Left(Foo3.Bazz): Either[Foo3, Foo3])
        .intoPartial[Either[Bar, Bar]]
        .withFieldRenamed(_.matchingLeft.matching[Foo3.Bazz.type], _.matchingLeft.matching[Bar.Baz.type])
        .withFieldRenamed(_.matchingRight.matching[Foo3.Bazz.type], _.matchingRight.matching[Bar.Baz.type])
        .transform
        .asOption ==> Some(Left(Bar.Baz))
      (Right(Foo3.Baz): Either[Foo3, Foo3])
        .intoPartial[Either[Bar, Bar]]
        .withFieldRenamed(_.matchingLeft.matching[Foo3.Bazz.type], _.matchingLeft.matching[Bar.Baz.type])
        .withFieldRenamed(_.matchingRight.matching[Foo3.Bazz.type], _.matchingRight.matching[Bar.Baz.type])
        .transform
        .asOption ==> Some(Right(Bar.Baz))
      (Right(Foo3.Bazz): Either[Foo3, Foo3])
        .intoPartial[Either[Bar, Bar]]
        .withFieldRenamed(_.matchingLeft.matching[Foo3.Bazz.type], _.matchingLeft.matching[Bar.Baz.type])
        .withFieldRenamed(_.matchingRight.matching[Foo3.Bazz.type], _.matchingRight.matching[Bar.Baz.type])
        .transform
        .asOption ==> Some(Right(Bar.Baz))

      (List(Foo3.Baz, Foo3.Bazz): List[Foo3])
        .intoPartial[List[Bar]]
        .withFieldRenamed(_.everyItem.matching[Foo3.Bazz.type], _.everyItem.matching[Bar.Baz.type])
        .transform
        .asOption ==> Some(List(Bar.Baz, Bar.Baz))

      (Map(Foo3.Baz -> Foo3.Baz, Foo3.Bazz -> Foo3.Bazz): Map[Foo3, Foo3])
        .intoPartial[Map[Bar, Bar]]
        .withFieldRenamed(_.everyMapKey.matching[Foo3.Bazz.type], _.everyMapKey.matching[Bar.Baz.type])
        .withFieldRenamed(_.everyMapValue.matching[Foo3.Bazz.type], _.everyMapValue.matching[Bar.Baz.type])
        .transform
        .asOption ==> Some(Map(Bar.Baz -> Bar.Baz))
    }
  }

  group("flag .enableCustomSubtypeNameComparison") {

    import fixtures.renames.Subtypes.*

    test("should be disabled by default") {

      compileErrors("""(Foo.BAZ: Foo).transformIntoPartial[Bar]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Foo to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "  can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar (transforming from: matching[io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ])",
        "  derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""(Foo.BAZ: Foo).intoPartial[Bar].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Foo to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "  can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar (transforming from: matching[io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ])",
        "  derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""(Bar.Baz: Bar).transformIntoPartial[Foo]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Bar to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "  can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo (transforming from: matching[io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz])",
        "  derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""(Bar.Baz: Bar).intoPartial[Foo].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Bar to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "  can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo (transforming from: matching[io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz])",
        "  derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should inform user if and why the setting cannot be read") {
      @unused object BadNameComparison extends TransformedNamesComparison {

        def namesMatch(fromName: String, toName: String): Boolean = fromName.equalsIgnoreCase(toName)
      }

      compileErrors("""(Foo.BAZ: Foo).into[Bar].enableCustomSubtypeNameComparison(BadNameComparison).transform""")
        .check(
          "Invalid TransformerNamesComparison type - only (case) objects are allowed, and only the ones defined as top-level or in top-level objects, got: io.scalaland.chimney.PartialTransformerSealedHierarchySpec.BadNameComparison!!!"
        )
    }

    test("should inform user when the matcher they provided results in ambiguities") {

      (Foo2.baz: Foo2).transformIntoPartial[BarAmbiguous].asOption ==> Some(BarAmbiguous.baz)
      (Foo2.baz: Foo2).intoPartial[BarAmbiguous].transform.asOption ==> Some(BarAmbiguous.baz)

      compileErrors(
        """
        (Foo2.baz: Foo2)
          .intoPartial[BarAmbiguous]
          .enableCustomSubtypeNameComparison(TransformedNamesComparison.BeanAware)
          .transform
        """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Foo2 to io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous",
        "io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous",
        "  coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Foo2.baz of io.scalaland.chimney.fixtures.renames.Subtypes.Foo2 has ambiguous matches in io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous: io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous.baz, io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous.getBaz",
        "io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous (transforming from: matching[io.scalaland.chimney.fixtures.renames.Subtypes.Foo2.baz])",
        "  derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Foo2.baz to io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should allow subtypes to be matched using user-provided predicate") {
      val result = (Foo.BAZ: Foo)
        .intoPartial[Bar]
        .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
      result.asOption ==> Some(Bar.Baz)
      result.asEither ==> Right(Bar.Baz)
      result.asErrorPathMessageStrings ==> Iterable()

      val result2 = (Bar.Baz: Bar)
        .intoPartial[Foo]
        .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
      result2.asOption ==> Some(Foo.BAZ)
      result2.asEither ==> Right(Foo.BAZ)
      result2.asErrorPathMessageStrings ==> Iterable()

      locally {
        implicit val config = TransformerConfiguration.default.enableCustomSubtypeNameComparison(
          TransformedNamesComparison.CaseInsensitiveEquality
        )

        val result3 = (Foo.BAZ: Foo).transformIntoPartial[Bar]
        result3.asOption ==> Some(Bar.Baz)
        result3.asEither ==> Right(Bar.Baz)
        result3.asErrorPathMessageStrings ==> Iterable()

        val result4 = (Foo.BAZ: Foo).intoPartial[Bar].transform
        result4.asOption ==> Some(Bar.Baz)
        result4.asEither ==> Right(Bar.Baz)
        result4.asErrorPathMessageStrings ==> Iterable()

        val result5 = (Bar.Baz: Bar).transformIntoPartial[Foo]
        result5.asOption ==> Some(Foo.BAZ)
        result5.asEither ==> Right(Foo.BAZ)
        result5.asErrorPathMessageStrings ==> Iterable()

        val result6 = (Bar.Baz: Bar).intoPartial[Foo].transform
        result6.asOption ==> Some(Foo.BAZ)
        result6.asEither ==> Right(Foo.BAZ)
        result6.asErrorPathMessageStrings ==> Iterable()
      }
    }

    test(
      "should allow subtypes to be matched using user-provided predicate only for a single field when scoped using .withSourceFlag(_.field)"
    ) {
      import fixtures.nestedpath.NestedProduct

      val result = NestedProduct(Foo.BAZ: Foo)
        .intoPartial[NestedProduct[Bar]]
        .withSourceFlag(_.value)
        .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
      result.asOption ==> Some(NestedProduct(Bar.Baz))
      result.asEither ==> Right(NestedProduct(Bar.Baz))
      result.asErrorPathMessageStrings ==> Iterable()

      val result2 = NestedProduct(Bar.Baz: Bar)
        .intoPartial[NestedProduct[Foo]]
        .withSourceFlag(_.value)
        .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform
      result2.asOption ==> Some(NestedProduct(Foo.BAZ))
      result2.asEither ==> Right(NestedProduct(Foo.BAZ))
      result2.asErrorPathMessageStrings ==> Iterable()
    }
  }

  group("flag .disableCustomSubtypeNameComparison") {

    import fixtures.renames.Subtypes.*

    test("should disable globally enabled .enableCustomSubtypeNameComparison") {
      @unused implicit val config = TransformerConfiguration.default.enableCustomSubtypeNameComparison(
        TransformedNamesComparison.CaseInsensitiveEquality
      )

      compileErrors("""(Foo.BAZ: Foo).intoPartial[Bar].disableCustomSubtypeNameComparison.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Foo to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "  can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar (transforming from: matching[io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ])",
        "  derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""(Bar.Baz: Bar).intoPartial[Foo].disableCustomSubtypeNameComparison.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Bar to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "  can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo (transforming from: matching[io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz])",
        "  derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

  group("flag .enableUnmatchedSubtypePolicyCheck(policyName)") {

    test("should be disabled by default") {
      (colors1.Red: colors1.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Red)
      (colors1.Green: colors1.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Green)
      (colors1.Blue: colors1.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Blue)
    }

    test("should fail compilation when policy is violated") {
      compileErrors(
        """(colors1.Red: colors1.Color).intoPartial[colors2.Color].enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype).transform"""
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.colors1.Color to io.scalaland.chimney.fixtures.colors2.Color",
        "io.scalaland.chimney.fixtures.colors2.Color",
        "  FailOnUnmatchedTargetSubtype policy check failed at _, offenders: io.scalaland.chimney.fixtures.colors2.Black!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      locally {
        @unused implicit val config =
          TransformerConfiguration.default.enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype)

        compileErrors("""(colors1.Red: colors1.Color).transformIntoPartial[colors2.Color]""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.colors1.Color to io.scalaland.chimney.fixtures.colors2.Color",
          "io.scalaland.chimney.fixtures.colors2.Color",
          "  FailOnUnmatchedTargetSubtype policy check failed at _, offenders: io.scalaland.chimney.fixtures.colors2.Black!",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
      }
    }
  }

  group("flag .disableUnusedFieldPolicyCheck") {

    test("should disable globally enabled .enableUnusedFieldPolicyCheck") {
      @unused implicit val config =
        TransformerConfiguration.default.enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype)

      (colors1.Red: colors1.Color)
        .intoPartial[colors2.Color]
        .disableUnmatchedSubtypePolicyCheck
        .transform
        .asOption ==> Some(colors2.Red)
      (colors1.Green: colors1.Color)
        .intoPartial[colors2.Color]
        .disableUnmatchedSubtypePolicyCheck
        .transform
        .asOption ==> Some(colors2.Green)
      (colors1.Blue: colors1.Color)
        .intoPartial[colors2.Color]
        .disableUnmatchedSubtypePolicyCheck
        .transform
        .asOption ==> Some(colors2.Blue)
    }
  }
}
