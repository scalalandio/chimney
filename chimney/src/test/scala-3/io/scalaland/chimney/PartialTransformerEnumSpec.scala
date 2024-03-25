package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*
import io.scalaland.chimney.utils.OptionUtils.*

import scala.annotation.unused

class PartialTransformerEnumSpec extends ChimneySpec {

  test(
    """transform sealed hierarchies from "subset" of case objects to "superset" of case objects without modifiers"""
  ) {
    (colors1enums.Color.Red: colors1enums.Color).transformIntoPartial[colors2enums.Color].asOption ==> Some(
      colors2enums.Color.Red
    )
    (colors1enums.Color.Green: colors1enums.Color).transformIntoPartial[colors2enums.Color].asOption ==> Some(
      colors2enums.Color.Green
    )
    (colors1enums.Color.Blue: colors1enums.Color).transformIntoPartial[colors2enums.Color].asOption ==> Some(
      colors2enums.Color.Blue
    )
  }

  test(
    """transform nested sealed hierarchies between flat and nested hierarchies of case objects without modifiers"""
  ) {
    (colors2enums.Color.Red: colors2enums.Color).transformIntoPartial[colors3enums.Color].asOption ==> Some(
      colors3enums.SimpleColor.Red
    )
    (colors2enums.Color.Green: colors2enums.Color).transformIntoPartial[colors3enums.Color].asOption ==> Some(
      colors3enums.SimpleColor.Green
    )
    (colors2enums.Color.Blue: colors2enums.Color).transformIntoPartial[colors3enums.Color].asOption ==> Some(
      colors3enums.SimpleColor.Blue
    )
    (colors2enums.Color.Black: colors2enums.Color).transformIntoPartial[colors3enums.Color].asOption ==> Some(
      colors3enums.ComplexColor.Black
    )

    (colors3enums.SimpleColor.Red: colors3enums.Color).transformIntoPartial[colors2enums.Color].asOption ==> Some(
      colors2enums.Color.Red
    )
    (colors3enums.SimpleColor.Green: colors3enums.Color).transformIntoPartial[colors2enums.Color].asOption ==> Some(
      colors2enums.Color.Green
    )
    (colors3enums.SimpleColor.Blue: colors3enums.Color).transformIntoPartial[colors2enums.Color].asOption ==> Some(
      colors2enums.Color.Blue
    )
    (colors3enums.ComplexColor.Black: colors3enums.Color).transformIntoPartial[colors2enums.Color].asOption ==> Some(
      colors2enums.Color.Black
    )
  }

  test(
    """transforming flat hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable with Total Transformer"""
  ) {
    implicit val intToDoubleTransformer: Transformer[Int, Double] = (_: Int).toDouble

    (shapes1enums.Shape.Triangle(
      shapes1enums.Point(0, 0),
      shapes1enums.Point(2, 2),
      shapes1enums.Point(2, 0)
    ): shapes1enums.Shape)
      .transformIntoPartial[shapes3enums.Shape]
      .asOption ==>
      Some(
        shapes3enums.Shape
          .Triangle(shapes3enums.Point(2.0, 0.0), shapes3enums.Point(2.0, 2.0), shapes3enums.Point(0.0, 0.0))
      )
    (shapes1enums.Shape.Rectangle(shapes1enums.Point(0, 0), shapes1enums.Point(6, 4)): shapes1enums.Shape)
      .transformIntoPartial[shapes3enums.Shape]
      .asOption ==>
      Some(shapes3enums.Shape.Rectangle(shapes3enums.Point(0.0, 0.0), shapes3enums.Point(6.0, 4.0)))

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

    (shapes1enums.Shape.Triangle(
      shapes1enums.Point(0, 0),
      shapes1enums.Point(2, 2),
      shapes1enums.Point(2, 0)
    ): shapes1enums.Shape)
      .transformIntoPartial[shapes3enums.Shape]
      .asOption ==>
      Some(
        shapes3enums.Shape
          .Triangle(shapes3enums.Point(2.0, 0.0), shapes3enums.Point(2.0, 2.0), shapes3enums.Point(0.0, 0.0))
      )
    (shapes1enums.Shape.Rectangle(shapes1enums.Point(0, 0), shapes1enums.Point(6, 4)): shapes1enums.Shape)
      .transformIntoPartial[shapes3enums.Shape]
      .asOption ==>
      Some(shapes3enums.Shape.Rectangle(shapes3enums.Point(0.0, 0.0), shapes3enums.Point(6.0, 4.0)))

    implicit val intParserOpt: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.toPartialResult)
    import numbers.*, ScalesEnumsPartialTransformer.shortToLongPartialInner

    (shortEnums.NumScale.Zero: shortEnums.NumScale[String, Nothing])
      .transformIntoPartial[longEnums.NumScale[Int]]
      .asOption ==> Some(longEnums.NumScale.Zero)
    (shortEnums.NumScale.Million("4"): shortEnums.NumScale[String, Nothing])
      .transformIntoPartial[longEnums.NumScale[Int]]
      .asOption ==> Some(longEnums.NumScale.Million(4))
    (shortEnums.NumScale.Billion("2"): shortEnums.NumScale[String, Nothing])
      .transformIntoPartial[longEnums.NumScale[Int]]
      .asOption ==> Some(longEnums.NumScale.Milliard(2))
    (shortEnums.NumScale.Trillion("100"): shortEnums.NumScale[String, Nothing])
      .transformIntoPartial[longEnums.NumScale[Int]]
      .asOption ==> Some(longEnums.NumScale.Billion(100))

    (shortEnums.NumScale.Million("x"): shortEnums.NumScale[String, Nothing])
      .transformIntoPartial[longEnums.NumScale[Int]]
      .asOption ==> None
    (shortEnums.NumScale.Billion("x"): shortEnums.NumScale[String, Nothing])
      .transformIntoPartial[longEnums.NumScale[Int]]
      .asOption ==> None
    (shortEnums.NumScale.Trillion("x"): shortEnums.NumScale[String, Nothing])
      .transformIntoPartial[longEnums.NumScale[Int]]
      .asOption ==> None
  }

  test(
    """transforming nested sealed hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable"""
  ) {
    (shapes3enums.Shape.Triangle(
      shapes3enums.Point(2.0, 0.0),
      shapes3enums.Point(2.0, 2.0),
      shapes3enums.Point(0.0, 0.0)
    ): shapes3enums.Shape)
      .transformIntoPartial[shapes4enums.Shape]
      .asOption ==>
      Some(
        shapes4enums.ThreeAnglesShape
          .Triangle(shapes4enums.Point(2.0, 0.0), shapes4enums.Point(2.0, 2.0), shapes4enums.Point(0.0, 0.0))
      )
    (shapes3enums.Shape.Rectangle(shapes3enums.Point(2.0, 0.0), shapes3enums.Point(2.0, 2.0)): shapes3enums.Shape)
      .transformIntoPartial[shapes4enums.Shape]
      .asOption ==>
      Some(shapes4enums.FourAnglesShape.Rectangle(shapes4enums.Point(2.0, 0.0), shapes4enums.Point(2.0, 2.0)))
    (shapes4enums.ThreeAnglesShape.Triangle(
      shapes4enums.Point(2.0, 0.0),
      shapes4enums.Point(2.0, 2.0),
      shapes4enums.Point(0.0, 0.0)
    ): shapes4enums.Shape)
      .transformIntoPartial[shapes3enums.Shape]
      .asOption ==>
      Some(
        shapes3enums.Shape
          .Triangle(shapes3enums.Point(2.0, 0.0), shapes3enums.Point(2.0, 2.0), shapes3enums.Point(0.0, 0.0))
      )
    (shapes4enums.FourAnglesShape.Rectangle(
      shapes4enums.Point(2.0, 0.0),
      shapes4enums.Point(2.0, 2.0)
    ): shapes4enums.Shape)
      .transformIntoPartial[shapes3enums.Shape]
      .asOption ==>
      Some(shapes3enums.Shape.Rectangle(shapes3enums.Point(2.0, 0.0), shapes3enums.Point(2.0, 2.0)))
  }

  group("setting .withCoproductInstance(mapping)") {

    test(
      """should be absent by default and not allow transforming "superset" of case class to "subset" of case objects"""
    ) {
      compileErrorsFixed("""(colors2enums.Color.Black: colors2enums.Color).transformIntoPartial[colors1enums.Color]""")
        .check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.colors2enums.Color to io.scalaland.chimney.fixtures.colors1enums.Color",
          "io.scalaland.chimney.fixtures.colors1enums.Color",
          "can't transform coproduct instance io.scalaland.chimney.fixtures.colors2enums.Color.Black to io.scalaland.chimney.fixtures.colors1enums.Color",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
    }

    test(
      """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
    ) {
      def blackIsRed(@unused b: colors2enums.Color.Black.type): colors1enums.Color =
        colors1enums.Color.Red

      (colors2enums.Color.Black: colors2enums.Color)
        .intoPartial[colors1enums.Color]
        .withCoproductInstance(blackIsRed)
        .transform
        .asOption ==> Some(colors1enums.Color.Red)

      (colors2enums.Color.Red: colors2enums.Color)
        .intoPartial[colors1enums.Color]
        .withCoproductInstance(blackIsRed)
        .transform
        .asOption ==> Some(colors1enums.Color.Red)

      (colors2enums.Color.Green: colors2enums.Color)
        .intoPartial[colors1enums.Color]
        .withCoproductInstance(blackIsRed)
        .transform
        .asOption ==> Some(colors1enums.Color.Green)

      (colors2enums.Color.Blue: colors2enums.Color)
        .intoPartial[colors1enums.Color]
        .withCoproductInstance(blackIsRed)
        .transform
        .asOption ==> Some(colors1enums.Color.Blue)
    }

    test(
      """transform sealed hierarchies from "superset" of case classes to "subset" of case classes when user-provided mapping handled non-trivial cases"""
    ) {
      def triangleToPolygon(t: shapes1enums.Shape.Triangle): shapes2enums.Shape =
        shapes2enums.Shape.Polygon(
          List(
            t.p1.transformInto[shapes2enums.Point],
            t.p2.transformInto[shapes2enums.Point],
            t.p3.transformInto[shapes2enums.Point]
          )
        )

      def rectangleToPolygon(r: shapes1enums.Shape.Rectangle): shapes2enums.Shape =
        shapes2enums.Shape.Polygon(
          List(
            r.p1.transformInto[shapes2enums.Point],
            shapes2enums.Point(r.p1.x, r.p2.y),
            r.p2.transformInto[shapes2enums.Point],
            shapes2enums.Point(r.p2.x, r.p1.y)
          )
        )

      val triangle: shapes1enums.Shape =
        shapes1enums.Shape.Triangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2), shapes1enums.Point(2, 0))

      triangle
        .intoPartial[shapes2enums.Shape]
        .withCoproductInstance(triangleToPolygon)
        .withCoproductInstance(rectangleToPolygon)
        .transform
        .asOption ==> Some(
        shapes2enums.Shape.Polygon(List(shapes2enums.Point(0, 0), shapes2enums.Point(2, 2), shapes2enums.Point(2, 0)))
      )

      val rectangle: shapes1enums.Shape =
        shapes1enums.Shape.Rectangle(shapes1enums.Point(0, 0), shapes1enums.Point(6, 4))

      rectangle
        .intoPartial[shapes2enums.Shape]
        .withCoproductInstance[shapes1enums.Shape] {
          case r: shapes1enums.Shape.Rectangle => rectangleToPolygon(r)
          case t: shapes1enums.Shape.Triangle  => triangleToPolygon(t)
        }
        .transform
        .asOption ==> Some(
        shapes2enums.Shape.Polygon(
          List(shapes2enums.Point(0, 0), shapes2enums.Point(0, 4), shapes2enums.Point(6, 4), shapes2enums.Point(6, 0))
        )
      )
    }
  }

  test(
    "transform sealed hierarchies of single value wrapping case classes to sealed hierarchy of flat case classes subtypes"
  ) {
    val triangle: shapes1enums.Shape =
      shapes1enums.Shape.Triangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2), shapes1enums.Point(2, 0))
    triangle.transformIntoPartial[shapes6enums.Shape].asOption ==> Some(
      shapes6enums.Shape.Triangle(
        shapes6enums.Triangle(shapes6enums.Point(0, 0), shapes6enums.Point(2, 2), shapes6enums.Point(2, 0))
      )
    )

    val rectangle: shapes1enums.Shape = shapes1enums.Shape.Rectangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2))
    rectangle.transformIntoPartial[shapes6enums.Shape].asOption ==> Some(
      shapes6enums.Shape.Rectangle(shapes6enums.Rectangle(shapes6enums.Point(0, 0), shapes6enums.Point(2, 2)))
    )
  }

  test(
    "transform sealed hierarchies of flat case classes subtypes to sealed hierarchy of single value wrapping case classes"
  ) {
    val triangle: shapes6enums.Shape =
      shapes6enums.Shape.Triangle(
        shapes6enums.Triangle(shapes6enums.Point(0, 0), shapes6enums.Point(2, 2), shapes6enums.Point(2, 0))
      )
    triangle.transformIntoPartial[shapes1enums.Shape].asOption ==> Some(
      shapes1enums.Shape.Triangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2), shapes1enums.Point(2, 0))
    )

    val rectangle: shapes6enums.Shape =
      shapes6enums.Shape.Rectangle(shapes6enums.Rectangle(shapes6enums.Point(0, 0), shapes6enums.Point(2, 2)))
    rectangle.transformIntoPartial[shapes1enums.Shape].asOption ==> Some(
      shapes1enums.Shape.Rectangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2))
    )
  }

  test("not allow transformation of of sealed hierarchies when the transformation would be ambiguous") {
    val error = compileErrorsFixed(
      """
           (shapes1enums.Shape.Triangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2), shapes1enums.Point(2, 0)): shapes1enums.Shape)
             .transformIntoPartial[shapes5enums.Shape]
        """
    )

    error.check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.shapes1enums.Shape to io.scalaland.chimney.fixtures.shapes5enums.Shape",
      "io.scalaland.chimney.fixtures.shapes5enums.Shape",
      "derivation from triangle: io.scalaland.chimney.fixtures.shapes1enums.Shape.Triangle to io.scalaland.chimney.fixtures.shapes5enums.Shape is not supported in Chimney!",
      "io.scalaland.chimney.fixtures.shapes5enums.Shape",
      "derivation from rectangle: io.scalaland.chimney.fixtures.shapes1enums.Shape.Rectangle to io.scalaland.chimney.fixtures.shapes5enums.Shape is not supported in Chimney!",
      "io.scalaland.chimney.fixtures.shapes5enums.Shape",
      "coproduct instance io.scalaland.chimney.fixtures.shapes1enums.Shape.Triangle of io.scalaland.chimney.fixtures.shapes1enums.Shape has ambiguous matches in io.scalaland.chimney.fixtures.shapes5enums.Shape: io.scalaland.chimney.fixtures.shapes5enums.Inner.Triangle, io.scalaland.chimney.fixtures.shapes5enums.Outer.Triangle",
      "coproduct instance io.scalaland.chimney.fixtures.shapes1enums.Shape.Rectangle of io.scalaland.chimney.fixtures.shapes1enums.Shape has ambiguous matches in io.scalaland.chimney.fixtures.shapes5enums.Shape: io.scalaland.chimney.fixtures.shapes5enums.Inner.Rectangle, io.scalaland.chimney.fixtures.shapes5enums.Outer.Rectangle",
      "Consult https://chimney.readthedocs.io for usage examples."
    )

    error.checkNot(
      "io.scalaland.chimney.fixtures.shapes5enums.Shape.Circle"
    )
  }

  group("setting .withCoproductInstancePartial[Subtype](mapping)") {

    test(
      """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
    ) {
      def blackIsRed(b: colors2enums.Color.Black.type): partial.Result[colors1enums.Color] =
        partial.Result.fromEmpty

      (colors2enums.Color.Black: colors2enums.Color)
        .intoPartial[colors1enums.Color]
        .withCoproductInstancePartial(blackIsRed)
        .transform
        .asOption ==> None

      (colors2enums.Color.Red: colors2enums.Color)
        .intoPartial[colors1enums.Color]
        .withCoproductInstancePartial(blackIsRed)
        .transform
        .asOption ==> Some(colors1enums.Color.Red)

      (colors2enums.Color.Green: colors2enums.Color)
        .intoPartial[colors1enums.Color]
        .withCoproductInstancePartial(blackIsRed)
        .transform
        .asOption ==> Some(colors1enums.Color.Green)

      (colors2enums.Color.Blue: colors2enums.Color)
        .intoPartial[colors1enums.Color]
        .withCoproductInstancePartial(blackIsRed)
        .transform
        .asOption ==> Some(colors1enums.Color.Blue)
    }

    test(
      """transform sealed hierarchies from "superset" of case classes to "subset" of case classes when user-provided mapping handled non-trivial cases"""
    ) {
      def triangleToPolygon(t: shapes1enums.Shape.Triangle): partial.Result[shapes2enums.Shape] =
        partial.Result.fromValue(
          shapes2enums.Shape.Polygon(
            List(
              t.p1.transformInto[shapes2enums.Point],
              t.p2.transformInto[shapes2enums.Point],
              t.p3.transformInto[shapes2enums.Point]
            )
          )
        )

      def rectangleToPolygon(r: shapes1enums.Shape.Rectangle): partial.Result[shapes2enums.Shape] =
        partial.Result.fromValue(
          shapes2enums.Shape.Polygon(
            List(
              r.p1.transformInto[shapes2enums.Point],
              shapes2enums.Point(r.p1.x, r.p2.y),
              r.p2.transformInto[shapes2enums.Point],
              shapes2enums.Point(r.p2.x, r.p1.y)
            )
          )
        )

      val triangle: shapes1enums.Shape =
        shapes1enums.Shape.Triangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2), shapes1enums.Point(2, 0))

      triangle
        .intoPartial[shapes2enums.Shape]
        .withCoproductInstancePartial(triangleToPolygon)
        .withCoproductInstancePartial(rectangleToPolygon)
        .transform
        .asOption ==> Some(
        shapes2enums.Shape.Polygon(List(shapes2enums.Point(0, 0), shapes2enums.Point(2, 2), shapes2enums.Point(2, 0)))
      )

      val rectangle: shapes1enums.Shape =
        shapes1enums.Shape.Rectangle(shapes1enums.Point(0, 0), shapes1enums.Point(6, 4))

      rectangle
        .intoPartial[shapes2enums.Shape]
        .withCoproductInstancePartial[shapes1enums.Shape] {
          case r: shapes1enums.Shape.Rectangle => rectangleToPolygon(r)
          case t: shapes1enums.Shape.Triangle  => triangleToPolygon(t)
        }
        .transform
        .asOption ==> Some(
        shapes2enums.Shape.Polygon(
          List(shapes2enums.Point(0, 0), shapes2enums.Point(0, 4), shapes2enums.Point(6, 4), shapes2enums.Point(6, 0))
        )
      )
    }
  }
}
