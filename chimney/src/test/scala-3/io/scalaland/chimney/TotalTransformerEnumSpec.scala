package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.annotation.unused

class TotalTransformerEnumSpec extends ChimneySpec {

  test(
    """transform flat sealed hierarchies from "subset" of case objects to "superset" of case objects without modifiers"""
  ) {
    (colors1enums.Color.Red: colors1enums.Color).transformInto[colors2enums.Color] ==> colors2enums.Color.Red
    (colors1enums.Color.Green: colors1enums.Color).transformInto[colors2enums.Color] ==> colors2enums.Color.Green
    (colors1enums.Color.Blue: colors1enums.Color).transformInto[colors2enums.Color] ==> colors2enums.Color.Blue
  }

  test(
    """transform nested sealed hierarchies between flat and nested hierarchies of case objects without modifiers"""
  ) {
    (colors2enums.Color.Red: colors2enums.Color).transformInto[colors3enums.Color] ==> colors3enums.SimpleColor.Red
    (colors2enums.Color.Green: colors2enums.Color).transformInto[colors3enums.Color] ==> colors3enums.SimpleColor.Green
    (colors2enums.Color.Blue: colors2enums.Color).transformInto[colors3enums.Color] ==> colors3enums.SimpleColor.Blue
    (colors2enums.Color.Black: colors2enums.Color).transformInto[colors3enums.Color] ==> colors3enums.ComplexColor.Black

    (colors3enums.SimpleColor.Red: colors3enums.Color).transformInto[colors2enums.Color] ==> colors2enums.Color.Red
    (colors3enums.SimpleColor.Green: colors3enums.Color).transformInto[colors2enums.Color] ==> colors2enums.Color.Green
    (colors3enums.SimpleColor.Blue: colors3enums.Color).transformInto[colors2enums.Color] ==> colors2enums.Color.Blue
    (colors3enums.ComplexColor.Black: colors3enums.Color).transformInto[colors2enums.Color] ==> colors2enums.Color.Black
  }

  test(
    """transforming flat hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable with Total Transformers"""
  ) {
    implicit val intToDoubleTransformer: Transformer[Int, Double] = (_: Int).toDouble

    (shapes1enums.Shape.Triangle(
      shapes1enums.Point(0, 0),
      shapes1enums.Point(2, 2),
      shapes1enums.Point(2, 0)
    ): shapes1enums.Shape)
      .transformInto[shapes3enums.Shape] ==>
      shapes3enums.Shape.Triangle(
        shapes3enums.Point(2.0, 0.0),
        shapes3enums.Point(2.0, 2.0),
        shapes3enums.Point(0.0, 0.0)
      )

    (shapes1enums.Shape.Rectangle(shapes1enums.Point(0, 0), shapes1enums.Point(6, 4)): shapes1enums.Shape)
      .transformInto[shapes3enums.Shape] ==>
      shapes3enums.Shape.Rectangle(shapes3enums.Point(0.0, 0.0), shapes3enums.Point(6.0, 4.0))
  }

  test(
    """transforming nested sealed hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable"""
  ) {
    (shapes3enums.Shape.Triangle(
      shapes3enums.Point(2.0, 0.0),
      shapes3enums.Point(2.0, 2.0),
      shapes3enums.Point(0.0, 0.0)
    ): shapes3enums.Shape)
      .transformInto[shapes4enums.Shape] ==>
      shapes4enums.ThreeAnglesShape.Triangle(
        shapes4enums.Point(2.0, 0.0),
        shapes4enums.Point(2.0, 2.0),
        shapes4enums.Point(0.0, 0.0)
      )

    (shapes3enums.Shape.Rectangle(shapes3enums.Point(2.0, 0.0), shapes3enums.Point(2.0, 2.0)): shapes3enums.Shape)
      .transformInto[shapes4enums.Shape] ==>
      shapes4enums.FourAnglesShape.Rectangle(shapes4enums.Point(2.0, 0.0), shapes4enums.Point(2.0, 2.0))

    (shapes4enums.ThreeAnglesShape.Triangle(
      shapes4enums.Point(2.0, 0.0),
      shapes4enums.Point(2.0, 2.0),
      shapes4enums.Point(0.0, 0.0)
    ): shapes4enums.Shape)
      .transformInto[shapes3enums.Shape] ==>
      shapes3enums.Shape.Triangle(
        shapes3enums.Point(2.0, 0.0),
        shapes3enums.Point(2.0, 2.0),
        shapes3enums.Point(0.0, 0.0)
      )

    (shapes4enums.FourAnglesShape.Rectangle(
      shapes4enums.Point(2.0, 0.0),
      shapes4enums.Point(2.0, 2.0)
    ): shapes4enums.Shape)
      .transformInto[shapes3enums.Shape] ==>
      shapes3enums.Shape.Rectangle(shapes3enums.Point(2.0, 0.0), shapes3enums.Point(2.0, 2.0))
  }

  test(
    "transform sealed hierarchies of single value wrapping case classes to sealed hierarchy of flat case classes subtypes"
  ) {
    val triangle: shapes1enums.Shape =
      shapes1enums.Shape.Triangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2), shapes1enums.Point(2, 0))
    triangle.transformInto[shapes6enums.Shape] ==>
      shapes6enums.Shape.Triangle(
        shapes6enums.Triangle(shapes6enums.Point(0, 0), shapes6enums.Point(2, 2), shapes6enums.Point(2, 0))
      )

    val rectangle: shapes1enums.Shape = shapes1enums.Shape.Rectangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2))
    rectangle.transformInto[shapes6enums.Shape] ==>
      shapes6enums.Shape.Rectangle(shapes6enums.Rectangle(shapes6enums.Point(0, 0), shapes6enums.Point(2, 2)))
  }

  test(
    "transform sealed hierarchies of flat case classes subtypes to sealed hierarchy of single value wrapping case classes"
  ) {
    val triangle: shapes6enums.Shape =
      shapes6enums.Shape.Triangle(
        shapes6enums.Triangle(shapes6enums.Point(0, 0), shapes6enums.Point(2, 2), shapes6enums.Point(2, 0))
      )
    triangle.transformInto[shapes1enums.Shape] ==>
      shapes1enums.Shape.Triangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2), shapes1enums.Point(2, 0))

    val rectangle: shapes6enums.Shape =
      shapes6enums.Shape.Rectangle(shapes6enums.Rectangle(shapes6enums.Point(0, 0), shapes6enums.Point(2, 2)))
    rectangle.transformInto[shapes1enums.Shape] ==>
      shapes1enums.Shape.Rectangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2))
  }

  test("not allow transformation of of sealed hierarchies when the transformation would be ambiguous") {
    val error = compileErrorsFixed(
      """
           (shapes1enums.Triangle(shapes1enums.Point(0, 0), shapes1enums.Point(2, 2), shapes1enums.Point(2, 0)): shapes1enums.Shape)
             .transformInto[shapes5enums.Shape]
        """
    )

    error.check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.shapes1enums.Shape to io.scalaland.chimney.fixtures.shapes5enums.Shape",
      "io.scalaland.chimney.fixtures.shapes5enums.Shape",
      "coproduct instance Triangle of io.scalaland.chimney.fixtures.shapes5enums.Shape is ambiguous",
      "coproduct instance Rectangle of io.scalaland.chimney.fixtures.shapes5enums.Shape is ambiguous",
      "Consult https://chimney.readthedocs.io for usage examples."
    )

    assert(
      !error.contains("coproduct instance Circle of io.scalaland.chimney.fixtures.shapes5enums.Shape is ambiguous")
    )
  }

  group("setting .withCoproductInstance[Subtype](mapping)") {

    test(
      """should be absent by default and not allow transforming "superset" of case class to "subset" of case objects"""
    ) {
      compileErrorsFixed("""(colors2enums.Color.Black: colors2enums.Color).transformInto[colors1enums.Color]""").check(
        "",
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
        .into[colors1enums.Color]
        .withCoproductInstance(blackIsRed)
        .transform ==> colors1enums.Color.Red

      (colors2enums.Color.Red: colors2enums.Color)
        .into[colors1enums.Color]
        .withCoproductInstance(blackIsRed)
        .transform ==> colors1enums.Color.Red

      (colors2enums.Color.Green: colors2enums.Color)
        .into[colors1enums.Color]
        .withCoproductInstance(blackIsRed)
        .transform ==> colors1enums.Color.Green

      (colors2enums.Color.Blue: colors2enums.Color)
        .into[colors1enums.Color]
        .withCoproductInstance(blackIsRed)
        .transform ==> colors1enums.Color.Blue
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
        .into[shapes2enums.Shape]
        .withCoproductInstance(triangleToPolygon)
        .withCoproductInstance(rectangleToPolygon)
        .transform ==> shapes2enums.Shape.Polygon(
        List(shapes2enums.Point(0, 0), shapes2enums.Point(2, 2), shapes2enums.Point(2, 0))
      )

      val rectangle: shapes1enums.Shape =
        shapes1enums.Shape.Rectangle(shapes1enums.Point(0, 0), shapes1enums.Point(6, 4))

      rectangle
        .into[shapes2enums.Shape]
        .withCoproductInstance[shapes1enums.Shape] {
          case r: shapes1enums.Shape.Rectangle => rectangleToPolygon(r)
          case t: shapes1enums.Shape.Triangle  => triangleToPolygon(t)
        }
        .transform ==> shapes2enums.Shape.Polygon(
        List(shapes2enums.Point(0, 0), shapes2enums.Point(0, 4), shapes2enums.Point(6, 4), shapes2enums.Point(6, 0))
      )
    }
  }
}
