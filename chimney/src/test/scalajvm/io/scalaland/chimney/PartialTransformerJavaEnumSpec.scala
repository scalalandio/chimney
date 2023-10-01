package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*
import io.scalaland.chimney.javafixtures.*
//import io.scalaland.chimney.utils.OptionUtils.*

//import scala.annotation.unused

class PartialTransformerJavaEnumSpec extends ChimneySpec {

  test(
    """transform from Java Enum "subset" instances to sealed hierarchy "superset" of case objects without modifiers"""
  ) {
    (jcolors1.Color.Red: jcolors1.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Red)
    (jcolors1.Color.Green: jcolors1.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Green)
    (jcolors1.Color.Blue: jcolors1.Color).transformIntoPartial[colors2.Color].asOption ==> Some(colors2.Blue)
  }

  test(
    """transform from sealed hierarchies "subset" of case objects to "superset" of Java Enums instances without modifiers"""
  ) {
    (colors1.Red: colors1.Color).transformIntoPartial[jcolors2.Color].asOption ==> Some(jcolors2.Color.Red)
    (colors1.Green: colors1.Color).transformIntoPartial[jcolors2.Color].asOption ==> Some(jcolors2.Color.Green)
    (colors1.Blue: colors1.Color).transformIntoPartial[jcolors2.Color].asOption ==> Some(jcolors2.Color.Blue)
  }

  test(
    """transform Java Enums from "subset" of instances to "superset" of instances without modifiers"""
  ) {
    (jcolors1.Color.Red: jcolors1.Color).transformIntoPartial[jcolors2.Color].asOption ==> Some(jcolors2.Color.Red)
    (jcolors1.Color.Green: jcolors1.Color).transformIntoPartial[jcolors2.Color].asOption ==> Some(jcolors2.Color.Green)
    (jcolors1.Color.Blue: jcolors1.Color).transformIntoPartial[jcolors2.Color].asOption ==> Some(jcolors2.Color.Blue)
  }

  test(
    """transform between Java Enums flat and nested hierarchies of case objects without modifiers"""
  ) {
    (jcolors2.Color.Red: jcolors2.Color).transformIntoPartial[colors3.Color].asOption ==> Some(colors3.Red)
    (jcolors2.Color.Green: jcolors2.Color).transformIntoPartial[colors3.Color].asOption ==> Some(colors3.Green)
    (jcolors2.Color.Blue: jcolors2.Color).transformIntoPartial[colors3.Color].asOption ==> Some(colors3.Blue)
    (jcolors2.Color.Black: jcolors2.Color).transformIntoPartial[colors3.Color].asOption ==> Some(colors3.Black)

    (colors3.Red: colors3.Color).transformIntoPartial[jcolors2.Color].asOption ==> Some(jcolors2.Color.Red)
    (colors3.Green: colors3.Color).transformIntoPartial[jcolors2.Color].asOption ==> Some(jcolors2.Color.Green)
    (colors3.Blue: colors3.Color).transformIntoPartial[jcolors2.Color].asOption ==> Some(jcolors2.Color.Blue)
    (colors3.Black: colors3.Color).transformIntoPartial[jcolors2.Color].asOption ==> Some(jcolors2.Color.Black)
  }

  test(
    "not allow transformation of of sealed hierarchies when the transformation would be ambiguous".withTags(
      if (isScala3) Set(munit.Ignore)
      else Set.empty // ignore only on Scala 3 until https://github.com/lampepfl/dotty/issues/18484 is fixed
    )
  ) {
    val error = compileErrorsScala2(
      """(jcolors2.Color.Black: jcolors2.Color).transformIntoPartial[colors4.Color]"""
    )

    error.check(
      "Chimney can't derive transformation from io.scalaland.chimney.javafixtures.jcolors2.Color to io.scalaland.chimney.fixtures.colors4.Color",
      "io.scalaland.chimney.fixtures.colors4.Color",
      "coproduct instance Green of io.scalaland.chimney.fixtures.colors4.Color is ambiguous",
      "coproduct instance Black of io.scalaland.chimney.fixtures.colors4.Color is ambiguous",
      "Consult https://chimney.readthedocs.io for usage examples."
    )

    error.checkNot(
      "coproduct instance Red of io.scalaland.chimney.fixtures.colors4.Color is ambiguous",
      "coproduct instance Blue of io.scalaland.chimney.fixtures.colors4.Color is ambiguous"
    )
  }

//  group("setting .withCoproductInstance[Subtype](mapping)") {
//
//    test(
//      """should be absent by default and not allow transforming "superset" of case class to "subset" of case objects"""
//    ) {
//      compileErrorsFixed("""(colors2.Black: colors2.Color).transformIntoPartial[colors1.Color]""").check(
//        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.colors2.Color to io.scalaland.chimney.fixtures.colors1.Color",
//        "io.scalaland.chimney.fixtures.colors1.Color",
//        "can't transform coproduct instance io.scalaland.chimney.fixtures.colors2.Black to io.scalaland.chimney.fixtures.colors1.Color",
//        "Consult https://chimney.readthedocs.io for usage examples."
//      )
//    }
//
//    test(
//      """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
//    ) {
//      def blackIsRed(@unused b: colors2.Black.type): colors1.Color =
//        colors1.Red
//
//      (colors2.Black: colors2.Color)
//        .intoPartial[colors1.Color]
//        .withCoproductInstance(blackIsRed)
//        .transform
//        .asOption ==> Some(colors1.Red)
//
//      (colors2.Red: colors2.Color)
//        .intoPartial[colors1.Color]
//        .withCoproductInstance(blackIsRed)
//        .transform
//        .asOption ==> Some(colors1.Red)
//
//      (colors2.Green: colors2.Color)
//        .intoPartial[colors1.Color]
//        .withCoproductInstance(blackIsRed)
//        .transform
//        .asOption ==> Some(colors1.Green)
//
//      (colors2.Blue: colors2.Color)
//        .intoPartial[colors1.Color]
//        .withCoproductInstance(blackIsRed)
//        .transform
//        .asOption ==> Some(colors1.Blue)
//    }
//
//    test(
//      """transform sealed hierarchies from "superset" of case classes to "subset" of case classes when user-provided mapping handled non-trivial cases"""
//    ) {
//      def triangleToPolygon(t: shapes1.Triangle): shapes2.Shape =
//        shapes2.Polygon(
//          List(
//            t.p1.transformInto[shapes2.Point],
//            t.p2.transformInto[shapes2.Point],
//            t.p3.transformInto[shapes2.Point]
//          )
//        )
//
//      def rectangleToPolygon(r: shapes1.Rectangle): shapes2.Shape =
//        shapes2.Polygon(
//          List(
//            r.p1.transformInto[shapes2.Point],
//            shapes2.Point(r.p1.x, r.p2.y),
//            r.p2.transformInto[shapes2.Point],
//            shapes2.Point(r.p2.x, r.p1.y)
//          )
//        )
//
//      val triangle: shapes1.Shape =
//        shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))
//
//      triangle
//        .intoPartial[shapes2.Shape]
//        .withCoproductInstance(triangleToPolygon)
//        .withCoproductInstance(rectangleToPolygon)
//        .transform
//        .asOption ==> Some(shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0))))
//
//      val rectangle: shapes1.Shape =
//        shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))
//
//      rectangle
//        .intoPartial[shapes2.Shape]
//        .withCoproductInstance[shapes1.Shape] {
//          case r: shapes1.Rectangle => rectangleToPolygon(r)
//          case t: shapes1.Triangle => triangleToPolygon(t)
//        }
//        .transform
//        .asOption ==> Some(
//        shapes2.Polygon(
//          List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
//        )
//      )
//    }
//  }
//
//  test(
//    "transform sealed hierarchies of single value wrapping case classes to sealed hierarchy of flat case classes subtypes"
//  ) {
//    val triangle: shapes1.Shape = shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))
//    triangle.transformIntoPartial[shapes6.Shape].asOption ==> Some(
//      shapes6.Triangle(shapes6.Shape.Triangle(shapes6.Point(0, 0), shapes6.Point(2, 2), shapes6.Point(2, 0)))
//    )
//
//    val rectangle: shapes1.Shape = shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(2, 2))
//    rectangle.transformIntoPartial[shapes6.Shape].asOption ==> Some(
//      shapes6.Rectangle(shapes6.Shape.Rectangle(shapes6.Point(0, 0), shapes6.Point(2, 2)))
//    )
//  }
//
//  test(
//    "transform sealed hierarchies of flat case classes subtypes to sealed hierarchy of single value wrapping case classes"
//  ) {
//    val triangle: shapes6.Shape =
//      shapes6.Triangle(shapes6.Shape.Triangle(shapes6.Point(0, 0), shapes6.Point(2, 2), shapes6.Point(2, 0)))
//    triangle.transformIntoPartial[shapes1.Shape].asOption ==> Some(
//      shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))
//    )
//
//    val rectangle: shapes6.Shape = shapes6.Rectangle(shapes6.Shape.Rectangle(shapes6.Point(0, 0), shapes6.Point(2, 2)))
//    rectangle.transformIntoPartial[shapes1.Shape].asOption ==> Some(
//      shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(2, 2))
//    )
//  }
//
//  group("setting .withCoproductInstancePartial[Subtype](mapping)") {
//
//    test(
//      """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
//    ) {
//      def blackIsRed(b: colors2.Black.type): partial.Result[colors1.Color] =
//        partial.Result.fromEmpty
//
//      (colors2.Black: colors2.Color)
//        .intoPartial[colors1.Color]
//        .withCoproductInstancePartial(blackIsRed)
//        .transform
//        .asOption ==> None
//
//      (colors2.Red: colors2.Color)
//        .intoPartial[colors1.Color]
//        .withCoproductInstancePartial(blackIsRed)
//        .transform
//        .asOption ==> Some(colors1.Red)
//
//      (colors2.Green: colors2.Color)
//        .intoPartial[colors1.Color]
//        .withCoproductInstancePartial(blackIsRed)
//        .transform
//        .asOption ==> Some(colors1.Green)
//
//      (colors2.Blue: colors2.Color)
//        .intoPartial[colors1.Color]
//        .withCoproductInstancePartial(blackIsRed)
//        .transform
//        .asOption ==> Some(colors1.Blue)
//    }
//
//    test(
//      """transform sealed hierarchies from "superset" of case classes to "subset" of case classes when user-provided mapping handled non-trivial cases"""
//    ) {
//      def triangleToPolygon(t: shapes1.Triangle): partial.Result[shapes2.Shape] =
//        partial.Result.fromValue(
//          shapes2.Polygon(
//            List(
//              t.p1.transformInto[shapes2.Point],
//              t.p2.transformInto[shapes2.Point],
//              t.p3.transformInto[shapes2.Point]
//            )
//          )
//        )
//
//      def rectangleToPolygon(r: shapes1.Rectangle): partial.Result[shapes2.Shape] =
//        partial.Result.fromValue(
//          shapes2.Polygon(
//            List(
//              r.p1.transformInto[shapes2.Point],
//              shapes2.Point(r.p1.x, r.p2.y),
//              r.p2.transformInto[shapes2.Point],
//              shapes2.Point(r.p2.x, r.p1.y)
//            )
//          )
//        )
//
//      val triangle: shapes1.Shape =
//        shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))
//
//      triangle
//        .intoPartial[shapes2.Shape]
//        .withCoproductInstancePartial(triangleToPolygon)
//        .withCoproductInstancePartial(rectangleToPolygon)
//        .transform
//        .asOption ==> Some(shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0))))
//
//      val rectangle: shapes1.Shape =
//        shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))
//
//      rectangle
//        .intoPartial[shapes2.Shape]
//        .withCoproductInstancePartial[shapes1.Shape] {
//          case r: shapes1.Rectangle => rectangleToPolygon(r)
//          case t: shapes1.Triangle => triangleToPolygon(t)
//        }
//        .transform
//        .asOption ==> Some(
//        shapes2.Polygon(
//          List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
//        )
//      )
//    }
//  }
}
