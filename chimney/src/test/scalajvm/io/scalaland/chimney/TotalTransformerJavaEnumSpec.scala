package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*
import io.scalaland.chimney.javafixtures.*

//import scala.annotation.unused

class TotalTransformerJavaEnumSpec extends ChimneySpec {

  test(
    """transform flat sealed hierarchies from "subset" of case objects to "superset" of case objects without modifiers"""
  ) {
    (jcolors1.Color.Red: jcolors1.Color).transformInto[colors2.Color] ==> colors2.Red
    (jcolors1.Color.Green: jcolors1.Color).transformInto[colors2.Color] ==> colors2.Green
    (jcolors1.Color.Blue: jcolors1.Color).transformInto[colors2.Color] ==> colors2.Blue
  }

  test(
    """transform from sealed hierarchies "subset" of case objects to "superset" of Java Enums instances without modifiers"""
  ) {
    (colors1.Red: colors1.Color).transformInto[jcolors2.Color] ==> jcolors2.Color.Red
    (colors1.Green: colors1.Color).transformInto[jcolors2.Color] ==> jcolors2.Color.Green
    (colors1.Blue: colors1.Color).transformInto[jcolors2.Color] ==> jcolors2.Color.Blue
  }

  test(
    """transform Java Enums from "subset" of instances to "superset" of instances without modifiers"""
  ) {
    (jcolors1.Color.Red: jcolors1.Color).transformInto[jcolors2.Color] ==> jcolors2.Color.Red
    (jcolors1.Color.Green: jcolors1.Color).transformInto[jcolors2.Color] ==> jcolors2.Color.Green
    (jcolors1.Color.Blue: jcolors1.Color).transformInto[jcolors2.Color] ==> jcolors2.Color.Blue
  }

  test(
    """transform between Java Enums flat and nested hierarchies of case objects without modifiers"""
  ) {
    (jcolors2.Color.Red: jcolors2.Color).transformInto[colors3.Color] ==> colors3.Red
    (jcolors2.Color.Green: jcolors2.Color).transformInto[colors3.Color] ==> colors3.Green
    (jcolors2.Color.Blue: jcolors2.Color).transformInto[colors3.Color] ==> colors3.Blue
    (jcolors2.Color.Black: jcolors2.Color).transformInto[colors3.Color] ==> colors3.Black

    (colors3.Red: colors3.Color).transformInto[jcolors2.Color] ==> jcolors2.Color.Red
    (colors3.Green: colors3.Color).transformInto[jcolors2.Color] ==> jcolors2.Color.Green
    (colors3.Blue: colors3.Color).transformInto[jcolors2.Color] ==> jcolors2.Color.Blue
    (colors3.Black: colors3.Color).transformInto[jcolors2.Color] ==> jcolors2.Color.Black
  }

  test(
    "not allow transformation of of sealed hierarchies when the transformation would be ambiguous".withTags(
      if (isScala3) Set(munit.Ignore)
      else Set.empty // ignore only on Scala 3 until https://github.com/lampepfl/dotty/issues/18484 is fixed
    )
  ) {
    val error = compileErrorsScala2(
      """(jcolors2.Color.Black: jcolors2.Color).transformInto[colors4.Color]"""
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
//      compileErrorsFixed("""(colors2.Black: colors2.Color).transformInto[colors1.Color]""").check(
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
//        .into[colors1.Color]
//        .withCoproductInstance(blackIsRed)
//        .transform ==> colors1.Red
//
//      (colors2.Red: colors2.Color)
//        .into[colors1.Color]
//        .withCoproductInstance(blackIsRed)
//        .transform ==> colors1.Red
//
//      (colors2.Green: colors2.Color)
//        .into[colors1.Color]
//        .withCoproductInstance(blackIsRed)
//        .transform ==> colors1.Green
//
//      (colors2.Blue: colors2.Color)
//        .into[colors1.Color]
//        .withCoproductInstance(blackIsRed)
//        .transform ==> colors1.Blue
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
//        .into[shapes2.Shape]
//        .withCoproductInstance(triangleToPolygon)
//        .withCoproductInstance(rectangleToPolygon)
//        .transform ==> shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0)))
//
//      val rectangle: shapes1.Shape =
//        shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))
//
//      rectangle
//        .into[shapes2.Shape]
//        .withCoproductInstance[shapes1.Shape] {
//          case r: shapes1.Rectangle => rectangleToPolygon(r)
//          case t: shapes1.Triangle => triangleToPolygon(t)
//        }
//        .transform ==> shapes2.Polygon(
//        List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
//      )
//    }
//  }
}
