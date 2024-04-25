package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

import scala.annotation.unused

class TotalTransformerSealedHierarchySpec extends ChimneySpec {

  test(
    """transform flat sealed hierarchies from "subset" of case objects to "superset" of case objects without modifiers"""
  ) {
    (colors1.Red: colors1.Color).transformInto[colors2.Color] ==> colors2.Red
    (colors1.Green: colors1.Color).transformInto[colors2.Color] ==> colors2.Green
    (colors1.Blue: colors1.Color).transformInto[colors2.Color] ==> colors2.Blue
  }

  test(
    """transform nested sealed hierarchies between flat and nested hierarchies of case objects without modifiers"""
  ) {
    (colors2.Red: colors2.Color).transformInto[colors3.Color] ==> colors3.Red
    (colors2.Green: colors2.Color).transformInto[colors3.Color] ==> colors3.Green
    (colors2.Blue: colors2.Color).transformInto[colors3.Color] ==> colors3.Blue
    (colors2.Black: colors2.Color).transformInto[colors3.Color] ==> colors3.Black

    (colors3.Red: colors3.Color).transformInto[colors2.Color] ==> colors2.Red
    (colors3.Green: colors3.Color).transformInto[colors2.Color] ==> colors2.Green
    (colors3.Blue: colors3.Color).transformInto[colors2.Color] ==> colors2.Blue
    (colors3.Black: colors3.Color).transformInto[colors2.Color] ==> colors2.Black
  }

  test(
    """transforming flat hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable with Total Transformers"""
  ) {
    implicit val intToDoubleTransformer: Transformer[Int, Double] = (_: Int).toDouble

    (shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0)): shapes1.Shape)
      .transformInto[shapes3.Shape] ==>
      shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0))

    (shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4)): shapes1.Shape)
      .transformInto[shapes3.Shape] ==>
      shapes3.Rectangle(shapes3.Point(0.0, 0.0), shapes3.Point(6.0, 4.0))
  }

  test(
    """transforming nested sealed hierarchies from "subset" of case classes to "superset" of case classes without modifiers when common corresponding types are transformable"""
  ) {
    (shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0)): shapes3.Shape)
      .transformInto[shapes4.Shape] ==>
      shapes4.Triangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0), shapes4.Point(0.0, 0.0))

    (shapes3.Rectangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0)): shapes3.Shape)
      .transformInto[shapes4.Shape] ==>
      shapes4.Rectangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0))

    (shapes4.Triangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0), shapes4.Point(0.0, 0.0)): shapes4.Shape)
      .transformInto[shapes3.Shape] ==>
      shapes3.Triangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0), shapes3.Point(0.0, 0.0))

    (shapes4.Rectangle(shapes4.Point(2.0, 0.0), shapes4.Point(2.0, 2.0)): shapes4.Shape)
      .transformInto[shapes3.Shape] ==>
      shapes3.Rectangle(shapes3.Point(2.0, 0.0), shapes3.Point(2.0, 2.0))
  }

  test(
    "transform sealed hierarchies of single value wrapping case classes to sealed hierarchy of flat case classes subtypes"
  ) {
    val triangle: shapes1.Shape = shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))
    triangle.transformInto[shapes6.Shape] ==>
      shapes6.Triangle(shapes6.Shape.Triangle(shapes6.Point(0, 0), shapes6.Point(2, 2), shapes6.Point(2, 0)))

    val rectangle: shapes1.Shape = shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(2, 2))
    rectangle.transformInto[shapes6.Shape] ==>
      shapes6.Rectangle(shapes6.Shape.Rectangle(shapes6.Point(0, 0), shapes6.Point(2, 2)))
  }

  test(
    "transform sealed hierarchies of flat case classes subtypes to sealed hierarchy of single value wrapping case classes"
  ) {
    val triangle: shapes6.Shape =
      shapes6.Triangle(shapes6.Shape.Triangle(shapes6.Point(0, 0), shapes6.Point(2, 2), shapes6.Point(2, 0)))
    triangle.transformInto[shapes1.Shape] ==>
      shapes1.Triangle(shapes1.Point(0, 0), shapes1.Point(2, 2), shapes1.Point(2, 0))

    val rectangle: shapes6.Shape = shapes6.Rectangle(shapes6.Shape.Rectangle(shapes6.Point(0, 0), shapes6.Point(2, 2)))
    rectangle.transformInto[shapes1.Shape] ==>
      shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(2, 2))
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
        .transformInto[shapes5.Shape]
      """
    )

    error.check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.shapes1.Shape to io.scalaland.chimney.fixtures.shapes5.Shape",
      "io.scalaland.chimney.fixtures.shapes5.Shape",
      "derivation from rectangle: io.scalaland.chimney.fixtures.shapes1.Rectangle to io.scalaland.chimney.fixtures.shapes5.Shape is not supported in Chimney!",
      "io.scalaland.chimney.fixtures.shapes5.Shape",
      "coproduct instance io.scalaland.chimney.fixtures.shapes1.Triangle of io.scalaland.chimney.fixtures.shapes1.Shape has ambiguous matches in io.scalaland.chimney.fixtures.shapes5.Shape: io.scalaland.chimney.fixtures.shapes5.Inner.Triangle, io.scalaland.chimney.fixtures.shapes5.Triangle",
      "coproduct instance io.scalaland.chimney.fixtures.shapes1.Rectangle of io.scalaland.chimney.fixtures.shapes1.Shape has ambiguous matches in io.scalaland.chimney.fixtures.shapes5.Shape: io.scalaland.chimney.fixtures.shapes5.Inner.Rectangle, io.scalaland.chimney.fixtures.shapes5.Rectangle",
      "io.scalaland.chimney.fixtures.shapes5.Shape",
      "derivation from triangle: io.scalaland.chimney.fixtures.shapes1.Triangle to io.scalaland.chimney.fixtures.shapes5.Shape is not supported in Chimney!",
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
      compileErrors("""(colors2.Black: colors2.Color).transformInto[colors1.Color]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.colors2.Color to io.scalaland.chimney.fixtures.colors1.Color",
        "io.scalaland.chimney.fixtures.colors1.Color",
        "can't transform coproduct instance io.scalaland.chimney.fixtures.colors2.Black to io.scalaland.chimney.fixtures.colors1.Color",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test(
      """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
    ) {
      def blackIsRed(@unused b: colors2.Black.type): colors1.Color =
        colors1.Red

      (colors2.Black: colors2.Color)
        .into[colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .transform ==> colors1.Red

      (colors2.Red: colors2.Color)
        .into[colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .transform ==> colors1.Red

      (colors2.Green: colors2.Color)
        .into[colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .transform ==> colors1.Green

      (colors2.Blue: colors2.Color)
        .into[colors1.Color]
        .withSealedSubtypeHandled(blackIsRed)
        .transform ==> colors1.Blue
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
        .into[shapes2.Shape]
        .withSealedSubtypeHandled(triangleToPolygon)
        .withSealedSubtypeHandled(rectangleToPolygon)
        .transform ==> shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0)))

      val rectangle: shapes1.Shape =
        shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))

      rectangle
        .into[shapes2.Shape]
        .withSealedSubtypeHandled[shapes1.Shape] {
          case r: shapes1.Rectangle => rectangleToPolygon(r)
          case t: shapes1.Triangle  => triangleToPolygon(t)
        }
        .transform ==> shapes2.Polygon(
        List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
      )
    }
  }

  group("setting .withEnumCaseHandled[Subtype](mapping)") {

    test(
      """transform sealed hierarchies from "superset" of case objects to "subset" of case objects when user-provided mapping handled additional cases"""
    ) {
      def blackIsRed(@unused b: colors2.Black.type): colors1.Color =
        colors1.Red

      (colors2.Black: colors2.Color)
        .into[colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .transform ==> colors1.Red

      (colors2.Red: colors2.Color)
        .into[colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .transform ==> colors1.Red

      (colors2.Green: colors2.Color)
        .into[colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .transform ==> colors1.Green

      (colors2.Blue: colors2.Color)
        .into[colors1.Color]
        .withEnumCaseHandled(blackIsRed)
        .transform ==> colors1.Blue
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
        .into[shapes2.Shape]
        .withEnumCaseHandled(triangleToPolygon)
        .withEnumCaseHandled(rectangleToPolygon)
        .transform ==> shapes2.Polygon(List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0)))

      val rectangle: shapes1.Shape =
        shapes1.Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4))

      rectangle
        .into[shapes2.Shape]
        .withEnumCaseHandled[shapes1.Shape] {
          case r: shapes1.Rectangle => rectangleToPolygon(r)
          case t: shapes1.Triangle  => triangleToPolygon(t)
        }
        .transform ==> shapes2.Polygon(
        List(shapes2.Point(0, 0), shapes2.Point(0, 4), shapes2.Point(6, 4), shapes2.Point(6, 0))
      )
    }
  }

  group("flag .enableCustomSubtypeNameComparison") {

    import fixtures.renames.Subtypes.*

    test("should be disabled by default") {

      compileErrors("""(Foo.BAZ: Foo).transformInto[Bar]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Foo to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar is not supported in Chimney!",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""(Foo.BAZ: Foo).into[Bar].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Foo to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar is not supported in Chimney!",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""(Bar.Baz: Bar).transformInto[Foo]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Bar to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo is not supported in Chimney!",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""(Bar.Baz: Bar).into[Foo].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Bar to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo is not supported in Chimney!",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should inform user if and why the setting cannot be read") {
      @unused object BadNameComparison extends TransformedNamesComparison {

        def namesMatch(fromName: String, toName: String): Boolean = fromName.equalsIgnoreCase(toName)
      }

      compileErrors("""(Foo.BAZ: Foo).into[Bar].enableCustomSubtypeNameComparison(BadNameComparison).transform""")
        .check(
          "Invalid TransformerNamesComparison type - only (case) objects are allowed, and only the ones defined as top-level or in top-level objects, got: io.scalaland.chimney.TotalTransformerSealedHierarchySpec.BadNameComparison!!!"
        )
    }

    test("should inform user when the matcher they provided results in ambiguities") {

      (Foo2.baz: Foo2).transformInto[BarAmbiguous] ==> BarAmbiguous.baz
      (Foo2.baz: Foo2).into[BarAmbiguous].transform ==> BarAmbiguous.baz

      compileErrors(
        """
        (Foo2.baz: Foo2)
          .into[BarAmbiguous]
          .enableCustomSubtypeNameComparison(TransformedNamesComparison.BeanAware)
          .transform
        """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Foo2 to io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous",
        "io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous",
        "derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Foo2.baz to io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous is not supported in Chimney!",
        "io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous",
        "coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Foo2.baz of io.scalaland.chimney.fixtures.renames.Subtypes.Foo2 has ambiguous matches in io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous: io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous.baz, io.scalaland.chimney.fixtures.renames.Subtypes.BarAmbiguous.getBaz",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should allow subtypes to be matched using user-provided predicate") {
      (Foo.BAZ: Foo)
        .into[Bar]
        .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform ==> Bar.Baz

      (Bar.Baz: Bar)
        .into[Foo]
        .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform ==> Foo.BAZ

      locally {
        implicit val config = TransformerConfiguration.default.enableCustomSubtypeNameComparison(
          TransformedNamesComparison.CaseInsensitiveEquality
        )

        (Foo.BAZ: Foo).transformInto[Bar] ==> Bar.Baz
        (Foo.BAZ: Foo).into[Bar].transform ==> Bar.Baz

        (Bar.Baz: Bar).transformInto[Foo] ==> Foo.BAZ
        (Bar.Baz: Bar).into[Foo].transform ==> Foo.BAZ
      }
    }
  }

  group("flag .disableCustomSubtypeNameComparison") {

    import fixtures.renames.Subtypes.*

    test("should disable globally enabled .enableCustomSubtypeNameComparison") {
      @unused implicit val config = TransformerConfiguration.default.enableCustomSubtypeNameComparison(
        TransformedNamesComparison.CaseInsensitiveEquality
      )

      compileErrors("""(Foo.BAZ: Foo).into[Bar].disableCustomSubtypeNameComparison.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Foo to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar is not supported in Chimney!",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Foo.BAZ to io.scalaland.chimney.fixtures.renames.Subtypes.Bar",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrors("""(Bar.Baz: Bar).into[Foo].disableCustomSubtypeNameComparison.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.fixtures.renames.Subtypes.Bar to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "derivation from baz: io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo is not supported in Chimney!",
        "io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "can't transform coproduct instance io.scalaland.chimney.fixtures.renames.Subtypes.Bar.Baz to io.scalaland.chimney.fixtures.renames.Subtypes.Foo",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }
}
