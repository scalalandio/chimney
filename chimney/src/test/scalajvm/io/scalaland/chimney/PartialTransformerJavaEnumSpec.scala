package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*
import io.scalaland.chimney.javafixtures.*

import scala.annotation.unused

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
      "coproduct instance io.scalaland.chimney.javafixtures.jcolors2.Color.Green of io.scalaland.chimney.javafixtures.jcolors2.Color has ambiguous matches in io.scalaland.chimney.fixtures.colors4.Color: io.scalaland.chimney.fixtures.colors4.Color.Green, io.scalaland.chimney.fixtures.colors4.Green",
      "coproduct instance io.scalaland.chimney.javafixtures.jcolors2.Color.Black of io.scalaland.chimney.javafixtures.jcolors2.Color has ambiguous matches in io.scalaland.chimney.fixtures.colors4.Color: io.scalaland.chimney.fixtures.colors4.Black, io.scalaland.chimney.fixtures.colors4.Color.Black",
      "io.scalaland.chimney.fixtures.colors4.Color",
      "derivation from color: io.scalaland.chimney.javafixtures.jcolors2.Color.Green to io.scalaland.chimney.fixtures.colors4.Color is not supported in Chimney!",
      "io.scalaland.chimney.fixtures.colors4.Color",
      "derivation from color: io.scalaland.chimney.javafixtures.jcolors2.Color.Black to io.scalaland.chimney.fixtures.colors4.Color is not supported in Chimney!",
      "Consult https://chimney.readthedocs.io for usage examples."
    )

    error.checkNot(
      "io.scalaland.chimney.fixtures.colors4.Color.Red",
      "io.scalaland.chimney.fixtures.colors4.Color.Blue"
    )
  }

  group("setting .withCoproductInstance[Subtype](mapping)") {

    test(
      """should be absent by default and not allow transforming Java Enum "superset" instances to sealed hierarchy "subset" of case objects"""
    ) {
      compileErrorsFixed("""(jcolors2.Color.Black: jcolors2.Color).transformIntoPartial[colors1.Color]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.javafixtures.jcolors2.Color to io.scalaland.chimney.fixtures.colors1.Color",
        "io.scalaland.chimney.fixtures.colors1.Color",
        "can't transform coproduct instance io.scalaland.chimney.javafixtures.jcolors2.Color.Black to io.scalaland.chimney.fixtures.colors1.Color",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test(
      """transform from Java Enum "superset" instances to sealed hierarchy "subset" of case objects when user-provided mapping handled additional cases"""
    ) {
      def blackIsRed(@unused b: jcolors2.Color.Black.type): colors1.Color =
        colors1.Red

      (jcolors2.Color.Black: jcolors2.Color)
        .intoPartial[colors1.Color]
        .withCoproductInstance((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform
        .asOption ==> Some(colors1.Red)

      (jcolors2.Color.Red: jcolors2.Color)
        .intoPartial[colors1.Color]
        .withCoproductInstance((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform
        .asOption ==> Some(colors1.Red)

      (jcolors2.Color.Green: jcolors2.Color)
        .intoPartial[colors1.Color]
        .withCoproductInstance((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform
        .asOption ==> Some(colors1.Green)

      (jcolors2.Color.Blue: jcolors2.Color)
        .intoPartial[colors1.Color]
        .withCoproductInstance((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform
        .asOption ==> Some(colors1.Blue)
    }
  }

  group("setting .withCoproductInstancePartial[Subtype](mapping)") {

    test(
      """transform from Java Enum "superset" instances to sealed hierarchy "subset" of case objects when user-provided mapping handled additional cases"""
    ) {
      def blackIsRed(b: jcolors2.Color.Black.type): partial.Result[colors1.Color] =
        partial.Result.fromEmpty

      (jcolors2.Color.Black: jcolors2.Color)
        .intoPartial[colors1.Color]
        .withCoproductInstancePartial((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform
        .asOption ==> None

      (jcolors2.Color.Red: jcolors2.Color)
        .intoPartial[colors1.Color]
        .withCoproductInstancePartial((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform
        .asOption ==> Some(colors1.Red)

      (jcolors2.Color.Green: jcolors2.Color)
        .intoPartial[colors1.Color]
        .withCoproductInstancePartial((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform
        .asOption ==> Some(colors1.Green)

      (jcolors2.Color.Blue: jcolors2.Color)
        .intoPartial[colors1.Color]
        .withCoproductInstancePartial((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform
        .asOption ==> Some(colors1.Blue)
    }
  }
}
