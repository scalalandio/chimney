package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*
import io.scalaland.chimney.javafixtures.*

import scala.annotation.unused

class TotalTransformerJavaEnumSpec extends ChimneySpec {

  test(
    """transform from Java Enum "subset" instances to sealed hierarchy "superset" of case objects without modifiers"""
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
      "coproduct instance io.scalaland.chimney.javafixtures.jcolors2.Color.Green of io.scalaland.chimney.javafixtures.jcolors2.Color has ambiguous matches in io.scalaland.chimney.fixtures.colors4.Color: io.scalaland.chimney.fixtures.colors4.Color.Green, io.scalaland.chimney.fixtures.colors4.Green",
      "coproduct instance io.scalaland.chimney.javafixtures.jcolors2.Color.Black of io.scalaland.chimney.javafixtures.jcolors2.Color has ambiguous matches in io.scalaland.chimney.fixtures.colors4.Color: io.scalaland.chimney.fixtures.colors4.Black, io.scalaland.chimney.fixtures.colors4.Color.Black",
      "io.scalaland.chimney.fixtures.colors4.Color",
      "derivation from green: io.scalaland.chimney.javafixtures.jcolors2.Color.Green to io.scalaland.chimney.fixtures.colors4.Color is not supported in Chimney!",
      "io.scalaland.chimney.fixtures.colors4.Color",
      "derivation from black: io.scalaland.chimney.javafixtures.jcolors2.Color.Black to io.scalaland.chimney.fixtures.colors4.Color is not supported in Chimney!",
      "Consult https://chimney.readthedocs.io for usage examples."
    )

    error.checkNot(
      "io.scalaland.chimney.fixtures.colors4.Color.Red",
      "io.scalaland.chimney.fixtures.colors4.Color.Blue"
    )
  }

  group("setting .withSealedSubtypeHandled[Subtype](mapping)") {

    test(
      """should be absent by default and not allow transforming Java Enum "superset" instances to sealed hierarchy "subset" of case objects"""
    ) {
      compileErrorsFixed("""(jcolors2.Color.Black: jcolors2.Color).transformInto[colors1.Color]""").check(
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
        .into[colors1.Color]
        .withSealedSubtypeHandled((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform ==> colors1.Red

      (jcolors2.Color.Red: jcolors2.Color)
        .into[colors1.Color]
        .withSealedSubtypeHandled((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform ==> colors1.Red

      (jcolors2.Color.Green: jcolors2.Color)
        .into[colors1.Color]
        .withSealedSubtypeHandled((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform ==> colors1.Green

      (jcolors2.Color.Blue: jcolors2.Color)
        .into[colors1.Color]
        .withSealedSubtypeHandled((b: jcolors2.Color.Black.type) => blackIsRed(b))
        .transform ==> colors1.Blue
    }
  }

  group("flag .enableCustomSubtypeNameComparison") {

    import jrenames.*

    test("should be disabled by default") {

      compileErrorsFixed("(Foo.bar: Foo).transformInto[Bar]").check(
        "Chimney can't derive transformation from io.scalaland.chimney.javafixtures.jrenames.Foo to io.scalaland.chimney.javafixtures.jrenames.Bar",
        "io.scalaland.chimney.javafixtures.jrenames.Bar",
        "derivation from bar: io.scalaland.chimney.javafixtures.jrenames.Foo.bar to io.scalaland.chimney.javafixtures.jrenames.Bar is not supported in Chimney!",
        "io.scalaland.chimney.javafixtures.jrenames.Bar",
        "can't transform coproduct instance io.scalaland.chimney.javafixtures.jrenames.Foo.bar to io.scalaland.chimney.javafixtures.jrenames.Bar",
        "Consult https://chimney.readthedocs.io for usage examples."
      )

      compileErrorsFixed("(Foo.bar: Foo).into[Bar].transform").check(
        "Chimney can't derive transformation from io.scalaland.chimney.javafixtures.jrenames.Foo to io.scalaland.chimney.javafixtures.jrenames.Bar",
        "io.scalaland.chimney.javafixtures.jrenames.Bar",
        "derivation from bar: io.scalaland.chimney.javafixtures.jrenames.Foo.bar to io.scalaland.chimney.javafixtures.jrenames.Bar is not supported in Chimney!",
        "io.scalaland.chimney.javafixtures.jrenames.Bar",
        "can't transform coproduct instance io.scalaland.chimney.javafixtures.jrenames.Foo.bar to io.scalaland.chimney.javafixtures.jrenames.Bar",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should inform user if and why the setting cannot be read") {
      @unused object BadNameComparison extends TransformedNamesComparison {

        def namesMatch(fromName: String, toName: String): Boolean = fromName.equalsIgnoreCase(toName)
      }

      compileErrorsFixed("""(Foo.bar: Foo).into[Bar].enableCustomSubtypeNameComparison(BadNameComparison).transform""")
        .check(
          "Invalid TransformerNamesComparison type - only (case) objects are allowed, and only the ones defined as top-level or in top-level objects, got: io.scalaland.chimney.TotalTransformerJavaEnumSpec.BadNameComparison!!!"
        )
    }

    test("should inform user when the matcher they provided results in ambiguities") {

      compileErrorsFixed(
        """
          (Foo.bar: Foo)
            .into[BarAmbiguous]
            .enableCustomSubtypeNameComparison(TransformedNamesComparison.BeanAware)
            .transform
          """
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.javafixtures.jrenames.Foo to io.scalaland.chimney.javafixtures.jrenames.BarAmbiguous",
        "io.scalaland.chimney.javafixtures.jrenames.BarAmbiguous",
        "coproduct instance io.scalaland.chimney.javafixtures.jrenames.Foo.bar of io.scalaland.chimney.javafixtures.jrenames.Foo has ambiguous matches in io.scalaland.chimney.javafixtures.jrenames.BarAmbiguous: io.scalaland.chimney.javafixtures.jrenames.BarAmbiguous.bar, io.scalaland.chimney.javafixtures.jrenames.BarAmbiguous.getBar",
        "io.scalaland.chimney.javafixtures.jrenames.BarAmbiguous",
        "derivation from bar: io.scalaland.chimney.javafixtures.jrenames.Foo.bar to io.scalaland.chimney.javafixtures.jrenames.BarAmbiguous is not supported in Chimney!",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should allow subtypes to be matched using user-provided predicate") {
      (Foo.bar: Foo)
        .into[Bar]
        .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)
        .transform ==> Bar.Bar

      locally {
        implicit val config = TransformerConfiguration.default
          .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)

        (Foo.bar: Foo).transformInto[Bar] ==> Bar.Bar
        (Foo.bar: Foo).into[Bar].transform ==> Bar.Bar
      }
    }
  }

  group("flag .disableCustomSubtypeNameComparison") {

    import jrenames.*

    test("should disable globally enabled .enableCustomSubtypeNameComparison") {

      @unused implicit val config = TransformerConfiguration.default
        .enableCustomSubtypeNameComparison(TransformedNamesComparison.CaseInsensitiveEquality)

      compileErrorsFixed("(Foo.bar: Foo).into[Bar].disableCustomSubtypeNameComparison.transform").check(
        "Chimney can't derive transformation from io.scalaland.chimney.javafixtures.jrenames.Foo to io.scalaland.chimney.javafixtures.jrenames.Bar",
        "io.scalaland.chimney.javafixtures.jrenames.Bar",
        "derivation from bar: io.scalaland.chimney.javafixtures.jrenames.Foo.bar to io.scalaland.chimney.javafixtures.jrenames.Bar is not supported in Chimney!",
        "io.scalaland.chimney.javafixtures.jrenames.Bar",
        "can't transform coproduct instance io.scalaland.chimney.javafixtures.jrenames.Foo.bar to io.scalaland.chimney.javafixtures.jrenames.Bar",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }
}
