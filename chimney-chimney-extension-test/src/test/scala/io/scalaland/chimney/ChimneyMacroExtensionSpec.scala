package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.chimneyextensiontest.{TestBox2, TestSpecialBox2, TestSpecialLeaf}

/** Proves Chimney's OWN engine-aware macro-extension SPI ([[io.scalaland.chimney.integrations.ChimneyMacroExtension]])
  * end to end: the `io.scalaland.chimney.chimneyextensiontest.TestChimneyMacroExtension` handler is SEPARATELY COMPILED
  * (in this module's `Compile`) and loaded via `ServiceLoader` when THESE test sources are macro-expanded - there is NO
  * import that brings the conversions into scope, so the only way they derive is through the ServiceLoader-registered
  * handler (property d).
  */
class ChimneyMacroExtensionSpec extends ChimneySpec {

  group("a special-cased pair derives (total + partial)") {

    test("Int -> TestSpecialLeaf derives in a total context (and lifts into a partial one)") {
      42.transformInto[TestSpecialLeaf] ==> TestSpecialLeaf.wrap(42)
      42.transformIntoPartial[TestSpecialLeaf].asOption ==> Some(TestSpecialLeaf.wrap(42))
    }

    test("String -> TestSpecialLeaf derives as a PARTIAL conversion (may fail)") {
      "7".transformIntoPartial[TestSpecialLeaf].asOption ==> Some(TestSpecialLeaf.wrap(7))
      "nope".transformIntoPartial[TestSpecialLeaf].asOption ==> None
    }
  }

  group("recursive inner derivation: handler builds the outer, defers N inner values to the engine") {

    test("TestBox2 -> TestSpecialBox2 defers both fields (inner Int -> TestSpecialLeaf re-hits the SPI recursively)") {
      TestBox2(1, "x").transformInto[TestSpecialBox2[TestSpecialLeaf, String]] ==>
        TestSpecialBox2.of(TestSpecialLeaf.wrap(1), "x")
    }

    test("recursive derivation propagates partiality: a partial inner makes the whole outer partial") {
      // inner "5" -> TestSpecialLeaf is the PARTIAL String handler; the outer becomes partial and succeeds/fails with it
      TestBox2("5", "y").transformIntoPartial[TestSpecialBox2[TestSpecialLeaf, String]].asOption ==>
        Some(TestSpecialBox2.of(TestSpecialLeaf.wrap(5), "y"))
      TestBox2("nope", "y").transformIntoPartial[TestSpecialBox2[TestSpecialLeaf, String]].asOption ==> None
    }
  }

  group("precedence: a user implicit Transformer for the same pair BEATS the handler") {

    test("user implicit Transformer[Int, TestSpecialLeaf] wins (special-cased rule is below the implicit rules)") {
      implicit val marker: Transformer[Int, TestSpecialLeaf] = (i: Int) => TestSpecialLeaf.wrap(i + 1000)

      42.transformInto[TestSpecialLeaf] ==> TestSpecialLeaf.wrap(1042)
    }
  }
}
