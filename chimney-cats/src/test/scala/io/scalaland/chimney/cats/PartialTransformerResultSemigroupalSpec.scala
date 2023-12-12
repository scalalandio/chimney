package io.scalaland.chimney.cats

import _root_.cats.syntax.semigroupal.*
import _root_.cats.laws.discipline.SemigroupalTests
import io.scalaland.chimney.{partial, ChimneySpec}

class PartialTransformerResultSemigroupalSpec extends ChimneySpec with utils.ArbitraryUtils {

  group("Semigroupal[partial.Result] should combine 2 partial.Results") {

    group("Results should follow semigroupal laws") {

      checkLawsAsTests(SemigroupalTests[partial.Result].semigroupal[Int, String, Double])
    }

    test("successful Results should form Cartesian product") {

      partial.Result.fromValue(1).product(partial.Result.fromValue(2)) ==>
        partial.Result.fromValue((1, 2))
    }

    test("any failed Result component should fail the combined sum Result") {

      partial.Result.fromValue(1).product(partial.Result.fromErrorString("abc")) ==>
        partial.Result.fromErrorString("abc")

      partial.Result.fromErrorString("abc").product(partial.Result.fromValue(1)) ==>
        partial.Result.fromErrorString("abc")
    }

    test("failures should aggregate") {

      partial.Result.fromErrorString("abc").product(partial.Result.fromErrorString("def")) ==>
        partial.Result.fromErrorStrings("abc", "def")
    }
  }
}
