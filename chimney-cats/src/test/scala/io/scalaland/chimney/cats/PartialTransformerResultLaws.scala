package io.scalaland.chimney.cats

import _root_.cats.laws.discipline.SemigroupalTests
import _root_.cats.syntax.semigroupal.*
import io.scalaland.chimney.{partial, ChimneySpec}

class PartialTransformerResultLaws extends ChimneySpec with utils.ArbitraryUtils {

  group("Semigroupal[partial.Result]") {

    group("should follow Semigroupal laws") {
      checkLawsAsTests(SemigroupalTests[partial.Result].semigroupal[Int, String, Double])
    }

    test("should form Cartesian product for successful values") {
      partial.Result.fromValue(1).product(partial.Result.fromValue(2)) ==>
        partial.Result.fromValue((1, 2))
    }

    test("should return Errors if either of the partial.Results failed") {
      partial.Result.fromValue(1).product(partial.Result.fromErrorString("abc")) ==>
        partial.Result.fromErrorString("abc")

      partial.Result.fromErrorString("abc").product(partial.Result.fromValue(1)) ==>
        partial.Result.fromErrorString("abc")
    }

    test("should combine Errors if both of the partial.Results failed") {
      partial.Result.fromErrorString("abc").product(partial.Result.fromErrorString("def")) ==>
        partial.Result.fromErrorStrings("abc", "def")
    }
  }
}
