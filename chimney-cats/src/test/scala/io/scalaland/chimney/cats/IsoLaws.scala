package io.scalaland.chimney.cats

import _root_.cats.laws.discipline.*
import io.scalaland.chimney.{ChimneySpec, Iso}

class IsoLaws extends ChimneySpec with utils.ArbitraryUtils {

  group("Category[Iso] instance should follow laws") {
    checkLawsAsTests(ComposeTests[Iso].compose[Int, Long, Float, Double])
    checkLawsAsTests(CategoryTests[Iso].category[Int, Long, Float, Double])
  }

  group("InvariantSemigroupal[Iso[Source, *]] instance should follow laws") {
    checkLawsAsTests(InvariantTests[Iso[String, *]].invariant[Int, String, Double])
    checkLawsAsTests(SemigroupalTests[Iso[String, *]].semigroupal[Int, String, Double])
    checkLawsAsTests(InvariantSemigroupalTests[Iso[String, *]].invariantSemigroupal[Int, String, Double])
  }
}
