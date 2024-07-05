package io.scalaland.chimney.cats

import _root_.cats.laws.discipline.*
import io.scalaland.chimney.{ChimneySpec, Codec}

class CodecLaws extends ChimneySpec with utils.ArbitraryUtils {

  group("Category[Codec] instance should follow laws") {
    checkLawsAsTests(ComposeTests[Codec].compose[Int, Long, Float, Double])
    checkLawsAsTests(CategoryTests[Codec].category[Int, Long, Float, Double])
  }

  group("InvariantSemigroupal[Codec[Source, *]] instance should follow laws") {
    checkLawsAsTests(InvariantTests[Codec[String, *]].invariant[Int, String, Double])
    checkLawsAsTests(SemigroupalTests[Codec[String, *]].semigroupal[Int, String, Double])
    checkLawsAsTests(InvariantSemigroupalTests[Codec[String, *]].invariantSemigroupal[Int, String, Double])
  }
}
