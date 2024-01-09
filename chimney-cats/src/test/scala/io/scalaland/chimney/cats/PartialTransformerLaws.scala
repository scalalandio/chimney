package io.scalaland.chimney.cats

import _root_.cats.laws.discipline.{ArrowChoiceTests, CommutativeArrowTests, MonadTests}
import io.scalaland.chimney.{ChimneySpec, PartialTransformer}

class PartialTransformerLaws extends ChimneySpec with utils.ArbitraryUtils {

  group("ArrowChoice[Transformer] should follow arrow choice laws") {
    checkLawsAsTests(ArrowChoiceTests[PartialTransformer].arrowChoice[Int, Long, Float, Double, String, Byte])
  }

  group("CommutativeArrow[Transformer] should follow arrow choice laws") {
    checkLawsAsTests(CommutativeArrowTests[PartialTransformer].commutativeArrow[Int, Long, Float, Double, String, Byte])
  }

  group("Monad[PartialTransformer[From, *]] should follow monadic laws") {
    checkLawsAsTests(MonadTests[PartialTransformer[Unit, *]].monad[Int, String, Double])
  }
}
