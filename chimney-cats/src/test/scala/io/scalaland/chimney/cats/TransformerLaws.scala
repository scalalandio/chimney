package io.scalaland.chimney.cats

import _root_.cats.laws.discipline.{ArrowChoiceTests, CommutativeArrowTests, MonadTests}
import io.scalaland.chimney.{ChimneySpec, Transformer}

class TransformerLaws extends ChimneySpec with utils.ArbitraryUtils {

  group("ArrowChoice[Transformer] should follow arrow choice laws") {
    checkLawsAsTests(ArrowChoiceTests[Transformer].arrowChoice[Int, Long, Float, Double, String, Byte])
  }

  group("CommutativeArrow[Transformer] should follow arrow choice laws") {
    checkLawsAsTests(CommutativeArrowTests[Transformer].commutativeArrow[Int, Long, Float, Double, String, Byte])
  }

  group("Monad[Transformer[From, *]] should follow monadic laws") {
    checkLawsAsTests(MonadTests[Transformer[Unit, *]].monad[Int, String, Double])
  }
}
