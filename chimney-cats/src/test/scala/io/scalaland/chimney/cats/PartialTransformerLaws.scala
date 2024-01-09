package io.scalaland.chimney.cats

import _root_.cats.Eq
import _root_.cats.laws.discipline.MonadTests
import _root_.cats.syntax.eq.*
import io.scalaland.chimney.{ChimneySpec, PartialTransformer}

class PartialTransformerLaws extends ChimneySpec with utils.ArbitraryUtils {

  group("Monad[PartialTransformer[From, *]]") {

    group("should follow monadic laws") {

      implicit def eq[A: Eq]: Eq[PartialTransformer[Unit, A]] = (t1, t2) => t1.transform(()) === t2.transform(())

      checkLawsAsTests(MonadTests[PartialTransformer[Unit, *]].monad[Int, String, Double])
    }
  }
}
