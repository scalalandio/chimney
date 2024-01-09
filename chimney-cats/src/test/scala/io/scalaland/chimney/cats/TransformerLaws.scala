package io.scalaland.chimney.cats

import _root_.cats.Eq
import _root_.cats.laws.discipline.MonadTests
import _root_.cats.syntax.eq.*
import io.scalaland.chimney.{ChimneySpec, Transformer}

class TransformerLaws extends ChimneySpec with utils.ArbitraryUtils {

  group("Monad[Transformer[From, *]]") {

    group("should follow monadic laws") {

      implicit def eq[A: Eq]: Eq[Transformer[Unit, A]] = (t1, t2) => t1.transform(()) === t2.transform(())
      
      checkLawsAsTests(MonadTests[Transformer[Unit, *]].monad[Int, String, Double])
    }
  }
}
