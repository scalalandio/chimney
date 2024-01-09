package io.scalaland.chimney.cats.utils

import io.scalaland.chimney.{partial, ChimneySpec, PartialTransformer, Transformer}
import org.scalacheck.Arbitrary
import org.scalacheck.Test.check
import org.typelevel.discipline.Laws

trait ArbitraryUtils { this: ChimneySpec =>

  implicit def arbitraryTransformer[From, To: Arbitrary]: Arbitrary[Transformer[From, To]] = Arbitrary {
    Arbitrary.arbitrary[To].map[Transformer[From, To]](to => _ => to)
  }

  implicit def arbitraryPartialTransformer[From, To: Arbitrary]: Arbitrary[PartialTransformer[From, To]] = Arbitrary {
    Arbitrary.arbitrary[To].map[PartialTransformer[From, To]](to => PartialTransformer.fromFunction[From, To](_ => to))
  }

  implicit def arbitraryResult[A: Arbitrary]: Arbitrary[partial.Result[A]] = Arbitrary {
    Arbitrary.arbitrary[Either[String, A]].map(partial.Result.fromEitherString)
  }

  def checkLawsAsTests(rules: Laws#RuleSet): Unit = rules.props.foreach { case (name, prop) =>
    test(name) {
      check(prop)
    }
  }
}
