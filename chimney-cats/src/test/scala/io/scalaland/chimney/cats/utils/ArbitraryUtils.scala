package io.scalaland.chimney.cats.utils

import io.scalaland.chimney.ChimneySpec
import io.scalaland.chimney.partial
import org.scalacheck.Arbitrary
import org.scalacheck.Test.check
import org.typelevel.discipline.Laws

trait ArbitraryUtils { this: ChimneySpec =>

  implicit def arbitraryResult[A: Arbitrary]: Arbitrary[partial.Result[A]] = Arbitrary {
    Arbitrary.arbitrary[Either[String, A]].map(partial.Result.fromEitherString)
  }

  def checkLawsAsTests(rules: Laws#RuleSet): Unit = rules.props.foreach { case (name, prop) =>
    test(name) {
      check(prop)
    }
  }
}
