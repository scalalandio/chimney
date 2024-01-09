package io.scalaland.chimney.cats.utils

import cats.Eq
import cats.syntax.eq.*
import io.scalaland.chimney.{partial, ChimneySpec, PartialTransformer, Transformer}
import io.scalaland.chimney.cats.eqPartialResult
import org.scalacheck.Arbitrary
import org.scalacheck.Test.check
import org.scalacheck.Prop.forAll
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

  implicit def eqTransformers[From: Arbitrary, To: Eq]: Eq[Transformer[From, To]] = (t1, t2) => {
    val result = check(forAll { (from: From) =>
      t1.transform(from) === t2.transform(from)
    })
    result(identity).passed
  }

  implicit def eqPartialTransformers[From: Arbitrary, To: Eq]: Eq[PartialTransformer[From, To]] = (t1, t2) => {
    val result = check(forAll { (from: From) =>
      t1.transform(from) === t2.transform(from)
    })
    result(identity).passed
  }

  def checkLawsAsTests(rules: Laws#RuleSet): Unit = rules.props.foreach { case (name, prop) =>
    test(name) {
      check(prop)
    }
  }
}
