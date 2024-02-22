package io.scalaland.chimney.cats.utils

import cats.Eq
import cats.data.Const
import cats.syntax.eq.*
import io.scalaland.chimney.{partial, ChimneySpec, PartialTransformer, Transformer}
import io.scalaland.chimney.cats.catsEqForPartialResult
import org.scalacheck.{Arbitrary, Cogen}
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

  implicit def arbitraryResultErrors: Arbitrary[partial.Result.Errors] = Arbitrary {
    Arbitrary.arbitrary[String].map(partial.Result.Errors.fromString)
  }

  implicit def arbitraryConst[A: Arbitrary, B]: Arbitrary[Const[A, B]] = Arbitrary {
    Arbitrary.arbitrary[A].map(a => Const[A, B](a))
  }

  implicit def cogenTransformer[From, To]: Cogen[Transformer[From, To]] = Cogen[Unit].contramap(_ => ())

  implicit def cogenPartialTransformer[From, To]: Cogen[PartialTransformer[From, To]] = Cogen[Unit].contramap(_ => ())

  implicit def cogenPartialResult[A]: Cogen[partial.Result[A]] = Cogen[Unit].contramap(_ => ())

  implicit def cogenPartialResultErrors: Cogen[partial.Result.Errors] = Cogen[Unit].contramap(_ => ())

  implicit def eqTransformers[From: Arbitrary, To: Eq]: Eq[Transformer[From, To]] = (t1, t2) =>
    check(forAll((from: From) => t1.transform(from) === t2.transform(from)))(_.withMinSuccessfulTests(20)).passed

  implicit def eqPartialTransformers[From: Arbitrary, To: Eq]: Eq[PartialTransformer[From, To]] = (t1, t2) =>
    check(forAll((from: From) => t1.transform(from) === t2.transform(from)))(_.withMinSuccessfulTests(20)).passed

  def checkLawsAsTests(rules: Laws#RuleSet): Unit = rules.props.foreach { case (name, prop) =>
    test(name) {
      check(prop)(identity)
    }
  }
}
