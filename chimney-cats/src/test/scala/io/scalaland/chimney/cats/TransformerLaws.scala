package io.scalaland.chimney.cats

import _root_.cats.laws.discipline.*
import io.scalaland.chimney.{ChimneySpec, Transformer}

class TransformerLaws extends ChimneySpec with utils.ArbitraryUtils {

  group("ArrowChoice[Transformer] & CommutativeArrow[Transformer] instance should follow laws") {
    checkLawsAsTests(ComposeTests[Transformer].compose[Int, Long, Float, Double])
    checkLawsAsTests(CategoryTests[Transformer].category[Int, Long, Float, Double])
    checkLawsAsTests(ProfunctorTests[Transformer].profunctor[Int, Long, Float, Double, String, Byte])
    checkLawsAsTests(StrongTests[Transformer].strong[Int, Long, Float, Double, String, Byte])
    checkLawsAsTests(ArrowTests[Transformer].arrow[Int, Long, Float, Double, String, Byte])
    checkLawsAsTests(ChoiceTests[Transformer].choice[Int, Long, Float, Double])
    checkLawsAsTests(ArrowChoiceTests[Transformer].arrowChoice[Int, Long, Float, Double, String, Byte])
    checkLawsAsTests(CommutativeArrowTests[Transformer].commutativeArrow[Int, Long, Float, Double, String, Byte])
  }

  group("Monad[Transformer[Source, *]] & CoflatMap[Transformer[Source, *]] instance should follow laws") {
    checkLawsAsTests(InvariantTests[Transformer[String, *]].invariant[Int, String, Double])
    checkLawsAsTests(FunctorTests[Transformer[String, *]].functor[Int, String, Double])
    checkLawsAsTests(ApplicativeTests[Transformer[String, *]].applicative[Int, String, Double])
    checkLawsAsTests(FlatMapTests[Transformer[String, *]].flatMap[Int, String, Double])
    checkLawsAsTests(MonadTests[Transformer[String, *]].monad[Int, String, Double])
    checkLawsAsTests(CoflatMapTests[Transformer[String, *]].coflatMap[Int, String, Double])
  }

  group("Contravariant[Transformer[*, To]] instance should follow laws") {
    checkLawsAsTests(ContravariantTests[Transformer[*, String]].contravariant[Int, String, Double])
  }
}
