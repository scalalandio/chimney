package io.scalaland.chimney.cats

import _root_.cats.laws.discipline.*
import io.scalaland.chimney.{partial, ChimneySpec, PartialTransformer}

class PartialTransformerLaws extends ChimneySpec with utils.ArbitraryUtils {

  group("ArrowChoice[PartialTransformer] & CommutativeArrow[PartialTransformer] instance should follow laws") {
    checkLawsAsTests(ComposeTests[PartialTransformer].compose[Int, Long, Float, Double])
    checkLawsAsTests(CategoryTests[PartialTransformer].category[Int, Long, Float, Double])
    checkLawsAsTests(ProfunctorTests[PartialTransformer].profunctor[Int, Long, Float, Double, String, Byte])
    checkLawsAsTests(StrongTests[PartialTransformer].strong[Int, Long, Float, Double, String, Byte])
    checkLawsAsTests(ArrowTests[PartialTransformer].arrow[Int, Long, Float, Double, String, Byte])
    checkLawsAsTests(ChoiceTests[PartialTransformer].choice[Int, Long, Float, Double])
    checkLawsAsTests(ArrowChoiceTests[PartialTransformer].arrowChoice[Int, Long, Float, Double, String, Byte])
    checkLawsAsTests(CommutativeArrowTests[PartialTransformer].commutativeArrow[Int, Long, Float, Double, String, Byte])
  }

  group(
    "MonadError[Transformer[Source, *], partial.Result.Errors] & CoflatMap[Transformer[Source, *]] instance should follow laws"
  ) {
    checkLawsAsTests(InvariantTests[PartialTransformer[String, *]].invariant[Int, String, Double])
    checkLawsAsTests(FunctorTests[PartialTransformer[String, *]].functor[Int, String, Double])
    checkLawsAsTests(ApplicativeTests[PartialTransformer[String, *]].applicative[Int, String, Double])
    checkLawsAsTests(FlatMapTests[PartialTransformer[String, *]].flatMap[Int, String, Double])
    checkLawsAsTests(MonadTests[PartialTransformer[String, *]].monad[Int, String, Double])
    checkLawsAsTests(
      ApplicativeErrorTests[PartialTransformer[String, *], partial.Result.Errors].applicativeError[Int, String, Double]
    )
    checkLawsAsTests(
      MonadErrorTests[PartialTransformer[String, *], partial.Result.Errors].monadError[Int, String, Double]
    )
    checkLawsAsTests(CoflatMapTests[PartialTransformer[String, *]].coflatMap[Int, String, Double])
  }
  
  // TODO: Parallel

  group("Contravariant[Transformer[*, To]] instance should follow laws") {
    checkLawsAsTests(ContravariantTests[PartialTransformer[*, String]].contravariant[Int, String, Double])
  }
}
