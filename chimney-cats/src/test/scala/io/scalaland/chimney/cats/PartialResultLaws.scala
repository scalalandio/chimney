package io.scalaland.chimney.cats

import _root_.cats.laws.discipline.*
import _root_.cats.syntax.semigroupal.*
import cats.data.Const
import io.scalaland.chimney.{partial, ChimneySpec}

class PartialResultLaws extends ChimneySpec with utils.ArbitraryUtils {

  group("Parallel[partial.Result] & Semigroupal[partial.Result]") {

    group("should follow laws") {
      checkLawsAsTests(NonEmptyParallelTests[partial.Result].nonEmptyParallel[Int, String])
      checkLawsAsTests(ParallelTests[partial.Result].parallel[Int, String])
      checkLawsAsTests(SemigroupalTests[partial.Result].semigroupal[Int, String, Double])
    }

    test("should form Cartesian product for successful values") {
      partial.Result.fromValue(1).product(partial.Result.fromValue(2)) ==>
        partial.Result.fromValue((1, 2))
    }

    test("should return Errors if either of the partial.Results failed") {
      partial.Result.fromValue(1).product(partial.Result.fromErrorString("abc")) ==>
        partial.Result.fromErrorString("abc")

      partial.Result.fromErrorString("abc").product(partial.Result.fromValue(1)) ==>
        partial.Result.fromErrorString("abc")
    }

    test("should combine Errors if both of the partial.Results failed") {
      partial.Result.fromErrorString("abc").product(partial.Result.fromErrorString("def")) ==>
        partial.Result.fromErrorStrings("abc", "def")
    }
  }

  group(
    "MonadError[partial.Result, partial.Result.Errors] & CoflatMap[partial.Result] & Traverse[partial.Result] should follow laws"
  ) {
    checkLawsAsTests(InvariantTests[partial.Result].invariant[Int, String, Double])
    checkLawsAsTests(FunctorTests[partial.Result].functor[Int, String, Double])
    checkLawsAsTests(ApplicativeTests[partial.Result].applicative[Int, String, Double])
    checkLawsAsTests(FlatMapTests[partial.Result].flatMap[Int, String, Double])
    checkLawsAsTests(MonadTests[partial.Result].monad[Int, String, Double])
    checkLawsAsTests(ApplicativeErrorTests[partial.Result, partial.Result.Errors].applicativeError[Int, String, Double])
    checkLawsAsTests(MonadErrorTests[partial.Result, partial.Result.Errors].monadError[Int, String, Double])
    checkLawsAsTests(CoflatMapTests[partial.Result].coflatMap[Int, String, Double])
    checkLawsAsTests(UnorderedFoldableTests[partial.Result].unorderedFoldable[Int, Long])
    checkLawsAsTests(FoldableTests[partial.Result].foldable[Int, Long])
    checkLawsAsTests(
      UnorderedTraverseTests[partial.Result].unorderedTraverse[Int, Long, Double, Const[Unit, *], Const[Int, *]]
    )
    checkLawsAsTests(TraverseTests[partial.Result].traverse[Int, Long, Double, Byte, Const[Unit, *], Const[Int, *]])
  }
}
