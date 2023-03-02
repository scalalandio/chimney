package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl._
import io.scalaland.chimney._
import io.scalaland.chimney.partial

class ErrorAccNestedLarge extends CommonBenchmarkSettings {
  import fixtures._

  type M[+A] = Either[Vector[TransformationError[String]], A]

  @Benchmark
  def nestedLargeHappyLiftedTransformer: M[Vector[LargeOutput]] = {
    implicit val ltl: TransformerF[M, Large, LargeOutput] = transformers.largeTransformerLiftedHappy
    samples.largeNestedSample.transformIntoF[M, Vector[LargeOutput]]
  }

  @Benchmark
  def nestedLargeHappyPartialTransformer: partial.Result[Vector[LargeOutput]] = {
    implicit val ltp: PartialTransformer[Large, LargeOutput] = transformers.largeTransformerPartialHappy
    samples.largeNestedSample.transformIntoPartial[Vector[LargeOutput]]
  }

  @Benchmark
  def nestedLargeUnhappyLiftedTransformer: M[Vector[LargeOutput]] = {
    implicit val ltlu: TransformerF[M, Large, LargeOutput] = transformers.largeTransformerLiftedUnhappy
    samples.largeNestedSample.transformIntoF[M, Vector[LargeOutput]]
  }

  @Benchmark
  def nestedLargeUnhappyPartialTransformer: partial.Result[Vector[LargeOutput]] = {
    implicit val ltpu: PartialTransformer[Large, LargeOutput] = transformers.largeTransformerPartialUnhappy
    samples.largeNestedSample.transformIntoPartial[Vector[LargeOutput]]
  }
}
