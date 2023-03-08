package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.*
import io.scalaland.chimney.partial

class ErrorAccNestedLarge extends CommonBenchmarkSettings {
  import fixtures.*

  @Benchmark
  def nestedLargeHappyPartialTransformer: partial.Result[Vector[LargeOutput]] = {
    implicit val ltp: PartialTransformer[Large, LargeOutput] = transformers.largeTransformerPartialHappy
    samples.largeNestedSample.transformIntoPartial[Vector[LargeOutput]]
  }

  @Benchmark
  def nestedLargeUnhappyPartialTransformer: partial.Result[Vector[LargeOutput]] = {
    implicit val ltpu: PartialTransformer[Large, LargeOutput] = transformers.largeTransformerPartialUnhappy
    samples.largeNestedSample.transformIntoPartial[Vector[LargeOutput]]
  }
}
