package io.scalaland.chimney.benchmarks

import io.scalaland.chimney.partial
import org.openjdk.jmh.annotations.Benchmark

class ErrorAccLarge extends CommonBenchmarkSettings {
  import fixtures.*

  @Benchmark
  def largeHappyPartialTransformer: partial.Result[LargeOutput] =
    transformers.largeTransformerPartialHappy.transform(samples.largeSample)

  @Benchmark
  def largeUnhappyPartialTransformer: partial.Result[LargeOutput] =
    transformers.largeTransformerPartialUnhappy.transform(samples.largeSample)
}
