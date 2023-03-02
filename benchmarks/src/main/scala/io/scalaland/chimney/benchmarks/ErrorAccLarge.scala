package io.scalaland.chimney.benchmarks

import io.scalaland.chimney.{TransformationError, partial}
import org.openjdk.jmh.annotations.Benchmark

class ErrorAccLarge extends CommonBenchmarkSettings {
  import fixtures._

  type M[+A] = Either[Vector[TransformationError[String]], A]

  @Benchmark
  def largeHappyLiftedTransformer: M[LargeOutput] =
    transformers.largeTransformerLiftedHappy.transform(samples.largeSample)

  @Benchmark
  def largeHappyPartialTransformer: partial.Result[LargeOutput] =
    transformers.largeTransformerPartialHappy.transform(samples.largeSample)

  @Benchmark
  def largeUnhappyLiftedTransformer: M[LargeOutput] =
    transformers.largeTransformerLiftedUnhappy.transform(samples.largeSample)

  @Benchmark
  def largeUnhappyPartialTransformer: partial.Result[LargeOutput] =
    transformers.largeTransformerPartialUnhappy.transform(samples.largeSample)
}
