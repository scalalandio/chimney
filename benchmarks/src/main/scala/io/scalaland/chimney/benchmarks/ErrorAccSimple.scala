package io.scalaland.chimney.benchmarks

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.*
import org.openjdk.jmh.annotations.Benchmark

class ErrorAccSimple extends CommonBenchmarkSettings {
  import fixtures.*
  import samples.validation.*

  @Benchmark
  def simpleHappyChimneyDefinedPartial: partial.Result[SimpleOutput] =
    transformers.simpleTransformerPartialHappy.transform(samples.simpleSample)

  @Benchmark
  def simpleHappyChimneyIntoPartial: partial.Result[SimpleOutput] =
    samples.simpleSample
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => happy.validateA(s.a).toPartialResult)
      .withFieldComputedPartial(_.b, s => happy.validateB(s.b).toPartialResult)
      .withFieldComputedPartial(_.c, s => happy.validateC(s.c).toPartialResult)
      .withFieldComputedPartial(_.d, s => happy.validateD(s.d).toPartialResult)
      .transform

  @Benchmark
  def simpleUnhappyChimneyDefinedPartial: partial.Result[SimpleOutput] =
    transformers.simpleTransformerPartialUnhappy.transform(samples.simpleSample)

  @Benchmark
  def simpleUnhappyChimneyIntoPartial: partial.Result[SimpleOutput] =
    samples.simpleSample
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => unhappy.validateA(s.a).toPartialResult)
      .withFieldComputedPartial(_.b, s => unhappy.validateB(s.b).toPartialResult)
      .withFieldComputedPartial(_.c, s => unhappy.validateC(s.c).toPartialResult)
      .withFieldComputedPartial(_.d, s => unhappy.validateD(s.d).toPartialResult)
      .transform
}
