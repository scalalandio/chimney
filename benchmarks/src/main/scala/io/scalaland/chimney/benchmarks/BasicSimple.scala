package io.scalaland.chimney.benchmarks

import io.scalaland.chimney.dsl.*
import org.openjdk.jmh.annotations.Benchmark

class BasicSimple extends CommonBenchmarkSettings {
  import fixtures.*

  @Benchmark
  def simpleChimneyInto: SimpleOutput =
    samples.simpleSample.transformInto[SimpleOutput]

  @Benchmark
  def simpleByHand: SimpleOutput =
    doSimpleByHand(samples.simpleSample)
}
