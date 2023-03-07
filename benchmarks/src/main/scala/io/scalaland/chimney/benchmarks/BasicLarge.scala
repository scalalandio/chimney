package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl.*

class BasicLarge extends CommonBenchmarkSettings {
  import fixtures.*

  @Benchmark
  def largeChimneyInto: LargeOutput =
    samples.largeSample.transformInto[LargeOutput]

  @Benchmark
  def largeByHand: LargeOutput =
    doLargeByHand(samples.largeSample)
}
