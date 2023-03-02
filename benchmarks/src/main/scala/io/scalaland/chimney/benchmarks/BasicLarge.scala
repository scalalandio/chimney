package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl._

class BasicLarge extends CommonBenchmarkSettings {
  import fixtures._

  @Benchmark
  def largeChimney(): LargeOutput =
    samples.largeSample.transformInto[LargeOutput]

  @Benchmark
  def largeByHand(): LargeOutput =
    doLargeByHand(samples.largeSample)
}
