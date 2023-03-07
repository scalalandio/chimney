package io.scalaland.chimney.benchmarks

import io.scalaland.chimney.dsl.*
import org.openjdk.jmh.annotations.Benchmark

class BasicHuge extends CommonBenchmarkSettings {
  import fixtures.*

  @Benchmark
  def hugeChimneyInto: HugeOutput =
    samples.hugeSample.transformInto[HugeOutput]

  @Benchmark
  def hugeByHand: HugeOutput =
    doHugeByHand(samples.hugeSample)

}
