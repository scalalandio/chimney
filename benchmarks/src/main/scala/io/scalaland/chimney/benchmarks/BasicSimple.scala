package io.scalaland.chimney.benchmarks

import io.scalaland.chimney.dsl._
import org.openjdk.jmh.annotations.Benchmark

class BasicSimple extends CommonBenchmarkSettings {
  import fixtures._

  @Benchmark
  def simpleChimney(): SimpleOutput =
    samples.simpleSample.transformInto[SimpleOutput]

  @Benchmark
  def simpleByHand(): SimpleOutput =
    doSimpleByHand(samples.simpleSample)
}
