package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl._

class BasicTransformBenchmark extends CommonBenchmarkSettings {
  import fixtures._

  var simpleSample: Simple = samples.simpleSample
  var longSample: Long = samples.longSample
  var veryLongSample: VeryLong = samples.veryLongSample

  @Benchmark
  def simpleChimney(): SimpleOutput =
    simpleSample.transformInto[SimpleOutput]

  @Benchmark
  def simpleByHand(): SimpleOutput =
    doSimpleByHand(simpleSample)

  @Benchmark
  def longChimney(): LongOutput =
    longSample.transformInto[LongOutput]

  @Benchmark
  def longByHand(): LongOutput =
    doLongByHand(longSample)

  @Benchmark
  def veryLongChimney(): VeryLongOutput =
    veryLongSample.transformInto[VeryLongOutput]

  @Benchmark
  def veryLongByHand(): VeryLongOutput =
    doVeryLongByHand(veryLongSample)

}
