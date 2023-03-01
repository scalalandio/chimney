package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl._

class BasicTransformBenchmark extends CommonBenchmarkSettings {
  import fixtures._

  var simpleSample: Simple = samples.simpleSample
  var longSample: Large = samples.largeSample
  var veryLongSample: Huge = samples.hugeSample

  @Benchmark
  def simpleChimney(): SimpleOutput =
    simpleSample.transformInto[SimpleOutput]

  @Benchmark
  def simpleByHand(): SimpleOutput =
    doSimpleByHand(simpleSample)

  @Benchmark
  def longChimney(): LargeOutput =
    longSample.transformInto[LargeOutput]

  @Benchmark
  def longByHand(): LargeOutput =
    doLargeByHand(longSample)

  @Benchmark
  def veryLongChimney(): HugeOutput =
    veryLongSample.transformInto[HugeOutput]

  @Benchmark
  def veryLongByHand(): HugeOutput =
    doHugeByHand(veryLongSample)

}
