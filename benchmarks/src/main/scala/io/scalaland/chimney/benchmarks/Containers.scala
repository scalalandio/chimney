package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl._

class Containers extends CommonBenchmarkSettings {

  import fixtures._

  @Benchmark
  def arrayTransformChimney: Array[SimpleOutput] = samples.simpleSampleArray.transformInto[Array[SimpleOutput]]

  @Benchmark
  def arrayTransformByHand: Array[SimpleOutput] = samples.simpleSampleArray.map(doSimpleByHand)

  @Benchmark
  def vectorTransformChimney: Vector[SimpleOutput] = samples.simpleSampleVector.transformInto[Vector[SimpleOutput]]

  @Benchmark
  def vectorTransformByHand: Vector[SimpleOutput] = samples.simpleSampleVector.map(doSimpleByHand)

  @Benchmark
  def mapTransformChimney: Map[String, SimpleOutput] =
    samples.simpleSampleMapOfStrings.transformInto[Map[String, SimpleOutput]]

  @Benchmark
  def mapTransformByHand: Map[String, SimpleOutput] =
    samples.simpleSampleMapOfStrings.map(t => (t._1, doSimpleByHand(t._2)))
}
