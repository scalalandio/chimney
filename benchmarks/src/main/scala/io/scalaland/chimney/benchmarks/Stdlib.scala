package io.scalaland.chimney.benchmarks

import io.scalaland.chimney.dsl.*
import org.openjdk.jmh.annotations.Benchmark

class Stdlib extends CommonBenchmarkSettings {

  import fixtures.*

  @Benchmark
  def valueClassTransformChimney: rich.Person =
    samples.rich.person.transformInto[plain.Person].transformInto[rich.Person]

  @Benchmark
  def valueClassTransformByHand: rich.Person = plainToRich(richToPlain(samples.rich.person))

  @Benchmark
  def optionTransformChimney: Option[SimpleOutput] = Some(samples.simpleSample).transformInto[Option[SimpleOutput]]

  @Benchmark
  def optionTransformByHand: Option[SimpleOutput] = Some(samples.simpleSample).map(doSimpleByHand)

  @Benchmark
  def eitherTransformChimney: Either[String, SimpleOutput] =
    samples.simpleSampleRight.transformInto[Either[String, SimpleOutput]]

  @Benchmark
  def eitherTransformByHand: Either[String, SimpleOutput] = samples.simpleSampleRight.map(doSimpleByHand)
}
