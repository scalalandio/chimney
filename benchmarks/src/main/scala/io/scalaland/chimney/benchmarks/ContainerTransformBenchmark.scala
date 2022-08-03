package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl._

class ContainerTransformBenchmark extends CommonBenchmarkSettings {

  import fixtures._

  var richPerson: rich.Person = samples.rich.person
  var simpleSample: Simple = samples.simpleSample
  var arrayOfSimples: Array[Simple] = Array.fill(100)(samples.simpleSample)
  var vectorOfSimples: Vector[Simple] = Vector.fill(100)(samples.simpleSample)
  var mapOfStrings2Simples: Map[String, Simple] = 1.to(100).map(i => i.toString -> samples.simpleSample).toMap
  var eitherOfStringOrSimple: Either[String, Simple] = Right(samples.simpleSample)

  @Benchmark
  def valueClassTransformChimney: rich.Person = richPerson.transformInto[plain.Person].transformInto[rich.Person]

  @Benchmark
  def valueClassTransformByHand: rich.Person = plainToRich(richToPlain(richPerson))

  @Benchmark
  def optionTransformChimney: Option[SimpleOutput] = Some(simpleSample).transformInto[Option[SimpleOutput]]

  @Benchmark
  def optionTransformByHand: Option[SimpleOutput] = Some(simpleSample).map(doSimpleByHand)

  @Benchmark
  def arrayTransformChimney: Array[SimpleOutput] = arrayOfSimples.transformInto[Array[SimpleOutput]]

  @Benchmark
  def arrayTransformByHand: Array[SimpleOutput] = arrayOfSimples.map(doSimpleByHand)

  @Benchmark
  def vectorTransformChimney: Vector[SimpleOutput] = vectorOfSimples.transformInto[Vector[SimpleOutput]]

  @Benchmark
  def vectorTransformByHand: Vector[SimpleOutput] = vectorOfSimples.map(doSimpleByHand)

  @Benchmark
  def mapTransformChimney: Map[String, SimpleOutput] = mapOfStrings2Simples.transformInto[Map[String, SimpleOutput]]

  @Benchmark
  def mapTransformByHand: Map[String, SimpleOutput] =
    mapOfStrings2Simples.iterator.map(t => (t._1, doSimpleByHand(t._2))).toMap

  @Benchmark
  def eitherTransformChimney: Either[String, SimpleOutput] =
    eitherOfStringOrSimple.transformInto[Either[String, SimpleOutput]]

  @Benchmark
  def eitherTransformByHand: Either[String, SimpleOutput] = eitherOfStringOrSimple.map(doSimpleByHand)
}
