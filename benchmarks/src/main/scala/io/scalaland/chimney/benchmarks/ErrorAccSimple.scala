package io.scalaland.chimney.benchmarks

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.*
import io.scalaland.chimney.partial.{ErrorMessage, PathElement}
import org.openjdk.jmh.annotations.Benchmark

class ErrorAccSimple extends CommonBenchmarkSettings {
  import fixtures.*
  import samples.validation.*

  @Benchmark
  def simpleHappyChimneyDefinedPartial: partial.Result[SimpleOutput] =
    transformers.simpleTransformerPartialHappy.transform(samples.simpleSample)

  @Benchmark
  def simpleHappyChimneyIntoPartial: partial.Result[SimpleOutput] =
    samples.simpleSample
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => happy.validateA(s.a).asResult)
      .withFieldComputedPartial(_.b, s => happy.validateB(s.b).asResult)
      .withFieldComputedPartial(_.c, s => happy.validateC(s.c).asResult)
      .withFieldComputedPartial(_.d, s => happy.validateD(s.d).asResult)
      .transform

  @Benchmark
  def simpleHappyByHandEitherSwap: M[SimpleOutput] =
    simpleByHandErrorAccEitherSwap(
      samples.simpleSample,
      happy
        .validateA(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("a")))),
      happy
        .validateB(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("b")))),
      happy
        .validateC(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("c")))),
      happy
        .validateD(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("d"))))
    )

  @Benchmark
  def simpleHappyByHandCrazyNesting: M[SimpleOutput] =
    simpleByHandErrorAccCrazyNesting(
      samples.simpleSample,
      happy
        .validateA(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("a")))),
      happy
        .validateB(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("b")))),
      happy
        .validateC(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("c")))),
      happy
        .validateD(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("d"))))
    )

  @Benchmark
  def simpleUnhappyChimneyDefinedPartial: partial.Result[SimpleOutput] =
    transformers.simpleTransformerPartialUnhappy.transform(samples.simpleSample)

  @Benchmark
  def simpleUnhappyChimneyIntoPartial: partial.Result[SimpleOutput] =
    samples.simpleSample
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => unhappy.validateA(s.a).asResult)
      .withFieldComputedPartial(_.b, s => unhappy.validateB(s.b).asResult)
      .withFieldComputedPartial(_.c, s => unhappy.validateC(s.c).asResult)
      .withFieldComputedPartial(_.d, s => unhappy.validateD(s.d).asResult)
      .transform

  @Benchmark
  def simpleUnhappyByHandEitherSwap: M[SimpleOutput] =
    simpleByHandErrorAccEitherSwap(
      samples.simpleSample,
      unhappy
        .validateA(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("a")))),
      unhappy
        .validateB(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("b")))),
      unhappy
        .validateC(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("c")))),
      unhappy
        .validateD(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("d"))))
    )

  @Benchmark
  def simpleUnhappyByHandCrazyNesting: M[SimpleOutput] =
    simpleByHandErrorAccCrazyNesting(
      samples.simpleSample,
      unhappy
        .validateA(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("a")))),
      unhappy
        .validateB(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("b")))),
      unhappy
        .validateC(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("c")))),
      unhappy
        .validateD(_)
        .left
        .map(s => Vector(partial.Error(ErrorMessage.StringMessage(s)).prependErrorPath(PathElement.Accessor("d"))))
    )

}
