package io.scalaland.chimney.benchmarks

import io.scalaland.chimney.dsl._
import io.scalaland.chimney._
import org.openjdk.jmh.annotations.Benchmark

class ErrorAccSimple extends CommonBenchmarkSettings {
  import fixtures._
  import samples.validation._

  type M[+A] = Either[Vector[TransformationError[String]], A]

  @Benchmark
  def simpleHappyChimneyDefinedLifted: M[SimpleOutput] =
    transformers.simpleTransformerLiftedHappy.transform(samples.simpleSample)

  @Benchmark
  def simpleHappyChimneyDefinedPartial: partial.Result[SimpleOutput] =
    transformers.simpleTransformerPartialHappy.transform(samples.simpleSample)

  @Benchmark
  def simpleHappyChimneyIntoLifted: M[SimpleOutput] =
    samples.simpleSample
      .intoF[M, SimpleOutput]
      .withFieldComputedF(_.a, s => happy.validateA(s.a).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.b, s => happy.validateB(s.b).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.c, s => happy.validateC(s.c).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.d, s => happy.validateD(s.d).left.map(s => Vector(TransformationError(s))))
      .transform

  @Benchmark
  def simpleHappyChimneyIntoPartial: partial.Result[SimpleOutput] =
    samples.simpleSample
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => happy.validateA(s.a).toPartialResult)
      .withFieldComputedPartial(_.b, s => happy.validateB(s.b).toPartialResult)
      .withFieldComputedPartial(_.c, s => happy.validateC(s.c).toPartialResult)
      .withFieldComputedPartial(_.d, s => happy.validateD(s.d).toPartialResult)
      .transform

  @Benchmark
  def simpleHappyByHandEitherSwap: M[SimpleOutput] =
    simpleByHandErrorAccEitherSwap(
      samples.simpleSample,
      happy.validateA(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("a")))),
      happy.validateB(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("b")))),
      happy.validateC(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("c")))),
      happy.validateD(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("d"))))
    )

  @Benchmark
  def simpleHappyByHandCrazyNesting: M[SimpleOutput] =
    simpleByHandErrorAccCrazyNesting(
      samples.simpleSample,
      happy.validateA(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("a")))),
      happy.validateB(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("b")))),
      happy.validateC(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("c")))),
      happy.validateD(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("d"))))
    )

  @Benchmark
  def simpleUnhappyChimneyDefinedLifted: M[SimpleOutput] =
    transformers.simpleTransformerLiftedUnhappy.transform(samples.simpleSample)

  @Benchmark
  def simpleUnhappyChimneyDefinedPartial: partial.Result[SimpleOutput] =
    transformers.simpleTransformerPartialUnhappy.transform(samples.simpleSample)

  @Benchmark
  def simpleUnhappyChimneyIntoLifted: M[SimpleOutput] =
    samples.simpleSample
      .intoF[M, SimpleOutput]
      .withFieldComputedF(_.a, s => unhappy.validateA(s.a).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.b, s => unhappy.validateB(s.b).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.c, s => unhappy.validateC(s.c).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.d, s => unhappy.validateD(s.d).left.map(s => Vector(TransformationError(s))))
      .transform

  @Benchmark
  def simpleUnhappyChimneyIntoPartial: partial.Result[SimpleOutput] =
    samples.simpleSample
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => unhappy.validateA(s.a).toPartialResult)
      .withFieldComputedPartial(_.b, s => unhappy.validateB(s.b).toPartialResult)
      .withFieldComputedPartial(_.c, s => unhappy.validateC(s.c).toPartialResult)
      .withFieldComputedPartial(_.d, s => unhappy.validateD(s.d).toPartialResult)
      .transform

  @Benchmark
  def simpleUnhappyByHandEitherSwap: M[SimpleOutput] =
    simpleByHandErrorAccEitherSwap(
      samples.simpleSample,
      unhappy.validateA(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("a")))),
      unhappy.validateB(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("b")))),
      unhappy.validateC(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("c")))),
      unhappy.validateD(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("d"))))
    )

  @Benchmark
  def simpleUnhappyByHandCrazyNesting: M[SimpleOutput] =
    simpleByHandErrorAccCrazyNesting(
      samples.simpleSample,
      unhappy.validateA(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("a")))),
      unhappy.validateB(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("b")))),
      unhappy.validateC(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("c")))),
      unhappy.validateD(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("d"))))
    )
}
