package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.{PartialTransformer, TransformationError, TransformerF}

class ErrorAccTransformersBenchmark extends CommonBenchmarkSettings {
  import fixtures._
  import samples.validation._

  type M[+A] = Either[Vector[TransformationError[String]], A]

  var simple: Simple = samples.simpleSample

  @Benchmark
  def simpleHappyLiftedTransformer: M[SimpleOutput] =
    simpleTransformerLifted.transform(simple)

  @Benchmark
  def simpleHappyPartialTransformer: PartialTransformer.Result[SimpleOutput] =
    simpleTransformerPartial.transform(simple)

  @Benchmark
  def simpleHappyInlineDslLiftedTransformer: M[SimpleOutput] =
    simple
      .intoF[M, SimpleOutput]
      .withFieldComputedF(_.a, s => happy.validateA(s.a).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.b, s => happy.validateB(s.b).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.c, s => happy.validateC(s.c).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.d, s => happy.validateD(s.d).left.map(_.map(TransformationError(_))))
      .transform

  @Benchmark
  def simpleHappyInlineDslPartialTransformer: PartialTransformer.Result[SimpleOutput] =
    simple
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => happy.validateA(s.a).toPartialTransformerResult)
      .withFieldComputedPartial(_.b, s => happy.validateB(s.b).toPartialTransformerResult)
      .withFieldComputedPartial(_.c, s => happy.validateC(s.c).toPartialTransformerResult)
      .withFieldComputedPartial(_.d, s => happy.validateD(s.d).toPartialTransformerResult)
      .transform

  @Benchmark
  def simpleHappyByHandErrorAccEitherSwap: M[SimpleOutput] =
    simpleByHandErrorAccEitherSwap(
      simple,
      happy.validateA(_).left.map(_.map(TransformationError(_))),
      happy.validateB(_).left.map(_.map(TransformationError(_))),
      happy.validateC(_).left.map(_.map(TransformationError(_))),
      happy.validateD(_).left.map(_.map(TransformationError(_)))
    )

  @Benchmark
  def simpleHappyByHandErrorAccCrazyNesting: M[SimpleOutput] =
    simpleByHandErrorAccCrazyNesting(
      simple,
      happy.validateA(_).left.map(_.map(TransformationError(_))),
      happy.validateB(_).left.map(_.map(TransformationError(_))),
      happy.validateC(_).left.map(_.map(TransformationError(_))),
      happy.validateD(_).left.map(_.map(TransformationError(_)))
    )

  @Benchmark
  def simpleUnhappyLiftedTransformer: M[SimpleOutput] =
    simpleTransformerLiftedUnhappy.transform(simple)

  @Benchmark
  def simpleUnhappyPartialTransformer: PartialTransformer.Result[SimpleOutput] =
    simpleTransformerPartialUnhappy.transform(simple)

  @Benchmark
  def simpleUnhappyInlineDslLiftedTransformer: M[SimpleOutput] =
    simple
      .intoF[M, SimpleOutput]
      .withFieldComputedF(_.a, s => unhappy.validateA(s.a).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.b, s => unhappy.validateB(s.b).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.c, s => unhappy.validateC(s.c).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.d, s => unhappy.validateD(s.d).left.map(_.map(TransformationError(_))))
      .transform

  @Benchmark
  def simpleUnhappyInlineDslPartialTransformer: PartialTransformer.Result[SimpleOutput] =
    simple
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => unhappy.validateA(s.a).toPartialTransformerResult)
      .withFieldComputedPartial(_.b, s => unhappy.validateB(s.b).toPartialTransformerResult)
      .withFieldComputedPartial(_.c, s => unhappy.validateC(s.c).toPartialTransformerResult)
      .withFieldComputedPartial(_.d, s => unhappy.validateD(s.d).toPartialTransformerResult)
      .transform

  @Benchmark
  def simpleUnhappyByHandErrorAccEitherSwap: M[SimpleOutput] =
    simpleByHandErrorAccEitherSwap(
      simple,
      unhappy.validateA(_).left.map(_.map(TransformationError(_))),
      unhappy.validateB(_).left.map(_.map(TransformationError(_))),
      unhappy.validateC(_).left.map(_.map(TransformationError(_))),
      unhappy.validateD(_).left.map(_.map(TransformationError(_)))
    )

  @Benchmark
  def simpleUnhappyByHandErrorAccCrazyNesting: M[SimpleOutput] =
    simpleByHandErrorAccCrazyNesting(
      simple,
      unhappy.validateA(_).left.map(_.map(TransformationError(_))),
      unhappy.validateB(_).left.map(_.map(TransformationError(_))),
      unhappy.validateC(_).left.map(_.map(TransformationError(_))),
      unhappy.validateD(_).left.map(_.map(TransformationError(_)))
    )

  private final val simpleTransformerLifted: TransformerF[M, Simple, SimpleOutput] =
    TransformerF
      .define[M, Simple, SimpleOutput]
      .withFieldComputedF(_.a, s => happy.validateA(s.a).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.b, s => happy.validateB(s.b).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.c, s => happy.validateC(s.c).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.d, s => happy.validateD(s.d).left.map(_.map(TransformationError(_))))
      .buildTransformer

  private final val simpleTransformerPartial: PartialTransformer[Simple, SimpleOutput] =
    PartialTransformer
      .define[Simple, SimpleOutput]
      .withFieldComputedPartial(_.a, s => happy.validateA(s.a).toPartialTransformerResult)
      .withFieldComputedPartial(_.b, s => happy.validateB(s.b).toPartialTransformerResult)
      .withFieldComputedPartial(_.c, s => happy.validateC(s.c).toPartialTransformerResult)
      .withFieldComputedPartial(_.d, s => happy.validateD(s.d).toPartialTransformerResult)
      .buildTransformer

  private final val simpleTransformerLiftedUnhappy: TransformerF[M, Simple, SimpleOutput] =
    TransformerF
      .define[M, Simple, SimpleOutput]
      .withFieldComputedF(_.a, s => unhappy.validateA(s.a).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.b, s => unhappy.validateB(s.b).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.c, s => unhappy.validateC(s.c).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.d, s => unhappy.validateD(s.d).left.map(_.map(TransformationError(_))))
      .buildTransformer

  private final val simpleTransformerPartialUnhappy: PartialTransformer[Simple, SimpleOutput] =
    PartialTransformer
      .define[Simple, SimpleOutput]
      .withFieldComputedPartial(_.a, s => unhappy.validateA(s.a).toPartialTransformerResult)
      .withFieldComputedPartial(_.b, s => unhappy.validateB(s.b).toPartialTransformerResult)
      .withFieldComputedPartial(_.c, s => unhappy.validateC(s.c).toPartialTransformerResult)
      .withFieldComputedPartial(_.d, s => unhappy.validateD(s.d).toPartialTransformerResult)
      .buildTransformer

}
