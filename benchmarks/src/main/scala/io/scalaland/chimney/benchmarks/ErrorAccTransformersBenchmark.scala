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

  @Benchmark
  def longHappyLiftedTransformer: M[LongOutput] =
    longTransformerLifted.transform(samples.longSample)

  @Benchmark
  def longHappyPartialTransformer: PartialTransformer.Result[LongOutput] =
    longTransformerPartial.transform(samples.longSample)

  @Benchmark
  def longUnhappyLiftedTransformer: M[LongOutput] =
    longTransformerLiftedUnhappy.transform(samples.longSample)

  @Benchmark
  def longUnhappyPartialTransformer: PartialTransformer.Result[LongOutput] =
    longTransformerPartialUnhappy.transform(samples.longSample)

  @Benchmark
  def nestedLongHappyLiftedTransformer: M[Vector[LongOutput]] = {
    implicit val ltl: TransformerF[M, Long, LongOutput] = longTransformerLifted
    samples.longNestedSample.transformIntoF[M, Vector[LongOutput]]
  }

  @Benchmark
  def nestedLongHappyPartialTransformer: PartialTransformer.Result[Vector[LongOutput]] = {
    implicit val ltp: PartialTransformer[Long, LongOutput] = longTransformerPartial
    samples.longNestedSample.transformIntoPartial[Vector[LongOutput]]
  }

  @Benchmark
  def nestedLongUnhappyLiftedTransformer: M[Vector[LongOutput]] = {
    implicit val ltlu: TransformerF[M, Long, LongOutput] = longTransformerLiftedUnhappy
    samples.longNestedSample.transformIntoF[M, Vector[LongOutput]]
  }

  @Benchmark
  def nestedLongUnhappyPartialTransformer: PartialTransformer.Result[Vector[LongOutput]] = {
    implicit val ltpu: PartialTransformer[Long, LongOutput] = longTransformerPartialUnhappy
    samples.longNestedSample.transformIntoPartial[Vector[LongOutput]]
  }

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

  private final val longTransformerLifted: TransformerF[M, Long, LongOutput] =
    TransformerF
      .define[M, Long, LongOutput]
      .withFieldComputedF(_.a, s => happy.squareInt(s.a).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.b, s => happy.squareInt(s.b).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.c, s => happy.squareInt(s.c).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.d, s => happy.squareInt(s.d).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.e, s => happy.squareInt(s.e).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.f, s => happy.squareInt(s.f).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.g, s => happy.squareInt(s.g).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.h, s => happy.squareInt(s.h).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.i, s => happy.squareInt(s.i).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.j, s => happy.squareInt(s.j).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.k, s => happy.squareInt(s.k).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.l, s => happy.squareInt(s.l).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.m, s => happy.squareInt(s.m).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.n, s => happy.squareInt(s.n).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.o, s => happy.squareInt(s.o).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.p, s => happy.squareInt(s.p).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.q, s => happy.squareInt(s.q).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.r, s => happy.squareInt(s.r).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.s, s => happy.squareInt(s.s).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.t, s => happy.squareInt(s.t).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.u, s => happy.squareInt(s.u).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.v, s => happy.squareInt(s.v).left.map(_.map(TransformationError(_))))
      .buildTransformer

  private final val longTransformerPartial: PartialTransformer[Long, LongOutput] =
    PartialTransformer
      .define[Long, LongOutput]
      .withFieldComputedPartial(_.a, s => happy.squareInt(s.a).toPartialTransformerResult)
      .withFieldComputedPartial(_.b, s => happy.squareInt(s.b).toPartialTransformerResult)
      .withFieldComputedPartial(_.c, s => happy.squareInt(s.c).toPartialTransformerResult)
      .withFieldComputedPartial(_.d, s => happy.squareInt(s.d).toPartialTransformerResult)
      .withFieldComputedPartial(_.e, s => happy.squareInt(s.e).toPartialTransformerResult)
      .withFieldComputedPartial(_.f, s => happy.squareInt(s.f).toPartialTransformerResult)
      .withFieldComputedPartial(_.g, s => happy.squareInt(s.g).toPartialTransformerResult)
      .withFieldComputedPartial(_.h, s => happy.squareInt(s.h).toPartialTransformerResult)
      .withFieldComputedPartial(_.i, s => happy.squareInt(s.i).toPartialTransformerResult)
      .withFieldComputedPartial(_.j, s => happy.squareInt(s.j).toPartialTransformerResult)
      .withFieldComputedPartial(_.k, s => happy.squareInt(s.k).toPartialTransformerResult)
      .withFieldComputedPartial(_.l, s => happy.squareInt(s.l).toPartialTransformerResult)
      .withFieldComputedPartial(_.m, s => happy.squareInt(s.m).toPartialTransformerResult)
      .withFieldComputedPartial(_.n, s => happy.squareInt(s.n).toPartialTransformerResult)
      .withFieldComputedPartial(_.o, s => happy.squareInt(s.o).toPartialTransformerResult)
      .withFieldComputedPartial(_.p, s => happy.squareInt(s.p).toPartialTransformerResult)
      .withFieldComputedPartial(_.q, s => happy.squareInt(s.q).toPartialTransformerResult)
      .withFieldComputedPartial(_.r, s => happy.squareInt(s.r).toPartialTransformerResult)
      .withFieldComputedPartial(_.s, s => happy.squareInt(s.s).toPartialTransformerResult)
      .withFieldComputedPartial(_.t, s => happy.squareInt(s.t).toPartialTransformerResult)
      .withFieldComputedPartial(_.u, s => happy.squareInt(s.u).toPartialTransformerResult)
      .withFieldComputedPartial(_.v, s => happy.squareInt(s.v).toPartialTransformerResult)
      .buildTransformer

  private final val longTransformerLiftedUnhappy: TransformerF[M, Long, LongOutput] =
    TransformerF
      .define[M, Long, LongOutput]
      .withFieldComputedF(_.a, s => unhappy.squareIntWhenOdd(s.a).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.b, s => unhappy.squareIntWhenOdd(s.b).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.c, s => unhappy.squareIntWhenOdd(s.c).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.d, s => unhappy.squareIntWhenOdd(s.d).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.e, s => unhappy.squareIntWhenOdd(s.e).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.f, s => unhappy.squareIntWhenOdd(s.f).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.g, s => unhappy.squareIntWhenOdd(s.g).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.h, s => unhappy.squareIntWhenOdd(s.h).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.i, s => unhappy.squareIntWhenOdd(s.i).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.j, s => unhappy.squareIntWhenOdd(s.j).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.k, s => unhappy.squareIntWhenOdd(s.k).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.l, s => unhappy.squareIntWhenOdd(s.l).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.m, s => unhappy.squareIntWhenOdd(s.m).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.n, s => unhappy.squareIntWhenOdd(s.n).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.o, s => unhappy.squareIntWhenOdd(s.o).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.p, s => unhappy.squareIntWhenOdd(s.p).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.q, s => unhappy.squareIntWhenOdd(s.q).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.r, s => unhappy.squareIntWhenOdd(s.r).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.s, s => unhappy.squareIntWhenOdd(s.s).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.t, s => unhappy.squareIntWhenOdd(s.t).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.u, s => unhappy.squareIntWhenOdd(s.u).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.v, s => unhappy.squareIntWhenOdd(s.v).left.map(_.map(TransformationError(_))))
      .buildTransformer

  private final val longTransformerPartialUnhappy: PartialTransformer[Long, LongOutput] =
    PartialTransformer
      .define[Long, LongOutput]
      .withFieldComputedPartial(_.a, s => unhappy.squareIntWhenOdd(s.a).toPartialTransformerResult)
      .withFieldComputedPartial(_.b, s => unhappy.squareIntWhenOdd(s.b).toPartialTransformerResult)
      .withFieldComputedPartial(_.c, s => unhappy.squareIntWhenOdd(s.c).toPartialTransformerResult)
      .withFieldComputedPartial(_.d, s => unhappy.squareIntWhenOdd(s.d).toPartialTransformerResult)
      .withFieldComputedPartial(_.e, s => unhappy.squareIntWhenOdd(s.e).toPartialTransformerResult)
      .withFieldComputedPartial(_.f, s => unhappy.squareIntWhenOdd(s.f).toPartialTransformerResult)
      .withFieldComputedPartial(_.g, s => unhappy.squareIntWhenOdd(s.g).toPartialTransformerResult)
      .withFieldComputedPartial(_.h, s => unhappy.squareIntWhenOdd(s.h).toPartialTransformerResult)
      .withFieldComputedPartial(_.i, s => unhappy.squareIntWhenOdd(s.i).toPartialTransformerResult)
      .withFieldComputedPartial(_.j, s => unhappy.squareIntWhenOdd(s.j).toPartialTransformerResult)
      .withFieldComputedPartial(_.k, s => unhappy.squareIntWhenOdd(s.k).toPartialTransformerResult)
      .withFieldComputedPartial(_.l, s => unhappy.squareIntWhenOdd(s.l).toPartialTransformerResult)
      .withFieldComputedPartial(_.m, s => unhappy.squareIntWhenOdd(s.m).toPartialTransformerResult)
      .withFieldComputedPartial(_.n, s => unhappy.squareIntWhenOdd(s.n).toPartialTransformerResult)
      .withFieldComputedPartial(_.o, s => unhappy.squareIntWhenOdd(s.o).toPartialTransformerResult)
      .withFieldComputedPartial(_.p, s => unhappy.squareIntWhenOdd(s.p).toPartialTransformerResult)
      .withFieldComputedPartial(_.q, s => unhappy.squareIntWhenOdd(s.q).toPartialTransformerResult)
      .withFieldComputedPartial(_.r, s => unhappy.squareIntWhenOdd(s.r).toPartialTransformerResult)
      .withFieldComputedPartial(_.s, s => unhappy.squareIntWhenOdd(s.s).toPartialTransformerResult)
      .withFieldComputedPartial(_.t, s => unhappy.squareIntWhenOdd(s.t).toPartialTransformerResult)
      .withFieldComputedPartial(_.u, s => unhappy.squareIntWhenOdd(s.u).toPartialTransformerResult)
      .withFieldComputedPartial(_.v, s => unhappy.squareIntWhenOdd(s.v).toPartialTransformerResult)
      .buildTransformer

}
