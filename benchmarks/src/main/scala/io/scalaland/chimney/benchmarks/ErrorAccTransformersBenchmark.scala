package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.{ErrorPathNode, PartialTransformer, TransformationError, TransformerF}
import io.scalaland.chimney.partial

class ErrorAccTransformersBenchmark extends CommonBenchmarkSettings {
  import fixtures._
  import samples.validation._

  type M[+A] = Either[Vector[TransformationError[String]], A]

  var simple: Simple = samples.simpleSample

  @Benchmark
  def simpleHappyLiftedTransformer: M[SimpleOutput] =
    simpleTransformerLifted.transform(simple)

  @Benchmark
  def simpleHappyPartialTransformer: partial.Result[SimpleOutput] =
    simpleTransformerPartial.transform(simple)

  @Benchmark
  def simpleHappyInlineDslLiftedTransformer: M[SimpleOutput] =
    simple
      .intoF[M, SimpleOutput]
      .withFieldComputedF(_.a, s => happy.validateA(s.a).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.b, s => happy.validateB(s.b).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.c, s => happy.validateC(s.c).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.d, s => happy.validateD(s.d).left.map(s => Vector(TransformationError(s))))
      .transform

  @Benchmark
  def simpleHappyInlineDslPartialTransformer: partial.Result[SimpleOutput] =
    simple
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => happy.validateA(s.a).toPartialResult)
      .withFieldComputedPartial(_.b, s => happy.validateB(s.b).toPartialResult)
      .withFieldComputedPartial(_.c, s => happy.validateC(s.c).toPartialResult)
      .withFieldComputedPartial(_.d, s => happy.validateD(s.d).toPartialResult)
      .transform

  @Benchmark
  def simpleHappyByHandEitherSwap: M[SimpleOutput] =
    simpleByHandErrorAccEitherSwap(
      simple,
      happy.validateA(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("a")))),
      happy.validateB(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("b")))),
      happy.validateC(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("c")))),
      happy.validateD(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("d"))))
    )

  @Benchmark
  def simpleHappyByHandCrazyNesting: M[SimpleOutput] =
    simpleByHandErrorAccCrazyNesting(
      simple,
      happy.validateA(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("a")))),
      happy.validateB(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("b")))),
      happy.validateC(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("c")))),
      happy.validateD(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("d"))))
    )

  @Benchmark
  def simpleUnhappyLiftedTransformer: M[SimpleOutput] =
    simpleTransformerLiftedUnhappy.transform(simple)

  @Benchmark
  def simpleUnhappyPartialTransformer: partial.Result[SimpleOutput] =
    simpleTransformerPartialUnhappy.transform(simple)

  @Benchmark
  def simpleUnhappyInlineDslLiftedTransformer: M[SimpleOutput] =
    simple
      .intoF[M, SimpleOutput]
      .withFieldComputedF(_.a, s => unhappy.validateA(s.a).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.b, s => unhappy.validateB(s.b).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.c, s => unhappy.validateC(s.c).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.d, s => unhappy.validateD(s.d).left.map(s => Vector(TransformationError(s))))
      .transform

  @Benchmark
  def simpleUnhappyInlineDslPartialTransformer: partial.Result[SimpleOutput] =
    simple
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => unhappy.validateA(s.a).toPartialResult)
      .withFieldComputedPartial(_.b, s => unhappy.validateB(s.b).toPartialResult)
      .withFieldComputedPartial(_.c, s => unhappy.validateC(s.c).toPartialResult)
      .withFieldComputedPartial(_.d, s => unhappy.validateD(s.d).toPartialResult)
      .transform

  @Benchmark
  def simpleUnhappyByHandEitherSwap: M[SimpleOutput] =
    simpleByHandErrorAccEitherSwap(
      simple,
      unhappy.validateA(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("a")))),
      unhappy.validateB(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("b")))),
      unhappy.validateC(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("c")))),
      unhappy.validateD(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("d"))))
    )

  @Benchmark
  def simpleUnhappyByHandCrazyNesting: M[SimpleOutput] =
    simpleByHandErrorAccCrazyNesting(
      simple,
      unhappy.validateA(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("a")))),
      unhappy.validateB(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("b")))),
      unhappy.validateC(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("c")))),
      unhappy.validateD(_).left.map(s => Vector(TransformationError(s).prepend(ErrorPathNode.Accessor("d"))))
    )

  @Benchmark
  def longHappyLiftedTransformer: M[LargeOutput] =
    longTransformerLifted.transform(samples.largeSample)

  @Benchmark
  def longHappyPartialTransformer: partial.Result[LargeOutput] =
    longTransformerPartial.transform(samples.largeSample)

  @Benchmark
  def longUnhappyLiftedTransformer: M[LargeOutput] =
    longTransformerLiftedUnhappy.transform(samples.largeSample)

  @Benchmark
  def longUnhappyPartialTransformer: partial.Result[LargeOutput] =
    longTransformerPartialUnhappy.transform(samples.largeSample)

  @Benchmark
  def nestedLongHappyLiftedTransformer: M[Vector[LargeOutput]] = {
    implicit val ltl: TransformerF[M, Large, LargeOutput] = longTransformerLifted
    samples.largeNestedSample.transformIntoF[M, Vector[LargeOutput]]
  }

  @Benchmark
  def nestedLongHappyPartialTransformer: partial.Result[Vector[LargeOutput]] = {
    implicit val ltp: PartialTransformer[Large, LargeOutput] = longTransformerPartial
    samples.largeNestedSample.transformIntoPartial[Vector[LargeOutput]]
  }

  @Benchmark
  def nestedLongUnhappyLiftedTransformer: M[Vector[LargeOutput]] = {
    implicit val ltlu: TransformerF[M, Large, LargeOutput] = longTransformerLiftedUnhappy
    samples.largeNestedSample.transformIntoF[M, Vector[LargeOutput]]
  }

  @Benchmark
  def nestedLongUnhappyPartialTransformer: partial.Result[Vector[LargeOutput]] = {
    implicit val ltpu: PartialTransformer[Large, LargeOutput] = longTransformerPartialUnhappy
    samples.largeNestedSample.transformIntoPartial[Vector[LargeOutput]]
  }

  private final val simpleTransformerLifted: TransformerF[M, Simple, SimpleOutput] =
    TransformerF
      .define[M, Simple, SimpleOutput]
      .withFieldComputedF(_.a, s => happy.validateA(s.a).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.b, s => happy.validateB(s.b).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.c, s => happy.validateC(s.c).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.d, s => happy.validateD(s.d).left.map(s => Vector(TransformationError(s))))
      .buildTransformer

  private final val simpleTransformerPartial: PartialTransformer[Simple, SimpleOutput] =
    PartialTransformer
      .define[Simple, SimpleOutput]
      .withFieldComputedPartial(_.a, s => happy.validateA(s.a).toPartialResult)
      .withFieldComputedPartial(_.b, s => happy.validateB(s.b).toPartialResult)
      .withFieldComputedPartial(_.c, s => happy.validateC(s.c).toPartialResult)
      .withFieldComputedPartial(_.d, s => happy.validateD(s.d).toPartialResult)
      .buildTransformer

  private final val simpleTransformerLiftedUnhappy: TransformerF[M, Simple, SimpleOutput] =
    TransformerF
      .define[M, Simple, SimpleOutput]
      .withFieldComputedF(_.a, s => unhappy.validateA(s.a).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.b, s => unhappy.validateB(s.b).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.c, s => unhappy.validateC(s.c).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.d, s => unhappy.validateD(s.d).left.map(s => Vector(TransformationError(s))))
      .buildTransformer

  private final val simpleTransformerPartialUnhappy: PartialTransformer[Simple, SimpleOutput] =
    PartialTransformer
      .define[Simple, SimpleOutput]
      .withFieldComputedPartial(_.a, s => unhappy.validateA(s.a).toPartialResult)
      .withFieldComputedPartial(_.b, s => unhappy.validateB(s.b).toPartialResult)
      .withFieldComputedPartial(_.c, s => unhappy.validateC(s.c).toPartialResult)
      .withFieldComputedPartial(_.d, s => unhappy.validateD(s.d).toPartialResult)
      .buildTransformer

  private final val longTransformerLifted: TransformerF[M, Large, LargeOutput] =
    TransformerF
      .define[M, Large, LargeOutput]
      .withFieldComputedF(_.a, s => happy.squareInt(s.a).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.b, s => happy.squareInt(s.b).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.c, s => happy.squareInt(s.c).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.d, s => happy.squareInt(s.d).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.e, s => happy.squareInt(s.e).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.f, s => happy.squareInt(s.f).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.g, s => happy.squareInt(s.g).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.h, s => happy.squareInt(s.h).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.i, s => happy.squareInt(s.i).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.j, s => happy.squareInt(s.j).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.k, s => happy.squareInt(s.k).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.l, s => happy.squareInt(s.l).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.m, s => happy.squareInt(s.m).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.n, s => happy.squareInt(s.n).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.o, s => happy.squareInt(s.o).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.p, s => happy.squareInt(s.p).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.q, s => happy.squareInt(s.q).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.r, s => happy.squareInt(s.r).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.s, s => happy.squareInt(s.s).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.t, s => happy.squareInt(s.t).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.u, s => happy.squareInt(s.u).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.v, s => happy.squareInt(s.v).left.map(s => Vector(TransformationError(s))))
      .buildTransformer

  private final val longTransformerPartial: PartialTransformer[Large, LargeOutput] =
    PartialTransformer
      .define[Large, LargeOutput]
      .withFieldComputedPartial(_.a, s => happy.squareInt(s.a).toPartialResult)
      .withFieldComputedPartial(_.b, s => happy.squareInt(s.b).toPartialResult)
      .withFieldComputedPartial(_.c, s => happy.squareInt(s.c).toPartialResult)
      .withFieldComputedPartial(_.d, s => happy.squareInt(s.d).toPartialResult)
      .withFieldComputedPartial(_.e, s => happy.squareInt(s.e).toPartialResult)
      .withFieldComputedPartial(_.f, s => happy.squareInt(s.f).toPartialResult)
      .withFieldComputedPartial(_.g, s => happy.squareInt(s.g).toPartialResult)
      .withFieldComputedPartial(_.h, s => happy.squareInt(s.h).toPartialResult)
      .withFieldComputedPartial(_.i, s => happy.squareInt(s.i).toPartialResult)
      .withFieldComputedPartial(_.j, s => happy.squareInt(s.j).toPartialResult)
      .withFieldComputedPartial(_.k, s => happy.squareInt(s.k).toPartialResult)
      .withFieldComputedPartial(_.l, s => happy.squareInt(s.l).toPartialResult)
      .withFieldComputedPartial(_.m, s => happy.squareInt(s.m).toPartialResult)
      .withFieldComputedPartial(_.n, s => happy.squareInt(s.n).toPartialResult)
      .withFieldComputedPartial(_.o, s => happy.squareInt(s.o).toPartialResult)
      .withFieldComputedPartial(_.p, s => happy.squareInt(s.p).toPartialResult)
      .withFieldComputedPartial(_.q, s => happy.squareInt(s.q).toPartialResult)
      .withFieldComputedPartial(_.r, s => happy.squareInt(s.r).toPartialResult)
      .withFieldComputedPartial(_.s, s => happy.squareInt(s.s).toPartialResult)
      .withFieldComputedPartial(_.t, s => happy.squareInt(s.t).toPartialResult)
      .withFieldComputedPartial(_.u, s => happy.squareInt(s.u).toPartialResult)
      .withFieldComputedPartial(_.v, s => happy.squareInt(s.v).toPartialResult)
      .buildTransformer

  private final val longTransformerLiftedUnhappy: TransformerF[M, Large, LargeOutput] =
    TransformerF
      .define[M, Large, LargeOutput]
      .withFieldComputedF(_.a, s => unhappy.squareIntWhenOdd(s.a).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.b, s => unhappy.squareIntWhenOdd(s.b).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.c, s => unhappy.squareIntWhenOdd(s.c).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.d, s => unhappy.squareIntWhenOdd(s.d).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.e, s => unhappy.squareIntWhenOdd(s.e).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.f, s => unhappy.squareIntWhenOdd(s.f).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.g, s => unhappy.squareIntWhenOdd(s.g).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.h, s => unhappy.squareIntWhenOdd(s.h).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.i, s => unhappy.squareIntWhenOdd(s.i).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.j, s => unhappy.squareIntWhenOdd(s.j).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.k, s => unhappy.squareIntWhenOdd(s.k).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.l, s => unhappy.squareIntWhenOdd(s.l).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.m, s => unhappy.squareIntWhenOdd(s.m).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.n, s => unhappy.squareIntWhenOdd(s.n).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.o, s => unhappy.squareIntWhenOdd(s.o).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.p, s => unhappy.squareIntWhenOdd(s.p).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.q, s => unhappy.squareIntWhenOdd(s.q).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.r, s => unhappy.squareIntWhenOdd(s.r).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.s, s => unhappy.squareIntWhenOdd(s.s).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.t, s => unhappy.squareIntWhenOdd(s.t).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.u, s => unhappy.squareIntWhenOdd(s.u).left.map(s => Vector(TransformationError(s))))
      .withFieldComputedF(_.v, s => unhappy.squareIntWhenOdd(s.v).left.map(s => Vector(TransformationError(s))))
      .buildTransformer

  private final val longTransformerPartialUnhappy: PartialTransformer[Large, LargeOutput] =
    PartialTransformer
      .define[Large, LargeOutput]
      .withFieldComputedPartial(_.a, s => unhappy.squareIntWhenOdd(s.a).toPartialResult)
      .withFieldComputedPartial(_.b, s => unhappy.squareIntWhenOdd(s.b).toPartialResult)
      .withFieldComputedPartial(_.c, s => unhappy.squareIntWhenOdd(s.c).toPartialResult)
      .withFieldComputedPartial(_.d, s => unhappy.squareIntWhenOdd(s.d).toPartialResult)
      .withFieldComputedPartial(_.e, s => unhappy.squareIntWhenOdd(s.e).toPartialResult)
      .withFieldComputedPartial(_.f, s => unhappy.squareIntWhenOdd(s.f).toPartialResult)
      .withFieldComputedPartial(_.g, s => unhappy.squareIntWhenOdd(s.g).toPartialResult)
      .withFieldComputedPartial(_.h, s => unhappy.squareIntWhenOdd(s.h).toPartialResult)
      .withFieldComputedPartial(_.i, s => unhappy.squareIntWhenOdd(s.i).toPartialResult)
      .withFieldComputedPartial(_.j, s => unhappy.squareIntWhenOdd(s.j).toPartialResult)
      .withFieldComputedPartial(_.k, s => unhappy.squareIntWhenOdd(s.k).toPartialResult)
      .withFieldComputedPartial(_.l, s => unhappy.squareIntWhenOdd(s.l).toPartialResult)
      .withFieldComputedPartial(_.m, s => unhappy.squareIntWhenOdd(s.m).toPartialResult)
      .withFieldComputedPartial(_.n, s => unhappy.squareIntWhenOdd(s.n).toPartialResult)
      .withFieldComputedPartial(_.o, s => unhappy.squareIntWhenOdd(s.o).toPartialResult)
      .withFieldComputedPartial(_.p, s => unhappy.squareIntWhenOdd(s.p).toPartialResult)
      .withFieldComputedPartial(_.q, s => unhappy.squareIntWhenOdd(s.q).toPartialResult)
      .withFieldComputedPartial(_.r, s => unhappy.squareIntWhenOdd(s.r).toPartialResult)
      .withFieldComputedPartial(_.s, s => unhappy.squareIntWhenOdd(s.s).toPartialResult)
      .withFieldComputedPartial(_.t, s => unhappy.squareIntWhenOdd(s.t).toPartialResult)
      .withFieldComputedPartial(_.u, s => unhappy.squareIntWhenOdd(s.u).toPartialResult)
      .withFieldComputedPartial(_.v, s => unhappy.squareIntWhenOdd(s.v).toPartialResult)
      .buildTransformer

}
