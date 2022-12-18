package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.{PartialTransformer, TransformationError, TransformerF}

class ErrorAccTransformersBenchmark extends CommonBenchmarkSettings {
  import fixtures._
  import samples.validation._

  type M[+A] = Either[Vector[TransformationError[String]], A]

  var simple: Simple = samples.simpleSample

  private final val transformerF: TransformerF[M, Simple, SimpleOutput] =
    TransformerF
      .define[M, Simple, SimpleOutput]
      .withFieldComputedF(_.a, s => happy.validateA(s.a).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.b, s => happy.validateB(s.b).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.c, s => happy.validateC(s.c).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.d, s => happy.validateD(s.d).left.map(_.map(TransformationError(_))))
      .buildTransformer

  private final val transformerPartial: PartialTransformer[Simple, SimpleOutput] =
    PartialTransformer
      .define[Simple, SimpleOutput]
      .withFieldComputedPartial(_.a, s => happy.validateA(s.a).toPartialTransformerResult)
      .withFieldComputedPartial(_.b, s => happy.validateB(s.b).toPartialTransformerResult)
      .withFieldComputedPartial(_.c, s => happy.validateC(s.c).toPartialTransformerResult)
      .withFieldComputedPartial(_.d, s => happy.validateD(s.d).toPartialTransformerResult)
      .buildTransformer

  private final val transformerFUnhappy: TransformerF[M, Simple, SimpleOutput] =
    TransformerF
      .define[M, Simple, SimpleOutput]
      .withFieldComputedF(_.a, s => unhappy.validateA(s.a).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.b, s => unhappy.validateB(s.b).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.c, s => unhappy.validateC(s.c).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.d, s => unhappy.validateD(s.d).left.map(_.map(TransformationError(_))))
      .buildTransformer

  private final val transformerPartialUnhappy: PartialTransformer[Simple, SimpleOutput] =
    PartialTransformer
      .define[Simple, SimpleOutput]
      .withFieldComputedPartial(_.a, s => unhappy.validateA(s.a).toPartialTransformerResult)
      .withFieldComputedPartial(_.b, s => unhappy.validateB(s.b).toPartialTransformerResult)
      .withFieldComputedPartial(_.c, s => unhappy.validateC(s.c).toPartialTransformerResult)
      .withFieldComputedPartial(_.d, s => unhappy.validateD(s.d).toPartialTransformerResult)
      .buildTransformer

  @Benchmark
  def happyLiftedTransformer: M[SimpleOutput] =
    transformerF.transform(simple)

  @Benchmark
  def happyPartialTransformer: PartialTransformer.Result[SimpleOutput] =
    transformerPartial.transform(simple)

  @Benchmark
  def happyLiftedTransformerInlineDsl: M[SimpleOutput] =
    simple
      .intoF[M, SimpleOutput]
      .withFieldComputedF(_.a, s => happy.validateA(s.a).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.b, s => happy.validateB(s.b).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.c, s => happy.validateC(s.c).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.d, s => happy.validateD(s.d).left.map(_.map(TransformationError(_))))
      .transform

  @Benchmark
  def happyPartialTransformerInlineDsl: PartialTransformer.Result[SimpleOutput] =
    simple
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => happy.validateA(s.a).toPartialTransformerResult)
      .withFieldComputedPartial(_.b, s => happy.validateB(s.b).toPartialTransformerResult)
      .withFieldComputedPartial(_.c, s => happy.validateC(s.c).toPartialTransformerResult)
      .withFieldComputedPartial(_.d, s => happy.validateD(s.d).toPartialTransformerResult)
      .transform

  @Benchmark
  def happyByHandErrorAccEitherSwap: M[SimpleOutput] =
    byHandErrorAccEitherSwap(
      simple,
      happy.validateA(_).left.map(_.map(TransformationError(_))),
      happy.validateB(_).left.map(_.map(TransformationError(_))),
      happy.validateC(_).left.map(_.map(TransformationError(_))),
      happy.validateD(_).left.map(_.map(TransformationError(_)))
    )

  @Benchmark
  def happyByHandErrorAccCrazyNesting: M[SimpleOutput] =
    byHandErrorAccCrazyNesting(
      simple,
      happy.validateA(_).left.map(_.map(TransformationError(_))),
      happy.validateB(_).left.map(_.map(TransformationError(_))),
      happy.validateC(_).left.map(_.map(TransformationError(_))),
      happy.validateD(_).left.map(_.map(TransformationError(_)))
    )

  @Benchmark
  def unhappyLiftedTransformer: M[SimpleOutput] =
    transformerFUnhappy.transform(simple)

  @Benchmark
  def unhappyPartialTransformer: PartialTransformer.Result[SimpleOutput] =
    transformerPartialUnhappy.transform(simple)

  @Benchmark
  def unhappyLiftedTransformerInlineDsl: M[SimpleOutput] =
    simple
      .intoF[M, SimpleOutput]
      .withFieldComputedF(_.a, s => unhappy.validateA(s.a).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.b, s => unhappy.validateB(s.b).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.c, s => unhappy.validateC(s.c).left.map(_.map(TransformationError(_))))
      .withFieldComputedF(_.d, s => unhappy.validateD(s.d).left.map(_.map(TransformationError(_))))
      .transform

  @Benchmark
  def unhappyPartialTransformerInlineDsl: PartialTransformer.Result[SimpleOutput] =
    simple
      .intoPartial[SimpleOutput]
      .withFieldComputedPartial(_.a, s => unhappy.validateA(s.a).toPartialTransformerResult)
      .withFieldComputedPartial(_.b, s => unhappy.validateB(s.b).toPartialTransformerResult)
      .withFieldComputedPartial(_.c, s => unhappy.validateC(s.c).toPartialTransformerResult)
      .withFieldComputedPartial(_.d, s => unhappy.validateD(s.d).toPartialTransformerResult)
      .transform

  @Benchmark
  def unhappyByHandErrorAccEitherSwap: M[SimpleOutput] =
    byHandErrorAccEitherSwap(
      simple,
      unhappy.validateA(_).left.map(_.map(TransformationError(_))),
      unhappy.validateB(_).left.map(_.map(TransformationError(_))),
      unhappy.validateC(_).left.map(_.map(TransformationError(_))),
      unhappy.validateD(_).left.map(_.map(TransformationError(_)))
    )

  @Benchmark
  def unhappyByHandErrorAccCrazyNesting: M[SimpleOutput] =
    byHandErrorAccCrazyNesting(
      simple,
      unhappy.validateA(_).left.map(_.map(TransformationError(_))),
      unhappy.validateB(_).left.map(_.map(TransformationError(_))),
      unhappy.validateC(_).left.map(_.map(TransformationError(_))),
      unhappy.validateD(_).left.map(_.map(TransformationError(_)))
    )

  private def byHandErrorAccEitherSwap(
      simple: Simple,
      fa: Int => M[Int],
      fb: Double => M[Double],
      fc: String => M[String],
      fd: Option[String] => M[Option[String]]
  ): M[SimpleOutput] = {
    val valA = fa(simple.a)
    val valB = fb(simple.b)
    val valC = fc(simple.c)
    val valD = fd(simple.d)

    if(valA.isRight && valB.isRight && valC.isRight && valD.isRight) {
      Right(SimpleOutput(valA.toOption.get, valB.toOption.get, valC.toOption.get, valD.toOption.get))
    } else {
      val errsB = Vector.newBuilder[TransformationError[String]]
      errsB ++= valA.swap.getOrElse(Vector.empty)
      errsB ++= valB.swap.getOrElse(Vector.empty)
      errsB ++= valC.swap.getOrElse(Vector.empty)
      errsB ++= valD.swap.getOrElse(Vector.empty)
      Left(errsB.result())
    }
  }

  private def byHandErrorAccCrazyNesting(
      simple: Simple,
      fa: Int => M[Int],
      fb: Double => M[Double],
      fc: String => M[String],
      fd: Option[String] => M[Option[String]]
  ): M[SimpleOutput] = {
    fa(simple.a) match {
      case Right(a) =>
        fb(simple.b) match {
          case Right(b) =>
            fc(simple.c) match {
              case Right(c) =>
                fd(simple.d) match {
                  case Right(d)         => Right(SimpleOutput(a, b, c, d))
                  case retVal @ Left(_) => retVal.asInstanceOf[M[SimpleOutput]]
                }
              case Left(errs3) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs3)
                  case Left(errs4) => Left(errs3 ++ errs4)
                }
            }
          case Left(errs2) =>
            fc(simple.c) match {
              case Right(_) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs2)
                  case Left(errs4) => Left(errs2 ++ errs4)
                }
              case Left(errs3) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs2 ++ errs3)
                  case Left(errs4) => Left(errs2 ++ errs3 ++ errs4)
                }
            }
        }
      case Left(errs1) =>
        fb(simple.b) match {
          case Right(_) =>
            fc(simple.c) match {
              case Right(_) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs1)
                  case Left(errs4) => Left(errs1 ++ errs4)
                }
              case Left(errs3) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs3)
                  case Left(errs4) => Left(errs1 ++ errs3 ++ errs4)
                }
            }
          case Left(errs2) =>
            fc(simple.c) match {
              case Right(_) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs1 ++ errs2)
                  case Left(errs4) => Left(errs1 ++ errs2 ++ errs4)
                }
              case Left(errs3) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs1 ++ errs2 ++ errs3)
                  case Left(errs4) => Left(errs1 ++ errs2 ++ errs3 ++ errs4)
                }
            }
        }
    }
  }

}
