package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.{PartialTransformer, TransformationError, TransformerF}

import scala.collection.mutable.ListBuffer

class TransformerBenchmark extends CommonBenchmarkSettings {
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
  def validateTransformFHappyChimney: M[SimpleOutput] =
    transformerF.transform(simple)

  @Benchmark
  def validateTransformPartialHappyChimney: PartialTransformer.Result[SimpleOutput] =
    transformerPartial.transform(simple)

//  @Benchmark
//  def validateTransformFHappyChimneyWithDsl: M[SimpleOutput] =
//    simple
//      .intoF[M, SimpleOutput]
//      .withFieldComputedF(_.a, s => happy.validateA(s.a).left.map(_.map(TransformationError(_))))
//      .withFieldComputedF(_.b, s => happy.validateB(s.b).left.map(_.map(TransformationError(_))))
//      .withFieldComputedF(_.c, s => happy.validateC(s.c).left.map(_.map(TransformationError(_))))
//      .withFieldComputedF(_.d, s => happy.validateD(s.d).left.map(_.map(TransformationError(_))))
//      .transform

//  @Benchmark
//  def validateTransformPartialHappyChimneyWithDsl: PartialTransformer.Result[SimpleOutput] =
//    simple
//      .intoPartial[SimpleOutput]
//      .withFieldComputedPartial(_.a, s => happy.validateA(s.a).toPartialTransformerResult)
//      .withFieldComputedPartial(_.b, s => happy.validateB(s.b).toPartialTransformerResult)
//      .withFieldComputedPartial(_.c, s => happy.validateC(s.c).toPartialTransformerResult)
//      .withFieldComputedPartial(_.d, s => happy.validateD(s.d).toPartialTransformerResult)
//      .transform
//
//  @Benchmark
//  def validateTransformFHappyByHandFlatMap: M[SimpleOutput] =
//    validateByHandHappyFlatMap(
//      simple,
//      happy.validateA(_).left.map(_.map(TransformationError(_))),
//      happy.validateB(_).left.map(_.map(TransformationError(_))),
//      happy.validateC(_).left.map(_.map(TransformationError(_))),
//      happy.validateD(_).left.map(_.map(TransformationError(_)))
//    )
//
//  @Benchmark
//  def validateTransformFHappyByHandErrorsSwap: M[SimpleOutput] =
//    validateByHandHandleErrors(
//      simple,
//      happy.validateA(_).left.map(_.map(TransformationError(_))),
//      happy.validateB(_).left.map(_.map(TransformationError(_))),
//      happy.validateC(_).left.map(_.map(TransformationError(_))),
//      happy.validateD(_).left.map(_.map(TransformationError(_)))
//    )

//  @Benchmark
//  def validateTransformFHappyByHandErrorsPatMat: M[SimpleOutput] =
//    validateDontDoThisAtHomeKids(
//      simple,
//      happy.validateA(_).left.map(_.map(TransformationError(_))),
//      happy.validateB(_).left.map(_.map(TransformationError(_))),
//      happy.validateC(_).left.map(_.map(TransformationError(_))),
//      happy.validateD(_).left.map(_.map(TransformationError(_)))
//    )

//  @Benchmark
//  def validateTransformFUnhappyChimney: M[SimpleOutput] =
//    transformerFUnhappy.transform(simple)

//  @Benchmark
//  def validateTransformPartialUnhappyChimney: PartialTransformer.Result[SimpleOutput] =
//    transformerPartialUnhappy.transform(simple)
//
//  @Benchmark
//  def validateTransformFUnhappyChimneyWithDsl: M[SimpleOutput] =
//    simple
//      .intoF[M, SimpleOutput]
//      .withFieldComputedF(_.a, s => unhappy.validateA(s.a).left.map(_.map(TransformationError(_))))
//      .withFieldComputedF(_.b, s => unhappy.validateB(s.b).left.map(_.map(TransformationError(_))))
//      .withFieldComputedF(_.c, s => unhappy.validateC(s.c).left.map(_.map(TransformationError(_))))
//      .withFieldComputedF(_.d, s => unhappy.validateD(s.d).left.map(_.map(TransformationError(_))))
//      .transform
//
//  @Benchmark
//  def validateTransformPartialUnhappyChimneyWithDsl: PartialTransformer.Result[SimpleOutput] =
//    simple
//      .intoPartial[SimpleOutput]
//      .withFieldComputedPartial(_.a, s => unhappy.validateA(s.a).toPartialTransformerResult)
//      .withFieldComputedPartial(_.b, s => unhappy.validateB(s.b).toPartialTransformerResult)
//      .withFieldComputedPartial(_.c, s => unhappy.validateC(s.c).toPartialTransformerResult)
//      .withFieldComputedPartial(_.d, s => unhappy.validateD(s.d).toPartialTransformerResult)
//      .transform

//  @Benchmark
//  def validateTransformFUnhappyByHandFlatMap: M[SimpleOutput] =
//    validateByHandHappyFlatMap(
//      simple,
//      unhappy.validateA(_).left.map(_.map(TransformationError(_))),
//      unhappy.validateB(_).left.map(_.map(TransformationError(_))),
//      unhappy.validateC(_).left.map(_.map(TransformationError(_))),
//      unhappy.validateD(_).left.map(_.map(TransformationError(_)))
//    )
//
//  @Benchmark
//  def validateTransformFUnhappyByHandErrorsSwap: M[SimpleOutput] =
//    validateByHandHandleErrors(
//      simple,
//      unhappy.validateA(_).left.map(_.map(TransformationError(_))),
//      unhappy.validateB(_).left.map(_.map(TransformationError(_))),
//      unhappy.validateC(_).left.map(_.map(TransformationError(_))),
//      unhappy.validateD(_).left.map(_.map(TransformationError(_)))
//    )
//
//  @Benchmark
//  def validateTransformFUnhappyByHandErrorsPatMat: M[SimpleOutput] =
//    validateDontDoThisAtHomeKids(
//      simple,
//      unhappy.validateA(_).left.map(_.map(TransformationError(_))),
//      unhappy.validateB(_).left.map(_.map(TransformationError(_))),
//      unhappy.validateC(_).left.map(_.map(TransformationError(_))),
//      unhappy.validateD(_).left.map(_.map(TransformationError(_)))
//    )

//  private def validateByHandHappyFlatMap(
//      simple: Simple,
//      fa: Int => M[Int],
//      fb: Double => M[Double],
//      fc: String => M[String],
//      fd: Option[String] => M[Option[String]]
//  ): M[SimpleOutput] =
//    for {
//      valA <- fa(simple.a)
//      valB <- fb(simple.b)
//      valC <- fc(simple.c)
//      valD <- fd(simple.d)
//    } yield SimpleOutput(valA, valB, valC, valD)
//
//  private def validateByHandHandleErrors(
//      simple: Simple,
//      fa: Int => M[Int],
//      fb: Double => M[Double],
//      fc: String => M[String],
//      fd: Option[String] => M[Option[String]]
//  ): M[SimpleOutput] = {
//    val valA = fa(simple.a)
//    val valB = fb(simple.b)
//    val valC = fc(simple.c)
//    val valD = fd(simple.d)
//
//    val errs = ListBuffer.empty[TransformationError[String]]
//    if (valA.isLeft) errs ++= valA.swap.getOrElse(Nil)
//    if (valB.isLeft) errs ++= valB.swap.getOrElse(Nil)
//    if (valC.isLeft) errs ++= valC.swap.getOrElse(Nil)
//    if (valD.isLeft) errs ++= valD.swap.getOrElse(Nil)
//
//    if (errs.isEmpty) Right(SimpleOutput(valA.toOption.get, valB.toOption.get, valC.toOption.get, valD.toOption.get))
//    else Left(errs.toVector)
//  }
//
//  private def validateDontDoThisAtHomeKids(
//      simple: Simple,
//      fa: Int => M[Int],
//      fb: Double => M[Double],
//      fc: String => M[String],
//      fd: Option[String] => M[Option[String]]
//  ): M[SimpleOutput] = {
//    fa(simple.a) match {
//      case Right(a) =>
//        fb(simple.b) match {
//          case Right(b) =>
//            fc(simple.c) match {
//              case Right(c) =>
//                fd(simple.d) match {
//                  case Right(d)         => Right(SimpleOutput(a, b, c, d))
//                  case retVal @ Left(_) => retVal.asInstanceOf[M[SimpleOutput]]
//                }
//              case Left(errs3) =>
//                fd(simple.d) match {
//                  case Right(_)    => Left(errs3)
//                  case Left(errs4) => Left(errs3 ++ errs4)
//                }
//            }
//          case Left(errs2) =>
//            fc(simple.c) match {
//              case Right(_) =>
//                fd(simple.d) match {
//                  case Right(_)    => Left(errs2)
//                  case Left(errs4) => Left(errs2 ++ errs4)
//                }
//              case Left(errs3) =>
//                fd(simple.d) match {
//                  case Right(_)    => Left(errs2 ++ errs3)
//                  case Left(errs4) => Left(errs2 ++ errs3 ++ errs4)
//                }
//            }
//        }
//      case Left(errs1) =>
//        fb(simple.b) match {
//          case Right(_) =>
//            fc(simple.c) match {
//              case Right(_) =>
//                fd(simple.d) match {
//                  case Right(_)    => Left(errs1)
//                  case Left(errs4) => Left(errs1 ++ errs4)
//                }
//              case Left(errs3) =>
//                fd(simple.d) match {
//                  case Right(_)    => Left(errs3)
//                  case Left(errs4) => Left(errs1 ++ errs3 ++ errs4)
//                }
//            }
//          case Left(errs2) =>
//            fc(simple.c) match {
//              case Right(_) =>
//                fd(simple.d) match {
//                  case Right(_)    => Left(errs1 ++ errs2)
//                  case Left(errs4) => Left(errs1 ++ errs2 ++ errs4)
//                }
//              case Left(errs3) =>
//                fd(simple.d) match {
//                  case Right(_)    => Left(errs1 ++ errs2 ++ errs3)
//                  case Left(errs4) => Left(errs1 ++ errs2 ++ errs3 ++ errs4)
//                }
//            }
//        }
//    }
//  }

}
