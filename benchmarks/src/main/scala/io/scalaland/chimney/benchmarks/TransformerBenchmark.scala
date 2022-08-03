package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.TransformerF

import scala.collection.mutable.ListBuffer

class TransformerBenchmark extends CommonBenchmarkSettings {
  import fixtures._
  import samples.validation._

  type M[+A] = Either[Vector[String], A]

  var simple: Simple = samples.simpleSample

  private final val transformerF: TransformerF[M, Simple, SimpleOutput] =
    TransformerF
      .define[M, Simple, SimpleOutput]
      .withFieldComputedF(_.a, s => happy.validateA(s.a))
      .withFieldComputedF(_.b, s => happy.validateB(s.b))
      .withFieldComputedF(_.c, s => happy.validateC(s.c))
      .withFieldComputedF(_.d, s => happy.validateD(s.d))
      .buildTransformer

  private final val transformerFUnhappy: TransformerF[M, Simple, SimpleOutput] =
    TransformerF
      .define[M, Simple, SimpleOutput]
      .withFieldComputedF(_.a, s => unhappy.validateA(s.a))
      .withFieldComputedF(_.b, s => unhappy.validateB(s.b))
      .withFieldComputedF(_.c, s => unhappy.validateC(s.c))
      .withFieldComputedF(_.d, s => unhappy.validateD(s.d))
      .buildTransformer

  @Benchmark
  def validateTransformFHappyChimney: M[SimpleOutput] =
    transformerF.transform(simple)

  @Benchmark
  def validateTransformFHappyChimneyWithDsl: M[SimpleOutput] =
    simple
      .intoF[M, SimpleOutput]
      .withFieldComputedF(_.a, s => happy.validateA(s.a))
      .withFieldComputedF(_.b, s => happy.validateB(s.b))
      .withFieldComputedF(_.c, s => happy.validateC(s.c))
      .withFieldComputedF(_.d, s => happy.validateD(s.d))
      .transform

  @Benchmark
  def validateTransformFHappyByHandFlatMap: M[SimpleOutput] =
    validateByHandHappyFlatMap(simple, happy.validateA, happy.validateB, happy.validateC, happy.validateD)

  @Benchmark
  def validateTransformFHappyByHandErrorsSwap: M[SimpleOutput] =
    validateByHandHandleErrors(simple, happy.validateA, happy.validateB, happy.validateC, happy.validateD)

  @Benchmark
  def validateTransformFHappyByHandErrorsPatMat: M[SimpleOutput] =
    validateDontDoThisAtHomeKids(simple, happy.validateA, happy.validateB, happy.validateC, happy.validateD)

  @Benchmark
  def validateTransformFUnhappyChimney: M[SimpleOutput] =
    transformerFUnhappy.transform(simple)

  @Benchmark
  def validateTransformFUnhappyChimneyWithDsl: M[SimpleOutput] =
    simple
      .intoF[M, SimpleOutput]
      .withFieldComputedF(_.a, s => unhappy.validateA(s.a))
      .withFieldComputedF(_.b, s => unhappy.validateB(s.b))
      .withFieldComputedF(_.c, s => unhappy.validateC(s.c))
      .withFieldComputedF(_.d, s => unhappy.validateD(s.d))
      .transform

  @Benchmark
  def validateTransformFUnhappyByHandFlatMap: M[SimpleOutput] =
    validateByHandHappyFlatMap(simple, unhappy.validateA, unhappy.validateB, unhappy.validateC, unhappy.validateD)

  @Benchmark
  def validateTransformFUnhappyByHandErrorsSwap: M[SimpleOutput] =
    validateByHandHandleErrors(simple, unhappy.validateA, unhappy.validateB, unhappy.validateC, unhappy.validateD)

  @Benchmark
  def validateTransformFUnhappyByHandErrorsPatMat: M[SimpleOutput] =
    validateDontDoThisAtHomeKids(simple, unhappy.validateA, unhappy.validateB, unhappy.validateC, unhappy.validateD)

  private def validateByHandHappyFlatMap(
      simple: Simple,
      fa: Int => M[Int],
      fb: Double => M[Double],
      fc: String => M[String],
      fd: Option[String] => M[Option[String]]
  ): M[SimpleOutput] =
    for {
      valA <- fa(simple.a)
      valB <- fb(simple.b)
      valC <- fc(simple.c)
      valD <- fd(simple.d)
    } yield SimpleOutput(valA, valB, valC, valD)

  private def validateByHandHandleErrors(
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

    val errs = ListBuffer.empty[String]
    if (valA.isLeft) errs ++= valA.swap.getOrElse(Nil)
    if (valB.isLeft) errs ++= valB.swap.getOrElse(Nil)
    if (valC.isLeft) errs ++= valC.swap.getOrElse(Nil)
    if (valD.isLeft) errs ++= valD.swap.getOrElse(Nil)

    if (errs.isEmpty) Right(SimpleOutput(valA.toOption.get, valB.toOption.get, valC.toOption.get, valD.toOption.get))
    else Left(errs.toVector)
  }

  private def validateDontDoThisAtHomeKids(
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
