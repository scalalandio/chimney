package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.partial

trait FuncionEitherToResult[FnE] {
  type FnR

  def lift(fn: FnE): FnR
}

object FuncionEitherToResult {

  type Aux[FnE, FnR0] = FuncionEitherToResult[FnE] { type FnR = FnR0 }

  def lift[FnE, FnR](fn: FnE)(implicit liftFn: Aux[FnE, FnR]): FnR = liftFn.lift(fn)

  private def make[FnE, FnR0](l: FnE => FnR0): Aux[FnE, FnR0] = new FuncionEitherToResult[FnE] {
    type FnR = FnR0
    def lift(fn: FnE): FnR = l(fn)
  }

  implicit def fn0[Out]: Aux[() => Either[String, Out], () => partial.Result[Out]] =
    make(fn => () => partial.Result.fromEitherString(fn()))
  implicit def fn1[A, Out]: Aux[A => Either[String, Out], A => partial.Result[Out]] =
    make(fn => a => partial.Result.fromEitherString(fn(a)))
  implicit def fn2[A, B, Out]: Aux[(A, B) => Either[String, Out], (A, B) => partial.Result[Out]] =
    make(fn => (a, b) => partial.Result.fromEitherString(fn(a, b)))
  implicit def fn3[A, B, C, Out]: Aux[(A, B, C) => Either[String, Out], (A, B, C) => partial.Result[Out]] =
    make(fn => (a, b, c) => partial.Result.fromEitherString(fn(a, b, c)))
  implicit def fn4[A, B, C, D, Out]: Aux[(A, B, C, D) => Either[String, Out], (A, B, C, D) => partial.Result[Out]] =
    make(fn => (a, b, c, d) => partial.Result.fromEitherString(fn(a, b, c, d)))
  implicit def fn5[A, B, C, D, E, Out]
      : Aux[(A, B, C, D, E) => Either[String, Out], (A, B, C, D, E) => partial.Result[Out]] =
    make(fn => (a, b, c, d, e) => partial.Result.fromEitherString(fn(a, b, c, d, e)))
}
