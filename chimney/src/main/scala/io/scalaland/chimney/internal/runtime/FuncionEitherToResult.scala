package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.partial

// $COVERAGE-OFF$evidence used only within macro-generated expressions

/** Exists because there is no `f.andThen(g)` on a function of arbitrary arity.
  *
  * We use it for `.withConstructorEither` for lifting it into `partial.Result` constructor.
  *
  * @since 1.0.0
  */
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
  implicit def fn6[A, B, C, D, E, F, Out]
      : Aux[(A, B, C, D, E, F) => Either[String, Out], (A, B, C, D, E, F) => partial.Result[Out]] =
    make(fn => (a, b, c, d, e, f) => partial.Result.fromEitherString(fn(a, b, c, d, e, f)))
  implicit def fn7[A, B, C, D, E, F, G, Out]
      : Aux[(A, B, C, D, E, F, G) => Either[String, Out], (A, B, C, D, E, F, G) => partial.Result[Out]] =
    make(fn => (a, b, c, d, e, f, g) => partial.Result.fromEitherString(fn(a, b, c, d, e, f, g)))
  implicit def fn8[A, B, C, D, E, F, G, H, Out]: Aux[
    (A, B, C, D, E, F, G, H) => Either[String, Out],
    (A, B, C, D, E, F, G, H) => partial.Result[Out]
  ] =
    make(fn => (a, b, c, d, e, f, g, h) => partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h)))
  implicit def fn9[A, B, C, D, E, F, G, H, I, Out]: Aux[
    (A, B, C, D, E, F, G, H, I) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I) => partial.Result[Out]
  ] =
    make(fn => (a, b, c, d, e, f, g, h, i) => partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i)))
  implicit def fn10[A, B, C, D, E, F, G, H, I, J, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J) => partial.Result[Out]
  ] =
    make(fn => (a, b, c, d, e, f, g, h, i, j) => partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j)))
  implicit def fn11[A, B, C, D, E, F, G, H, I, J, K, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k) => partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k))
    )
  implicit def fn12[A, B, C, D, E, F, G, H, I, J, K, L, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K, L) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l) => partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k, l))
    )
  implicit def fn13[A, B, C, D, E, F, G, H, I, J, K, L, M, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K, L, M) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m) =>
        partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k, l, m))
    )
  implicit def fn14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =>
        partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n))
    )
  implicit def fn15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =>
        partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))
    )
  implicit def fn16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
        partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))
    )
  implicit def fn17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
        partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))
    )
  implicit def fn18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
        partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))
    )
  implicit def fn19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
        partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))
    )
  implicit def fn20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
        partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))
    )
  implicit def fn21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
        partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))
    )
  implicit def fn22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out]: Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Either[String, Out],
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => partial.Result[Out]
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
        partial.Result.fromEitherString(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))
    )
}
