package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.partial

// $COVERAGE-OFF$evidence used only within macro-generated expressions

/** Exists because there is no `f.andThen(g)` on a function of arbitrary arity.
  *
  * We use it for `.withConstructorEither` for lifting it into `partial.Result` constructor.
  *
  * @since 1.0.0
  */
trait FunctionEitherToResult[FnE] {
  type FnR

  def lift(fn: FnE): FnR
}
object FunctionEitherToResult extends FunctionEitherToResultImplicits0 {

  type Aux[FnE, FnR0] = FunctionEitherToResult[FnE] { type FnR = FnR0 }

  def lift[FnE, FnR](fn: FnE)(implicit liftFn: Aux[FnE, FnR]): FnR = liftFn.lift(fn)
}
private[runtime] trait FunctionEitherToResultImplicits0 extends FunctionEitherToResultImplicits1 {
  this: FunctionEitherToResult.type =>

  private def make[FnE, FnR0](l: FnE => FnR0): Aux[FnE, FnR0] = new FunctionEitherToResult[FnE] {
    type FnR = FnR0
    def lift(fn: FnE): FnR = l(fn)
  }

  implicit def curriedFunction0[Mid, Out](implicit ev: Aux[Mid, Out]): Aux[() => Mid, () => Out] =
    make(fn => () => ev.lift(fn()))
  implicit def curriedFunction1[A, Mid, Out](implicit ev: Aux[Mid, Out]): Aux[A => Mid, A => Out] =
    make(fn => a => ev.lift(fn(a)))
  implicit def curriedFunction2[A, B, Mid, Out](implicit ev: Aux[Mid, Out]): Aux[(A, B) => Mid, (A, B) => Out] =
    make(fn => (a, b) => ev.lift(fn(a, b)))
  implicit def curriedFunction3[A, B, C, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C) => Mid, (A, B, C) => Out] =
    make(fn => (a, b, c) => ev.lift(fn(a, b, c)))
  implicit def curriedFunction4[A, B, C, D, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D) => Mid, (A, B, C, D) => Out] =
    make(fn => (a, b, c, d) => ev.lift(fn(a, b, c, d)))
  implicit def curriedFunction5[A, B, C, D, E, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D, E) => Mid, (A, B, C, D, E) => Out] =
    make(fn => (a, b, c, d, e) => ev.lift(fn(a, b, c, d, e)))
  implicit def curriedFunction6[A, B, C, D, E, F, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D, E, F) => Mid, (A, B, C, D, E, F) => Out] =
    make(fn => (a, b, c, d, e, f) => ev.lift(fn(a, b, c, d, e, f)))
  implicit def curriedFunction7[A, B, C, D, E, F, G, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D, E, F, G) => Mid, (A, B, C, D, E, F, G) => Out] =
    make(fn => (a, b, c, d, e, f, g) => ev.lift(fn(a, b, c, d, e, f, g)))
  implicit def curriedFunction8[A, B, C, D, E, F, G, H, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D, E, F, G, H) => Mid, (A, B, C, D, E, F, G, H) => Out] =
    make(fn => (a, b, c, d, e, f, g, h) => ev.lift(fn(a, b, c, d, e, f, g, h)))
  implicit def curriedFunction9[A, B, C, D, E, F, G, H, I, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D, E, F, G, H, I) => Mid, (A, B, C, D, E, F, G, H, I) => Out] =
    make(fn => (a, b, c, d, e, f, g, h, i) => ev.lift(fn(a, b, c, d, e, f, g, h, i)))
  implicit def curriedFunction10[A, B, C, D, E, F, G, H, I, J, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D, E, F, G, H, I, J) => Mid, (A, B, C, D, E, F, G, H, I, J) => Out] =
    make(fn => (a, b, c, d, e, f, g, h, i, j) => ev.lift(fn(a, b, c, d, e, f, g, h, i, j)))
  implicit def curriedFunction11[A, B, C, D, E, F, G, H, I, J, K, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D, E, F, G, H, I, J, K) => Mid, (A, B, C, D, E, F, G, H, I, J, K) => Out] =
    make(fn => (a, b, c, d, e, f, g, h, i, j, k) => ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k)))
  implicit def curriedFunction12[A, B, C, D, E, F, G, H, I, J, K, L, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D, E, F, G, H, I, J, K, L) => Mid, (A, B, C, D, E, F, G, H, I, J, K, L) => Out] =
    make(fn => (a, b, c, d, e, f, g, h, i, j, k, l) => ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k, l)))
  implicit def curriedFunction13[A, B, C, D, E, F, G, H, I, J, K, L, M, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M) => Mid, (A, B, C, D, E, F, G, H, I, J, K, L, M) => Out] =
    make(fn => (a, b, c, d, e, f, g, h, i, j, k, l, m) => ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k, l, m)))
  implicit def curriedFunction14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Mid, (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out] =
    make(fn => (a, b, c, d, e, f, g, h, i, j, k, l, m, n) => ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n)))
  implicit def curriedFunction15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Mid, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))
    )
  implicit def curriedFunction16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Mid,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) => ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))
    )
  implicit def curriedFunction17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Mid,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
        ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))
    )
  implicit def curriedFunction18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Mid,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
        ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))
    )
  implicit def curriedFunction19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Mid,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
        ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))
    )
  implicit def curriedFunction20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Mid,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
        ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))
    )
  implicit def curriedFunction21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Mid,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
        ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))
    )
  implicit def curriedFunction22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Mid, Out](implicit
      ev: Aux[Mid, Out]
  ): Aux[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Mid,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out
  ] =
    make(fn =>
      (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
        ev.lift(fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))
    )
}
private[runtime] trait FunctionEitherToResultImplicits1 { this: FunctionEitherToResult.type =>

  private def make[FnE, FnR0](l: FnE => FnR0): Aux[FnE, FnR0] = new FunctionEitherToResult[FnE] {
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
