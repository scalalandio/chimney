package io.scalaland.chimney.internal.runtime

import scala.annotation.implicitNotFound

// TODO: move implicit not found to use DSL, to be able to use ${To} in error message
@implicitNotFound(
  "Expected function of any arity (Function0, Function1, Function2, ...) that returns a value of the target type"
)
sealed trait IsFunction[Fn] {
  type Out
}
object IsFunction {
  type Of[Fn, Out0] = IsFunction[Fn] { type Out = Out0 }

  private val impl = new IsFunction[Any] {}
  private def cast[Fn, Out]: IsFunction.Of[Fn, Out] = impl.asInstanceOf[IsFunction.Of[Fn, Out]]

  implicit def function0[Out]: IsFunction.Of[() => Out, Out] = cast // TODO: is it needed or is it Function1?
  implicit def function1[A, Out]: IsFunction.Of[A => Out, Out] = cast
  implicit def function2[A, B, Out]: IsFunction.Of[(A, B) => Out, Out] = cast
  implicit def function3[A, B, C, Out]: IsFunction.Of[(A, B, C) => Out, Out] = cast
  implicit def function4[A, B, C, D, Out]: IsFunction.Of[(A, B, C, D) => Out, Out] = cast
  implicit def function5[A, B, C, D, E, Out]: IsFunction.Of[(A, B, C, D, E) => Out, Out] = cast
  implicit def function6[A, B, C, D, E, F, Out]: IsFunction.Of[(A, B, C, D, E, F) => Out, Out] = cast
  implicit def function7[A, B, C, D, E, F, G, Out]: IsFunction.Of[(A, B, C, D, E, F, G) => Out, Out] = cast
  implicit def function8[A, B, C, D, E, F, G, H, Out]: IsFunction.Of[(A, B, C, D, E, F, G, H) => Out, Out] = cast
  implicit def function9[A, B, C, D, E, F, G, H, I, Out]: IsFunction.Of[(A, B, C, D, E, F, G, H, I) => Out, Out] = cast
  implicit def function10[A, B, C, D, E, F, G, H, I, J, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J) => Out, Out] = cast
  implicit def function11[A, B, C, D, E, F, G, H, I, J, K, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K) => Out, Out] = cast
  implicit def function12[A, B, C, D, E, F, G, H, I, J, K, L, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K, L) => Out, Out] = cast
  implicit def function13[A, B, C, D, E, F, G, H, I, J, K, L, M, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K, L, M) => Out, Out] = cast
  implicit def function14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out, Out] = cast
  implicit def function15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out, Out] = cast
  implicit def function16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out, Out] = cast
  implicit def function17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out, Out] = cast
  implicit def function18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out, Out] = cast
  implicit def function19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out, Out] = cast
  implicit def function20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out, Out] = cast
  implicit def function21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out, Out] = cast
  implicit def function22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out]
      : IsFunction.Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out, Out] = cast

  // TODO: multiple parameter lists
}
