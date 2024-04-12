package io.scalaland.chimney.internal.runtime

import scala.annotation.implicitNotFound

/** Allow us to provide some better IDE support than just accepting everything as a parameter and waiting for the macro
  * to scream with compilation error, in the world where different function types have no common ancestor (other than
  * AnyRef).
  *
  * @since 0.8.4
  */
@implicitNotFound(
  "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of the target type, got ${Fn}"
)
sealed trait IsFunction[Fn] {
  type Out
}
object IsFunction extends IsFunctionLowPriorityImplicits {
  @implicitNotFound(
    "Expected function of any arity (scala.Function0, scala.Function1, scala.Function2, ...) that returns a value of ${Out0}, got ${Fn}"
  )
  type Of[Fn, Out0] = IsFunction[Fn] { type Out = Out0 }

  private def cast[A, Out](of: Of[?, Out]): Of[A, Out] = of.asInstanceOf[Of[A, Out]]

  implicit def curriedFunction0[Mid, Out](implicit ev: Of[Mid, Out]): Of[() => Mid, Out] = cast(ev)
  implicit def curriedFunction1[A, Mid, Out](implicit ev: Of[Mid, Out]): Of[A => Mid, Out] = cast(ev)
  implicit def curriedFunction2[A, B, Mid, Out](implicit ev: Of[Mid, Out]): Of[(A, B) => Mid, Out] = cast(ev)
  implicit def curriedFunction3[A, B, C, Mid, Out](implicit ev: Of[Mid, Out]): Of[(A, B, C) => Mid, Out] = cast(ev)
  implicit def curriedFunction4[A, B, C, D, Mid, Out](implicit ev: Of[Mid, Out]): Of[(A, B, C, D) => Mid, Out] = cast(
    ev
  )
  implicit def curriedFunction5[A, B, C, D, E, Mid, Out](implicit ev: Of[Mid, Out]): Of[(A, B, C, D, E) => Mid, Out] =
    cast(ev)
  implicit def curriedFunction6[A, B, C, D, E, F, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F) => Mid, Out] = cast(ev)
  implicit def curriedFunction7[A, B, C, D, E, F, G, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G) => Mid, Out] = cast(ev)
  implicit def curriedFunction8[A, B, C, D, E, F, G, H, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H) => Mid, Out] = cast(ev)
  implicit def curriedFunction9[A, B, C, D, E, F, G, H, I, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I) => Mid, Out] = cast(ev)
  implicit def curriedFunction10[A, B, C, D, E, F, G, H, I, J, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J) => Mid, Out] = cast(ev)
  implicit def curriedFunction11[A, B, C, D, E, F, G, H, I, J, K, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K) => Mid, Out] = cast(ev)
  implicit def curriedFunction12[A, B, C, D, E, F, G, H, I, J, K, L, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K, L) => Mid, Out] = cast(ev)
  implicit def curriedFunction13[A, B, C, D, E, F, G, H, I, J, K, L, M, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K, L, M) => Mid, Out] = cast(ev)
  implicit def curriedFunction14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Mid, Out] = cast(ev)
  implicit def curriedFunction15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Mid, Out] = cast(ev)
  implicit def curriedFunction16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Mid, Out] = cast(ev)
  implicit def curriedFunction17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Mid, Out] = cast(ev)
  implicit def curriedFunction18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Mid, Out] = cast(ev)
  implicit def curriedFunction19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Mid, Out] = cast(ev)
  implicit def curriedFunction20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Mid, Out] = cast(ev)
  implicit def curriedFunction21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Mid, Out] = cast(ev)
  implicit def curriedFunction22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Mid, Out](implicit
      ev: Of[Mid, Out]
  ): Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Mid, Out] = cast(ev)
}
private[runtime] trait IsFunctionLowPriorityImplicits { this: IsFunction.type =>

  private val impl = new IsFunction[Any] {}
  private def cast[Fn, Out]: Of[Fn, Out] = impl.asInstanceOf[Of[Fn, Out]]

  implicit def function0[Out]: Of[() => Out, Out] = cast
  implicit def function1[A, Out]: Of[A => Out, Out] = cast
  implicit def function2[A, B, Out]: Of[(A, B) => Out, Out] = cast
  implicit def function3[A, B, C, Out]: Of[(A, B, C) => Out, Out] = cast
  implicit def function4[A, B, C, D, Out]: Of[(A, B, C, D) => Out, Out] = cast
  implicit def function5[A, B, C, D, E, Out]: Of[(A, B, C, D, E) => Out, Out] = cast
  implicit def function6[A, B, C, D, E, F, Out]: Of[(A, B, C, D, E, F) => Out, Out] = cast
  implicit def function7[A, B, C, D, E, F, G, Out]: Of[(A, B, C, D, E, F, G) => Out, Out] = cast
  implicit def function8[A, B, C, D, E, F, G, H, Out]: Of[(A, B, C, D, E, F, G, H) => Out, Out] = cast
  implicit def function9[A, B, C, D, E, F, G, H, I, Out]: Of[(A, B, C, D, E, F, G, H, I) => Out, Out] = cast
  implicit def function10[A, B, C, D, E, F, G, H, I, J, Out]: Of[(A, B, C, D, E, F, G, H, I, J) => Out, Out] = cast
  implicit def function11[A, B, C, D, E, F, G, H, I, J, K, Out]: Of[(A, B, C, D, E, F, G, H, I, J, K) => Out, Out] =
    cast
  implicit def function12[A, B, C, D, E, F, G, H, I, J, K, L, Out]
      : Of[(A, B, C, D, E, F, G, H, I, J, K, L) => Out, Out] = cast
  implicit def function13[A, B, C, D, E, F, G, H, I, J, K, L, M, Out]
      : Of[(A, B, C, D, E, F, G, H, I, J, K, L, M) => Out, Out] = cast
  implicit def function14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out]
      : Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out, Out] = cast
  implicit def function15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out]
      : Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out, Out] = cast
  implicit def function16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out]
      : Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out, Out] = cast
  implicit def function17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out]
      : Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out, Out] = cast
  implicit def function18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out]
      : Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out, Out] = cast
  implicit def function19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out]
      : Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out, Out] = cast
  implicit def function20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out]
      : Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out, Out] = cast
  implicit def function21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out]
      : Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out, Out] = cast
  implicit def function22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out]
      : Of[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out, Out] = cast
}
