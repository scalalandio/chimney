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

  implicit def function0[Out]: IsFunction.Of[() => Out, Out] =
    impl.asInstanceOf[IsFunction.Of[() => Out, Out]]
  implicit def function1[A, Out]: IsFunction.Of[A => Out, Out] =
    impl.asInstanceOf[IsFunction.Of[A => Out, Out]]
  implicit def function2[A, B, Out]: IsFunction.Of[(A, B) => Out, Out] =
    impl.asInstanceOf[IsFunction.Of[(A, B) => Out, Out]]
  // TODO: arities up to 22
}
