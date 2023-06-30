package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Existentials { this: Types with Exprs =>

  /** Represents value with some existential type and Type using the same existential. 
   * 
   * Since Scala 3 removed a lot of cases for existential types we cannot just use Type[?] in shared code.
   * Additionally, we might need to have something to prove that our Type[?] is has the same ? as some Value[?].
   * For that, this utility would be useful.
   */
  final protected type Existential[F[_]] = Existential.Bounded[Nothing, Any, F]
  protected object Existential {

    /** Bounded version which allows expressing L <:< A <:< U where it's needed. */
    sealed trait Bounded[L, U >: L, F[_ >: L <: U]] {

      type Underlying >: L <: U
      val Underlying: Type[Underlying]

      val value: F[Underlying]

      def mapK[G[_]](f: Type[Underlying] => F[Underlying] => G[Underlying]): Bounded[L, U, G] =
        Bounded[L, U, G, Underlying](f(Underlying)(value))(Underlying)
    }
    object Bounded {

      def apply[L, U >: L, F[_ >: L <: U], A >: L <: U: Type](value: F[A]): Bounded[L, U, F] =
        new Impl[L, U, F, A](Type[A], value)
    }
    private class Impl[L, U >: L, F[_ >: L <: U], A >: L <: U](
        val Underlying: Type[A],
        val value: F[A]
    ) extends Bounded[L, U, F] {
      type Underlying = A
    }

    def apply[F[_], A: Type](value: F[A]): Existential[F] = Bounded[Nothing, Any, F, A](value)

    def use[F[_], Out](e: Existential[F])(thunk: Type[e.Underlying] => F[e.Underlying] => Out): Out =
      thunk(e.Underlying)(e.value)
    def use2[F1[_], F2[_], Out](e1: Existential[F1], e2: Existential[F2])(
        thunk: Type[e1.Underlying] => Type[e2.Underlying] => (F1[e1.Underlying], F2[e2.Underlying]) => Out
    ): Out = thunk(e1.Underlying)(e2.Underlying)(e1.value, e2.value)
  }

  /** Convenient utility to represent Type[?] with erased inner type, but without any accompanying value. */
  final protected type ExistentialType = Existential[Type]
  protected object ExistentialType {

    /** Convenient utility to represent Type[? >: L <: U] with erased inner type, but without any accompanying value. */
    type Bounded[L, U >: L] = Existential.Bounded[L, U, Type]
    object Bounded {
      def apply[L, U >: L, A >: L <: U](implicit A: Type[A]): Bounded[L, U] = Existential.Bounded[L, U, Type, A](A)

      def use[L, U >: L, Out](et: ExistentialType.Bounded[L, U])(thunk: Type[et.Underlying] => Out): Out = thunk(
        et.Underlying
      )
      def use2[L1, U1 >: L1, L2, U2 >: L2, Out](
          et1: ExistentialType.Bounded[L1, U1],
          et2: ExistentialType.Bounded[L2, U2]
      )(
          thunk: Type[et1.Underlying] => Type[et2.Underlying] => Out
      ): Out = use(et2)(use(et1)(thunk))
      def use3[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, Out](
          et1: ExistentialType.Bounded[L1, U1],
          et2: ExistentialType.Bounded[L2, U2],
          et3: ExistentialType.Bounded[L3, U3]
      )(
          thunk: Type[et1.Underlying] => Type[et2.Underlying] => Type[et3.Underlying] => Out
      ): Out = use(et3)(use2(et1, et2)(thunk))
      def use4[L1, U1 >: L1, L2, U2 >: L2, L3, U3 >: L3, L4, U4 >: L4, Out](
          et1: ExistentialType.Bounded[L1, U1],
          et2: ExistentialType.Bounded[L2, U2],
          et3: ExistentialType.Bounded[L3, U3],
          et4: ExistentialType.Bounded[L4, U4]
      )(
          thunk: Type[et1.Underlying] => Type[et2.Underlying] => Type[et3.Underlying] => Type[et4.Underlying] => Out
      ): Out = use(et4)(use3(et1, et2, et3)(thunk))
    }

    /** Convenient utility to represent Type[? >: L] with erased inner type, but without any accompanying value. */
    type LowerBounded[L] = Existential.Bounded[L, Any, Type]
    object LowerBounded {
      def apply[L, A >: L](implicit A: Type[A]): Bounded[L, Any] = Existential.Bounded[L, Any, Type, A](A)
    }

    /** Convenient utility to represent Type[? <: U] with erased inner type, but without any accompanying value. */
    type UpperBounded[U] = Existential.Bounded[Nothing, U, Type]
    object UpperBounded {
      def apply[U, A <: U](implicit A: Type[A]): Bounded[Nothing, U] = Existential.Bounded[Nothing, U, Type, A](A)
    }

    def apply[A](implicit A: Type[A]): ExistentialType = Existential[Type, A](A)(A)

    def prettyPrint(existentialType: ExistentialType): String = Type.prettyPrint(existentialType.Underlying)

    // Different arities of use* allow us to avoid absurdly nested blocks, since only 1-parameter lambda can have
    // implicit parameter.

    def use[Out](et: ExistentialType)(thunk: Type[et.Underlying] => Out): Out = thunk(et.Underlying)
    def use2[Out](et1: ExistentialType, et2: ExistentialType)(
        thunk: Type[et1.Underlying] => Type[et2.Underlying] => Out
    ): Out = use(et2)(use(et1)(thunk))
    def use3[Out](et1: ExistentialType, et2: ExistentialType, et3: ExistentialType)(
        thunk: Type[et1.Underlying] => Type[et2.Underlying] => Type[et3.Underlying] => Out
    ): Out = use(et3)(use2(et1, et2)(thunk))
    def use4[Out](et1: ExistentialType, et2: ExistentialType, et3: ExistentialType, et4: ExistentialType)(
        thunk: Type[et1.Underlying] => Type[et2.Underlying] => Type[et3.Underlying] => Type[et4.Underlying] => Out
    ): Out = use(et4)(use3(et1, et2, et3)(thunk))
  }

  /** Convenient utility to represent Expr[?] with erased inner type with accompanying Type[?] of the same ?. */
  final protected type ExistentialExpr = Existential[Expr]
  protected object ExistentialExpr {

    def apply[A: Type](expr: Expr[A]): ExistentialExpr = Existential[Expr, A](expr)

    def withoutType[A](expr: Expr[A]): ExistentialExpr = apply(expr)(Expr.typeOf(expr))

    def prettyPrint(existentialExpr: ExistentialExpr): String = Expr.prettyPrint(existentialExpr.value)

    def use[Out](e: ExistentialExpr)(thunk: Type[e.Underlying] => Expr[e.Underlying] => Out): Out =
      thunk(e.Underlying)(e.value)
  }
}
