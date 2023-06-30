package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Existentials { this: Types with Exprs =>

  /** Represents value with some existential type and Type using the same existential. 
   * 
   * Since Scala 3 removed a lot of cases for existential types we cannot just use Type[?] in shared code.
   * Additionally, we might need to have something to prove that our Type[?] is has the same ? as some Value[?].
   * For that, this utility would be useful.
   */
  final protected type Existential[F[_]] = Existential.Bounded[Nothing, Any, F]
  protected object Existential extends ExistentialCompanion {

    /** Bounded version which allows expressing L <:< A <:< U where it's needed. */
    sealed trait Bounded[L, U >: L, F[_ >: L <: U]] {

      type Underlying >: L <: U
      val Underlying: Type[Underlying]

      val value: F[Underlying]

      def mapK[G[_]](f: Type[Underlying] => F[Underlying] => G[Underlying]): Bounded[L, U, G] =
        Bounded[L, U, G, Underlying](f(Underlying)(value))(Underlying)
    }
    object Bounded extends ExistentialCompanion {
      def apply[L, U >: L, F[_ >: L <: U], A >: L <: U: Type](value: F[A]): Bounded[L, U, F] =
        new Impl[L, U, F, A](Type[A], value)
    }
    type LowerBounded[L, F[_ >: L]] = Existential.Bounded[L, Any, F]
    object LowerBounded extends ExistentialCompanion {
      def apply[L, F[_ >: L], A >: L](value: F[A])(implicit A: Type[A]): LowerBounded[L, F] =
        Existential.Bounded[L, Any, F, A](value)
    }
    type UpperBounded[U, F[_ <: U]] = Existential.Bounded[Nothing, U, F]
    object UpperBounded extends ExistentialCompanion {
      def apply[U, F[_ <: U], A <: U](value: F[A])(implicit A: Type[A]): UpperBounded[U, F] =
        Existential.Bounded[Nothing, U, F, A](value)
    }

    def apply[F[_], A: Type](value: F[A]): Existential[F] = Bounded[Nothing, Any, F, A](value)
  }

  /** Convenient utility to represent Type[?] with erased inner type, but without any accompanying value. */
  final protected type ExistentialType = ExistentialType.Bounded[Nothing, Any]
  protected object ExistentialType extends ExistentialTypeCompanion {

    /** Convenient utility to represent Type[? >: L <: U] with erased inner type, but without any accompanying value. */
    type Bounded[L, U >: L] = Existential.Bounded[L, U, Type]
    object Bounded extends ExistentialTypeCompanion {
      def apply[L, U >: L, A >: L <: U](implicit A: Type[A]): Bounded[L, U] = Existential.Bounded[L, U, Type, A](A)
    }

    /** Convenient utility to represent Type[? >: L] with erased inner type, but without any accompanying value. */
    type LowerBounded[L] = Existential.Bounded[L, Any, Type]
    object LowerBounded extends ExistentialTypeCompanion {
      def apply[L, A >: L](implicit A: Type[A]): Bounded[L, Any] = Existential.Bounded[L, Any, Type, A](A)
    }

    /** Convenient utility to represent Type[? <: U] with erased inner type, but without any accompanying value. */
    type UpperBounded[U] = Existential.Bounded[Nothing, U, Type]
    object UpperBounded extends ExistentialTypeCompanion {
      def apply[U, A <: U](implicit A: Type[A]): Bounded[Nothing, U] = Existential.Bounded[Nothing, U, Type, A](A)
    }

    def apply[A](implicit A: Type[A]): ExistentialType = Existential[Type, A](A)(A)

    def prettyPrint(existentialType: ExistentialType): String = Type.prettyPrint(existentialType.Underlying)

    // Different arities of use* allow us to avoid absurdly nested blocks, since only 1-parameter lambda can have
    // implicit parameter.
  }

  /** Convenient utility to represent Expr[?] with erased inner type with accompanying Type[?] of the same ?. */
  final protected type ExistentialExpr = Existential[Expr]
  protected object ExistentialExpr extends ExistentialCompanion {

    def apply[A: Type](expr: Expr[A]): ExistentialExpr = Existential[Expr, A](expr)

    def withoutType[A](expr: Expr[A]): ExistentialExpr = apply(expr)(Expr.typeOf(expr))

    def prettyPrint(existentialExpr: ExistentialExpr): String = Expr.prettyPrint(existentialExpr.value)
  }

  private class Impl[L, U >: L, F[_ >: L <: U], A >: L <: U](
      val Underlying: Type[A],
      val value: F[A]
  ) extends Existential.Bounded[L, U, F] {
    type Underlying = A
  }

  sealed trait ExistentialCompanion {

    // Different arities of use* allow us to avoid absurdly nested blocks, since only 1-parameter lambda can have
    // implicit parameter.

    def use[L, U >: L, F[_ >: L <: U], Out](et: Existential.Bounded[L, U, F])(
        thunk: Type[et.Underlying] => F[et.Underlying] => Out
    ): Out = thunk(et.Underlying)(et.value)

    def use2[L1, U1 >: L1, F1[_ >: L1 <: U1], L2, U2 >: L2, F2[_ >: L2 <: U2], Out](
        et1: Existential.Bounded[L1, U1, F1],
        et2: Existential.Bounded[L2, U2, F2]
    )(
        thunk: Type[et1.Underlying] => Type[et2.Underlying] => (F1[et1.Underlying], F2[et2.Underlying]) => Out
    ): Out = thunk(et1.Underlying)(et2.Underlying)(et1.value, et2.value)

    def use3[L1, U1 >: L1, F1[_ >: L1 <: U1], L2, U2 >: L2, F2[_ >: L2 <: U2], L3, U3 >: L3, F3[_ >: L3 <: U3], Out](
        et1: Existential.Bounded[L1, U1, F1],
        et2: Existential.Bounded[L2, U2, F2],
        et3: Existential.Bounded[L3, U3, F3]
    )(
        thunk: Type[et1.Underlying] => Type[et2.Underlying] => Type[et3.Underlying] => (
            F1[et1.Underlying],
            F2[et2.Underlying],
            F3[et3.Underlying]
        ) => Out
    ): Out = thunk(et1.Underlying)(et2.Underlying)(et3.Underlying)(et1.value, et2.value, et3.value)

    def use4[L1, U1 >: L1, F1[_ >: L1 <: U1], L2, U2 >: L2, F2[_ >: L2 <: U2], L3, U3 >: L3, F3[
        _ >: L3 <: U3
    ], L4, U4 >: L4, F4[_ >: L4 <: U4], Out](
        et1: Existential.Bounded[L1, U1, F1],
        et2: Existential.Bounded[L2, U2, F2],
        et3: Existential.Bounded[L3, U3, F3],
        et4: Existential.Bounded[L4, U4, F4]
    )(
        thunk: Type[et1.Underlying] => Type[et2.Underlying] => Type[et3.Underlying] => Type[et4.Underlying] => (
            F1[et1.Underlying],
            F2[et2.Underlying],
            F3[et3.Underlying],
            F4[et4.Underlying]
        ) => Out
    ): Out = thunk(et1.Underlying)(et2.Underlying)(et3.Underlying)(et4.Underlying)(
      et1.value,
      et2.value,
      et3.value,
      et4.value
    )
  }

  sealed trait ExistentialTypeCompanion {

    // Different arities of use* allow us to avoid absurdly nested blocks, since only 1-parameter lambda can have
    // implicit parameter.

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
}
