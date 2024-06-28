package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Existentials { this: Types with Exprs =>

  /** Represents value with some existential type `t` both for `Type[t]` as well as `F[t]`.
    *
    * Since Scala 3 removed a lot of cases for existential types we cannot just use `Type[?]` in shared code.
    * Additionally, we might need to have something to prove that our `Type[?]` is has the same `?` as some `Value[?]`.
    * For that, this utility would be useful.
    */
  final protected type Existential[F[_]] = Existential.Bounded[Nothing, Any, F]
  protected object Existential {

    /** Bounded version which allows expressing `L <:< A <:< U` where it's needed. */
    sealed trait Bounded[L, U >: L, F[_ >: L <: U]] {

      type Underlying >: L <: U
      implicit val Underlying: Type[Underlying]

      val value: F[Underlying]

      def mapK[G[_]](f: Type[Underlying] => F[Underlying] => G[Underlying]): Bounded[L, U, G] =
        Bounded[L, U, G, Underlying](f(Underlying)(value))(Underlying)
    }
    object Bounded {
      def apply[L, U >: L, F[_ >: L <: U], A >: L <: U: Type](value: F[A]): Bounded[L, U, F] =
        new Impl[L, U, F, A](Type[A], value)
    }
    type LowerBounded[L, F[_ >: L]] = Existential.Bounded[L, Any, F]
    object LowerBounded {
      def apply[L, F[_ >: L], A >: L](value: F[A])(implicit A: Type[A]): LowerBounded[L, F] =
        Existential.Bounded[L, Any, F, A](value)
    }
    type UpperBounded[U, F[_ <: U]] = Existential.Bounded[Nothing, U, F]
    object UpperBounded {
      def apply[U, F[_ <: U], A <: U](value: F[A])(implicit A: Type[A]): UpperBounded[U, F] =
        Existential.Bounded[Nothing, U, F, A](value)
    }

    def apply[F[_], A: Type](value: F[A]): Existential[F] = Bounded[Nothing, Any, F, A](value)
  }

  /** Convenient utility to represent `Type[?]` with erased inner type, but without any accompanying value. */
  final protected type ExistentialType = ExistentialType.Bounded[Nothing, Any]
  protected object ExistentialType {

    /** Convenient utility to represent `Type[? >: L <: U]` with erased inner type, but without any accompanying value.
      */
    type Bounded[L, U >: L] = Existential.Bounded[L, U, Type]
    object Bounded {
      def apply[L, U >: L, A >: L <: U](implicit A: Type[A]): Bounded[L, U] = Existential.Bounded[L, U, Type, A](A)
    }

    /** Convenient utility to represent `Type[? >: L]` with erased inner type, but without any accompanying value. */
    type LowerBounded[L] = Existential.Bounded[L, Any, Type]
    object LowerBounded {
      def apply[L, A >: L](implicit A: Type[A]): Bounded[L, Any] = Existential.Bounded[L, Any, Type, A](A)
    }

    /** Convenient utility to represent `Type[? <: U]` with erased inner type, but without any accompanying value. */
    type UpperBounded[U] = Existential.Bounded[Nothing, U, Type]
    object UpperBounded {
      def apply[U, A <: U](implicit A: Type[A]): Bounded[Nothing, U] = Existential.Bounded[Nothing, U, Type, A](A)
    }

    def apply[A](implicit A: Type[A]): ExistentialType = Existential[Type, A](A)(A)

    def prettyPrint(existentialType: ExistentialType): String = Type.prettyPrint(existentialType.Underlying)
  }

  /** Convenient utility to represent `Expr[?]` with erased inner type with accompanying `Type[?]` of the same `?`. */
  final protected type ExistentialExpr = Existential[Expr]
  protected object ExistentialExpr {

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

  /** Convenient for literal singletons */
  type Id[A] = A

  // aliases to make the (very common) existential types shorter

  type ?? = ExistentialType
  type ?>[L] = ExistentialType.LowerBounded[L]
  type ?<[U] = ExistentialType.UpperBounded[U]
  type >?<[L, U >: L] = ExistentialType.Bounded[L, U]
}
