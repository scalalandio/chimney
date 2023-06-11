package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Existentials { this: Types with Exprs =>

  /** Represents value with some existential type and Type using the same existential. 
   * 
   * Since Scala 3 removed a lot of cases for existential types we cannot just use Type[?] in shared code.
   * Additionally, we might need to have something to prove that our Type[?] is has the same ? as some Value[?].
   * For that, this utility would be useful.
   */
  sealed protected trait Existential[T[_], V[_]] {

    type Underlying
    val Underlying: Type[T[Underlying]]

    val value: V[Underlying]
  }
  protected object Existential {

    private class Impl[T[_], V[_], A](val Underlying: Type[T[A]], val value: V[A]) extends Existential[T, V] {

      type Underlying = A
    }

    def apply[T[_], V[_], A](value: V[A])(implicit TA: Type[T[A]]): Existential[T, V] = new Impl(TA, value)

    def use[T[_], V[_], Out](e: Existential[T, V])(thunk: Type[T[e.Underlying]] => V[e.Underlying] => Out): Out =
      thunk(e.Underlying)(e.value)
    def use2[T1[_], V1[_], T2[_], V2[_], Out](e1: Existential[T1, V1], e2: Existential[T2, V2])(
        thunk: Type[T1[e1.Underlying]] => Type[T2[e2.Underlying]] => (V1[e1.Underlying], V2[e2.Underlying]) => Out
    ): Out = thunk(e1.Underlying)(e2.Underlying)(e1.value, e2.value)
  }

  final protected type Id[A] = A

  /** Convenient utility to represent Type[?] with erased inner type, but without any accompanying value. */
  final protected type ExistentialType = Existential[Id, Type]
  protected object ExistentialType {

    def apply[A](tpe: Type[A]): ExistentialType = Existential[Id, Type, A](tpe)(tpe)

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
  final protected type ExistentialExpr = Existential[Id, Expr]
  protected object ExistentialExpr {

    def apply[A: Type](expr: Expr[A]): ExistentialExpr = Existential[Id, Expr, A](expr)(Type[A])

    def withoutType[A](expr: Expr[A]): ExistentialExpr = apply(expr)(Expr.typeOf(expr))

    def prettyPrint(existentialExpr: ExistentialExpr): String = Expr.prettyPrint(existentialExpr.value)

    def use[Out](e: ExistentialExpr)(thunk: Type[e.Underlying] => Expr[e.Underlying] => Out): Out =
      thunk(e.Underlying)(e.value)
  }
}
