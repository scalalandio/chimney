package io.scalaland.chimney.internal.compiletime

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Exprs { this: Definitions =>

  /** Platform-specific expression representation (c.universe.Expr[A] in 2, quotes.Expr[A] in 3 */
  protected type Expr[A]

  val Expr: ExprModule
  trait ExprModule { this: Expr.type =>
    val Nothing: Expr[Nothing]
    val Unit: Expr[Unit]
    def Array[A: Type](args: Expr[A]*): Expr[Array[A]]

    val Option: OptionModule
    trait OptionModule { this: Option.type =>
      def apply[A: Type](a: Expr[A]): Expr[Option[A]]
      def empty[A: Type]: Expr[Option[A]]
      def wrap[A: Type]: Expr[A => Option[A]]
      val None: Expr[scala.None.type]
      def map[A: Type, B: Type](opt: Expr[Option[A]])(f: Expr[A => B]): Expr[Option[B]]
      def fold[A: Type, B: Type](opt: Expr[Option[A]])(none: Expr[B])(f: Expr[A => B]): Expr[B]
    }

    val Either: EitherModule
    trait EitherModule { this: Either.type =>
      def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]]
      def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]]
    }

    def asInstanceOf[T: Type, U: Type](expr: Expr[T]): Expr[U]

    def prettyPrint[T](expr: Expr[T]): String

    def typeOf[T](expr: Expr[T]): Type[T]
  }

  implicit class ExprOps[T: Type](private val expr: Expr[T]) {
    def asInstanceOfExpr[U: Type]: Expr[U] = Expr.asInstanceOf[T, U](expr)
    def unsafeAs[U]: Expr[U] = expr.asInstanceOf[Expr[U]]
  }

  type ComputedExpr = { type Underlying }
  object ComputedExpr {

    def apply[T](expr: Expr[T]): ComputedExpr { type Underlying = T } =
      expr.asInstanceOf[ComputedExpr { type Underlying = T }]

    def prettyPrint(computedExpr: ComputedExpr): String = Expr.prettyPrint(computedExpr.Expr)

    def use[Out](expr: ComputedExpr)(thunk: (Type[expr.Underlying], Expr[expr.Underlying]) => Out): Out = {
      val e = expr.asInstanceOf[Expr[expr.Underlying]]
      thunk(Expr.typeOf(e).asInstanceOf[Type[expr.Underlying]], e)
    }
  }

  implicit class ComputedExprOps(val ce: ComputedExpr) {
    def Expr: Expr[ce.Underlying] = ce.asInstanceOf[Expr[ce.Underlying]]
  }
}
