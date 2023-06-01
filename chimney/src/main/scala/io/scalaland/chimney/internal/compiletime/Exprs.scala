package io.scalaland.chimney.internal.compiletime

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Exprs { this: Definitions =>

  import TypeImplicits.*

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

    object Function1 {
      def lift[A: Type, B: Type](f: Expr[A] => Expr[B]): Expr[A => B] =
        ExprPromise
          .promise[A](ExprPromise.NameGenerationStrategy.FromType)
          .map[Expr[B]](f)
          .fulfilAsLambda[B, Expr[A => B]](e => e)
    }

    object Function2 {
      def lift[A: Type, B: Type, C: Type](f: (Expr[A], Expr[B]) => Expr[C]): Expr[(A, B) => C] =
        ExprPromise
          .promise[A](ExprPromise.NameGenerationStrategy.FromType)
          .fulfilAsLambda2[B, Expr[B], C, Expr[(A, B) => C]](
            ExprPromise.promise[B](ExprPromise.NameGenerationStrategy.FromType)
          )(f)(e => e)
    }

    def summonImplicit[A: Type]: Option[Expr[A]]

    def asInstanceOf[A: Type, B: Type](expr: Expr[A]): Expr[B]

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B]

    def prettyPrint[A](expr: Expr[A]): String

    def typeOf[A](expr: Expr[A]): Type[A]
  }

  implicit final class ExprOps[A: Type](private val expr: Expr[A]) {
    def asInstanceOfExpr[B: Type]: Expr[B] = Expr.asInstanceOf[A, B](expr)
    def upcastExpr[B: Type]: Expr[B] = Expr.upcast[A, B](expr)
  }

  type ComputedExpr = { type Underlying }
  object ComputedExpr {

    def apply[A](expr: Expr[A]): ComputedExpr { type Underlying = A } =
      expr.asInstanceOf[ComputedExpr { type Underlying = A }]

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
