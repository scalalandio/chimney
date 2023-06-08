package io.scalaland.chimney.internal.compiletime

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Exprs { this: Definitions =>

  /** Platform-specific expression representation (c.universe.Expr[A] in 2, quotes.Expr[A] in 3 */
  protected type Expr[A]
  protected val Expr: ExprModule
  protected trait ExprModule { this: Expr.type =>

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
      def getOrElse[A: Type](opt: Expr[Option[A]])(orElse: Expr[A]): Expr[A]
    }

    val Either: EitherModule
    trait EitherModule { this: Either.type =>
      val Left: LeftModule
      trait LeftModule { this: Left.type =>
        def apply[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]]

        def value[L: Type, R: Type](left: Expr[Left[L, R]]): Expr[L]
      }
      val Right: RightModule
      trait RightModule { this: Right.type =>
        def apply[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]]

        def value[L: Type, R: Type](right: Expr[Right[L, R]]): Expr[R]
      }
    }

    object Function1 {
      def instance[A: Type, B: Type](f: Expr[A] => Expr[B]): Expr[A => B] =
        ExprPromise
          .promise[A](ExprPromise.NameGenerationStrategy.FromType)
          .map[Expr[B]](f)
          .fulfilAsLambda[B, Expr[A => B]](e => e)
    }

    object Function2 {
      def instance[A: Type, B: Type, C: Type](f: (Expr[A], Expr[B]) => Expr[C]): Expr[(A, B) => C] =
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
  implicit final protected class ExprOps[A: Type](private val expr: Expr[A]) {

    def asInstanceOfExpr[B: Type]: Expr[B] = Expr.asInstanceOf[A, B](expr)
    def upcastExpr[B: Type]: Expr[B] = Expr.upcast[A, B](expr)
  }

  implicit final protected class OptionExprOps[A: Type](private val optionExpr: Expr[Option[A]]) {

    def map[B: Type](fExpr: Expr[A => B]): Expr[Option[B]] = Expr.Option.map(optionExpr)(fExpr)
    def fold[B: Type](noneExpr: Expr[B])(fExpr: Expr[A => B]): Expr[B] =
      Expr.Option.fold(optionExpr)(noneExpr)(fExpr)
    def getOrElse(noneExpr: Expr[A]): Expr[A] = Expr.Option.getOrElse(optionExpr)(noneExpr)
  }

  implicit final protected class LeftExprOps[L: Type, R: Type](private val leftExpr: Expr[Left[L, R]]) {

    def value: Expr[L] = Expr.Either.Left.value(leftExpr)
  }

  implicit final protected class RightExprOps[L: Type, R: Type](private val rightExpr: Expr[Right[L, R]]) {

    def value: Expr[R] = Expr.Either.Right.value(rightExpr)
  }
}
