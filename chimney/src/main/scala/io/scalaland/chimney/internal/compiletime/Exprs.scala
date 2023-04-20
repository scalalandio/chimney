package io.scalaland.chimney.internal.compiletime

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Exprs { this: Definitions =>

  /** Platform-specific expression representation (c.universe.Expr[A] in 2, quotes.Expr[A] in 3 */
  protected type Expr[A]

  val Expr: ExprModule
  trait ExprModule { this: Expr.type =>
    val Unit: Expr[Unit]
    def Array[A: Type](args: Expr[A]*): Expr[Array[A]]

    val Option: OptionModule
    trait OptionModule { this: Option.type =>
      def apply[A: Type](a: Expr[A]): Expr[Option[A]]
      def empty[A: Type]: Expr[Option[A]]
      def apply[A: Type]: Expr[A => Option[A]]
      val None: Expr[scala.None.type]
    }

    val Either: EitherModule
    trait EitherModule { this: Either.type =>
      def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]]
      def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]]
    }

    def asInstanceOf[T: Type, U: Type](expr: Expr[T]): Expr[U]

    def prettyPrint[T: Type](expr: Expr[T]): String
  }

  implicit class ExprOps[T: Type](private val expr: Expr[T]) {
    def asInstanceOfExpr[U: Type]: Expr[U] = Expr.asInstanceOf[T, U](expr)
  }
}
