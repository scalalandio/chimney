package io.scalaland.chimney.internal.compiletime

import scala.quoted
import scala.reflect.ClassTag

private[compiletime] trait ExprsPlatform extends Exprs { this: DefinitionsPlatform =>

  import quotes.*
  import quotes.reflect.*

  final override type Expr[A] = quoted.Expr[A]

  object Expr extends ExprModule {
    val Unit: Expr[Unit] = '{ () }
    def Array[A: Type](args: Expr[A]*): Expr[Array[A]] =
      '{ scala.Array.apply[A](${ quoted.Varargs(args.toSeq) }*)(${ quoted.Expr.summon[ClassTag[A]].get }) }

    object Option extends OptionModule {
      def apply[A: Type](a: Expr[A]): Expr[Option[A]] = '{ scala.Option(${ a }) }
      def empty[A: Type]: Expr[Option[A]] = '{ scala.Option.empty[A] }
      def apply[A: Type]: Expr[A => Option[A]] = '{ scala.Option.apply[A](_) }
      val None: Expr[scala.None.type] = '{ scala.None }
    }

    object Either extends EitherModule {
      def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] = '{ scala.Left[L, R](${ value }) }
      def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] = '{ scala.Right[L, R](${ value }) }
    }

    def asInstanceOf[T: Type, U: Type](expr: Expr[T]): Expr[U] = '{ ${ expr }.asInstanceOf[U] }

    def prettyPrint[T: Type](expr: Expr[T]): String = expr.asTerm.show(using Printer.TreeAnsiCode)
  }
}
