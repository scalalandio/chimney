package io.scalaland.chimney.internal.compiletime

import scala.quoted
import scala.reflect.ClassTag

private[compiletime] trait ExprsPlatform extends Exprs { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  final override type Expr[A] = quoted.Expr[A]

  object Expr extends ExprModule {
    val Nothing: Expr[Nothing] = '{ ??? }
    val Unit: Expr[Unit] = '{ () }
    def Array[A: Type](args: Expr[A]*): Expr[Array[A]] =
      '{ scala.Array.apply[A](${ quoted.Varargs(args.toSeq) }*)(${ quoted.Expr.summon[ClassTag[A]].get }) }

    object Option extends OptionModule {
      def apply[A: Type](a: Expr[A]): Expr[Option[A]] = '{ scala.Option(${ a }) }
      def empty[A: Type]: Expr[Option[A]] = '{ scala.Option.empty[A] }
      def wrap[A: Type]: Expr[A => Option[A]] = '{ scala.Option[A](_) }
      val None: Expr[scala.None.type] = '{ scala.None }
      def map[A: Type, B: Type](opt: Expr[Option[A]])(f: Expr[A => B]): Expr[Option[B]] = '{ ${ opt }.map(${ f }) }
      def fold[A: Type, B: Type](opt: Expr[Option[A]])(onNone: Expr[B])(onSome: Expr[A => B]): Expr[B] =
        '{ ${ opt }.fold(${ onNone })(${ onSome }) }
    }

    object Either extends EitherModule {
      def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] = '{ scala.Left[L, R](${ value }) }
      def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] = '{ scala.Right[L, R](${ value }) }
    }

    def summonImplicit[A: Type]: Option[Expr[A]] = scala.quoted.Expr.summon[A]

    def asInstanceOf[A: Type, B: Type](expr: Expr[A]): Expr[B] = '{ ${ expr }.asInstanceOf[B] }

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      Predef.assert(
        Type[A] <:< Type[B],
        s"Upcasting can only be done to type proved to be super type! Failed ${Type.prettyPrint[A]} <:< ${Type.prettyPrint[B]} check"
      )
      if Type[A] =:= Type[B] then expr.asInstanceOf[Expr[B]] else expr.asExprOf[B]
    }

    def prettyPrint[A](expr: Expr[A]): String = expr.asTerm.show(using Printer.TreeAnsiCode)

    def typeOf[A](expr: Expr[A]): Type[A] = expr.asTerm.tpe.asType.asInstanceOf[Type[A]]
  }
}
