package io.scalaland.chimney.internal.compiletime

import scala.quoted
import scala.reflect.ClassTag

private[compiletime] trait ExprsPlatform extends Exprs { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  final override protected type Expr[A] = quoted.Expr[A]
  protected object Expr extends ExprModule {
    val Nothing: Expr[Nothing] = '{ ??? }
    val Unit: Expr[Unit] = '{ () }

    object Function2 extends Function2Module {
      def tupled[A: Type, B: Type, C: Type](fn2: Expr[(A, B) => C]): Expr[((A, B)) => C] = '{ ${ fn2 }.tupled }
    }

    def Array[A: Type](args: Expr[A]*): Expr[Array[A]] =
      '{ scala.Array.apply[A](${ quoted.Varargs(args.toSeq) }*)(${ quoted.Expr.summon[ClassTag[A]].get }) }

    object Option extends OptionModule {
      def apply[A: Type](a: Expr[A]): Expr[Option[A]] = '{ scala.Option(${ a }) }
      def empty[A: Type]: Expr[Option[A]] = '{ scala.Option.empty[A] }
      val None: Expr[scala.None.type] = '{ scala.None }
      def map[A: Type, B: Type](opt: Expr[Option[A]])(f: Expr[A => B]): Expr[Option[B]] = '{ ${ opt }.map(${ f }) }
      def fold[A: Type, B: Type](opt: Expr[Option[A]])(onNone: Expr[B])(onSome: Expr[A => B]): Expr[B] =
        '{ ${ opt }.fold(${ onNone })(${ onSome }) }
      def getOrElse[A: Type](opt: Expr[Option[A]])(orElse: Expr[A]): Expr[A] =
        '{ ${ opt }.getOrElse(${ orElse }) }
    }

    object Either extends EitherModule {
      def fold[L: Type, R: Type, A: Type](either: Expr[Either[L, R]])(left: Expr[L => A])(
          right: Expr[R => A]
      ): Expr[A] =
        '{ ${ either }.fold[A](${ left }, ${ right }) }

      object Left extends LeftModule {
        def apply[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] = '{ scala.Left[L, R](${ value }) }

        def value[L: Type, R: Type](left: Expr[Left[L, R]]): Expr[L] = '{ ${ left }.value }
      }
      object Right extends RightModule {
        def apply[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] = '{ scala.Right[L, R](${ value }) }

        def value[L: Type, R: Type](right: Expr[Right[L, R]]): Expr[R] = '{ ${ right }.value }
      }
    }

    object Map extends MapModule {
      def iterator[K: Type, V: Type](map: Expr[Map[K, V]]): Expr[Iterator[(K, V)]] = '{ ${ map }.iterator }
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
