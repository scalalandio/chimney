package io.scalaland.chimney.internal.compiletime

import scala.quoted
import scala.reflect.ClassTag

private[compiletime] trait ExprsPlatform extends Exprs { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  final override protected type Expr[A] = quoted.Expr[A]
  protected object Expr extends ExprModule {
    val Nothing: Expr[Nothing] = '{ ??? }
    val Null: Expr[Null] = '{ null }
    val Unit: Expr[Unit] = '{ () }

    def Int(value: Int): Expr[Int] = scala.quoted.Expr(value)
    def String(value: String): Expr[String] = scala.quoted.Expr(value)

    object Function1 extends Function1Module {
      def apply[A: Type, B: Type](fn: Expr[A => B])(a: Expr[A]): Expr[B] = '{ ${ fn }.apply(${ a }) }
    }

    object Function2 extends Function2Module {
      def tupled[A: Type, B: Type, C: Type](fn2: Expr[(A, B) => C]): Expr[((A, B)) => C] = '{ ${ fn2 }.tupled }
    }

    object Array extends ArrayModule {
      def apply[A: Type](args: Expr[A]*): Expr[Array[A]] =
        '{ scala.Array.apply[A](${ quoted.Varargs(args.toSeq) }*)(${ summonImplicit[ClassTag[A]].get }) }

      def map[A: Type, B: Type](array: Expr[Array[A]])(fExpr: Expr[A => B]): Expr[Array[B]] =
        '{ ${ array }.map(${ fExpr })(${ summonImplicit[ClassTag[B]].get }) }

      def to[A: Type, C: Type](array: Expr[Array[A]])(
          factoryExpr: Expr[scala.collection.compat.Factory[A, C]]
      ): Expr[C] =
        '{ ${ array }.to(${ factoryExpr }) }

      def iterator[A: Type](array: Expr[Array[A]]): Expr[Iterator[A]] = '{ ${ array }.iterator }
    }

    object Option extends OptionModule {
      def apply[A: Type](a: Expr[A]): Expr[Option[A]] = '{ scala.Option(${ a }) }
      def empty[A: Type]: Expr[Option[A]] = '{ scala.Option.empty[A] }
      val None: Expr[scala.None.type] = '{ scala.None }
      def map[A: Type, B: Type](opt: Expr[Option[A]])(f: Expr[A => B]): Expr[Option[B]] = '{ ${ opt }.map(${ f }) }
      def fold[A: Type, B: Type](opt: Expr[Option[A]])(onNone: Expr[B])(onSome: Expr[A => B]): Expr[B] =
        '{ ${ opt }.fold(${ onNone })(${ onSome }) }
      def getOrElse[A: Type](opt: Expr[Option[A]])(orElse: Expr[A]): Expr[A] =
        '{ ${ opt }.getOrElse(${ orElse }) }
      def get[A: Type](opt: Expr[Option[A]]): Expr[A] =
        '{ ${ opt }.get }
      def isDefined[A: Type](opt: Expr[Option[A]]): Expr[Boolean] =
        '{ ${ opt }.isDefined }
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

    object Iterable extends IterableModule {
      def map[A: Type, B: Type](iterable: Expr[Iterable[A]])(fExpr: Expr[A => B]): Expr[Iterable[B]] =
        '{ ${ iterable }.map(${ fExpr }) }

      def to[A: Type, C: Type](iterable: Expr[Iterable[A]])(
          factoryExpr: Expr[scala.collection.compat.Factory[A, C]]
      ): Expr[C] =
        '{ ${ iterable }.to(${ factoryExpr }) }

      def iterator[A: Type](iterable: Expr[Iterable[A]]): Expr[Iterator[A]] = '{ ${ iterable }.iterator }
    }

    object Map extends MapModule {
      def iterator[K: Type, V: Type](map: Expr[Map[K, V]]): Expr[Iterator[(K, V)]] = '{ ${ map }.iterator }
    }

    object Iterator extends IteratorModule {
      def map[A: Type, B: Type](iterator: Expr[Iterator[A]])(fExpr: Expr[A => B]): Expr[Iterator[B]] =
        '{ ${ iterator }.map(${ fExpr }) }

      def to[A: Type, C: Type](iterator: Expr[Iterator[A]])(
          factoryExpr: Expr[scala.collection.compat.Factory[A, C]]
      ): Expr[C] =
        '{ ${ iterator }.to(${ factoryExpr }) }

      def zipWithIndex[A: Type](it: Expr[Iterator[A]]): Expr[Iterator[(A, Int)]] = '{ ${ it }.zipWithIndex }
    }

    def ifElse[A: Type](cond: Expr[Boolean])(ifBranch: Expr[A])(elseBranch: Expr[A]): Expr[A] =
      '{
        if ${ cond } then ${ ifBranch }
        else ${ elseBranch }
      }

    def block[A: Type](statements: List[Expr[Unit]], expr: Expr[A]): Expr[A] =
      Block(statements.map(_.asTerm), expr.asTerm).asExprOf[A]

    def summonImplicit[A: Type]: Option[Expr[A]] = scala.quoted.Expr.summon[A]

    def eq[A: Type, B: Type](a: Expr[A], b: Expr[B]): Expr[Boolean] = '{ ${ a } == ${ b } }

    def asInstanceOf[A: Type, B: Type](expr: Expr[A]): Expr[B] = '{ ${ expr }.asInstanceOf[B] }

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      val wideningChecked = expr.widenExpr[B]
      if Type[A] =:= Type[B] then wideningChecked
      else expr.asExprOf[B] // TODO: ask Janek if this upcast in code
    }

    def suppressUnused[A: Type](expr: Expr[A]): Expr[Unit] = '{ val _ = ${ expr } }

    def prettyPrint[A](expr: Expr[A]): String = expr.asTerm.show(using Printer.TreeAnsiCode)

    def typeOf[A](expr: Expr[A]): Type[A] = expr.asTerm.tpe.asType.asInstanceOf[Type[A]]
  }
}
