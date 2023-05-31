package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ExprsPlatform extends Exprs { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  final override type Expr[A] = c.Expr[A]

  object Expr extends ExprModule {
    val Nothing: Expr[Nothing] = c.Expr(q"???")
    val Unit: Expr[Unit] = c.Expr(q"()")
    def Array[A: Type](args: Expr[A]*): Expr[Array[A]] = c.Expr(q"_root_.scala.Array[${Type[A]}](..${args})")
    object Option extends OptionModule {
      def apply[A: Type](a: Expr[A]): Expr[Option[A]] = c.Expr(q"_root_.scala.Option[${Type[A]}]($a)")
      def empty[A: Type]: Expr[Option[A]] = c.Expr(q"_root_.scala.Option.empty[${Type[A]}]")
      def wrap[A: Type]: Expr[A => Option[A]] = c.Expr(q"_root_.scala.Option[${Type[A]}](_)")
      val None: Expr[scala.None.type] = c.Expr(q"_root_.scala.None")
      def map[A: Type, B: Type](opt: Expr[Option[A]])(f: Expr[A => B]): Expr[Option[B]] =
        c.Expr(q"$opt.map[${Type[B]}]($f)")
      def fold[A: Type, B: Type](opt: Expr[Option[A]])(onNone: Expr[B])(onSome: Expr[A => B]): Expr[B] =
        c.Expr(q"$opt.fold[${Type[B]}]($onNone)($onSome)")
    }

    object Either extends EitherModule {
      def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] =
        c.Expr(q"new _root_.scala.util.Left[${Type[L]}, ${Type[R]}]($value)")
      def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] =
        c.Expr(q"new _root_.scala.util.Right[${Type[L]}, ${Type[R]}]($value)")
    }

    def asInstanceOf[T: Type, U: Type](expr: Expr[T]): Expr[U] = c.Expr(q"${expr}.asInstanceOf[${Type[U]}]")

    def prettyPrint[T](expr: Expr[T]): String =
      expr
        .toString()
        .replaceAll("\\$\\d+", "")
        .replace("$u002E", ".")

    def typeOf[T](expr: Expr[T]): Type[T] = typeUtils.fromUntyped(expr.actualType)
  }
}
