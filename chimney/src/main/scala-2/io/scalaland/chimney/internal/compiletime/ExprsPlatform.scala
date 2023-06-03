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
      def getOrElse[A: Type](opt: Expr[Option[A]])(orElse: Expr[A]): Expr[A] =
        c.Expr(q"$opt.getOrElse[${Type[A]}]($orElse)")
    }

    object Either extends EitherModule {
      def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] =
        c.Expr(q"new _root_.scala.util.Left[${Type[L]}, ${Type[R]}]($value)")
      def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] =
        c.Expr(q"new _root_.scala.util.Right[${Type[L]}, ${Type[R]}]($value)")
    }

    def summonImplicit[A: Type]: Option[Expr[A]] = scala.util
      .Try(c.inferImplicitValue(Type[A], silent = true, withMacrosDisabled = false))
      .toOption
      .filterNot(_ == EmptyTree)
      .map(c.Expr[A](_))

    def asInstanceOf[A: Type, B: Type](expr: Expr[A]): Expr[B] = c.Expr(q"${expr}.asInstanceOf[${Type[B]}]")

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      Predef.assert(
        Type[A] <:< Type[B],
        s"Upcasting can only be done to type proved to be super type! Failed ${Type.prettyPrint[A]} <:< ${Type.prettyPrint[B]} check"
      )
      if (Type[A] =:= Type[B]) expr.asInstanceOf[Expr[B]] else c.Expr[B](q"($expr : ${Type[B]})")
    }

    def prettyPrint[A](expr: Expr[A]): String =
      expr
        .toString()
        .replaceAll("\\$\\d+", "")
        .replace("$u002E", ".")

    def typeOf[A](expr: Expr[A]): Type[A] = typeUtils.fromUntyped(expr.staticType)
  }
}
