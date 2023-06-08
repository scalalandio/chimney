package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ExprsPlatform extends Exprs { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}
  import TypeImplicits.*

  final override protected type Expr[A] = c.Expr[A]
  protected object Expr extends ExprModule {

    object platformSpecific {

      // Ensures that whe we do:
      //   def sth[A: Type] = Expr[A](q"...")
      //   sth[OurType]
      // we would get Expr[OutType](...) rather than Expr[A] where A would fail at macro expansion with error:
      //    Macro expansion contains free type variable B defined by upcast in ExprsPlatform.scala:41:25.
      //    Have you forgotten to use c.WeakTypeTag annotation for this type parameter?
      //    If you have troubles tracking free type variables, consider using -Xlog-free-types
      def asExpr[A: Type](tree: Tree): Expr[A] = c.Expr(tree)(Type.platformSpecific.toWeakConversion.weakFromType[A])
    }

    import platformSpecific.asExpr

    val Nothing: Expr[Nothing] = asExpr[Nothing](q"???")
    val Unit: Expr[Unit] = asExpr[Unit](q"()")
    def Array[A: Type](args: Expr[A]*): Expr[Array[A]] = asExpr[Array[A]](q"_root_.scala.Array[${Type[A]}](..${args})")
    object Option extends OptionModule {
      def apply[A: Type](a: Expr[A]): Expr[Option[A]] = asExpr[Option[A]](q"_root_.scala.Option[${Type[A]}]($a)")
      def empty[A: Type]: Expr[Option[A]] = asExpr[Option[A]](q"_root_.scala.Option.empty[${Type[A]}]")
      def wrap[A: Type]: Expr[A => Option[A]] = asExpr[A => Option[A]](q"_root_.scala.Option[${Type[A]}](_)")
      val None: Expr[scala.None.type] = asExpr[scala.None.type](q"_root_.scala.None")
      def map[A: Type, B: Type](opt: Expr[Option[A]])(f: Expr[A => B]): Expr[Option[B]] =
        asExpr[Option[B]](q"$opt.map[${Type[B]}]($f)")
      def fold[A: Type, B: Type](opt: Expr[Option[A]])(onNone: Expr[B])(onSome: Expr[A => B]): Expr[B] =
        asExpr[B](q"$opt.fold[${Type[B]}]($onNone)($onSome)")
      def getOrElse[A: Type](opt: Expr[Option[A]])(orElse: Expr[A]): Expr[A] =
        asExpr[A](q"$opt.getOrElse[${Type[A]}]($orElse)")
    }

    object Either extends EitherModule {
      object Left extends LeftModule {
        def apply[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] =
          asExpr[Left[L, R]](q"new _root_.scala.util.Left[${Type[L]}, ${Type[R]}]($value)")

        def value[L: Type, R: Type](left: Expr[Left[L, R]]): Expr[L] = asExpr[L](q"$left.value")
      }
      object Right extends RightModule {
        def apply[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] =
          asExpr[Right[L, R]](q"new _root_.scala.util.Right[${Type[L]}, ${Type[R]}]($value)")

        def value[L: Type, R: Type](right: Expr[Right[L, R]]): Expr[R] = asExpr[R](q"$right.value")
      }
    }

    def summonImplicit[A: Type]: Option[Expr[A]] = scala.util
      .Try(c.inferImplicitValue(Type[A], silent = true, withMacrosDisabled = false))
      .toOption
      .filterNot(_ == EmptyTree)
      .map(asExpr[A](_))

    def asInstanceOf[A: Type, B: Type](expr: Expr[A]): Expr[B] = asExpr[B](q"${expr}.asInstanceOf[${Type[B]}]")

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      Predef.assert(
        Type[A] <:< Type[B],
        s"Upcasting can only be done to type proved to be super type! Failed ${Type.prettyPrint[A]} <:< ${Type.prettyPrint[B]} check"
      )
      if (Type[A] =:= Type[B]) expr.asInstanceOf[Expr[B]]
      else asExpr[B](q"($expr : ${Type[B]})")
    }

    def prettyPrint[A](expr: Expr[A]): String =
      expr
        .toString()
        .replaceAll("\\$\\d+", "")
        .replace("$u002E", ".")

    def typeOf[A](expr: Expr[A]): Type[A] = Type.platformSpecific.fromUntyped(expr.staticType.finalResultType)
  }
}
