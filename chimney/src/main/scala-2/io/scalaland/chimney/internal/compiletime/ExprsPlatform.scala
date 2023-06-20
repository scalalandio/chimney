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
    val Null: Expr[Null] = asExpr[Null](q"null")
    val Unit: Expr[Unit] = asExpr[Unit](q"()")

    def Int(value: Int): Expr[Int] = asExpr[Int](q"$value")
    def String(value: String): Expr[String] = asExpr[String](q"$value")

    object Function1 extends Function1Module {
      def apply[A: Type, B: Type](fn: Expr[A => B])(a: Expr[A]): Expr[B] = asExpr(q"$fn.apply($a)")
    }

    object Function2 extends Function2Module {
      def tupled[A: Type, B: Type, C: Type](fn2: Expr[(A, B) => C]): Expr[((A, B)) => C] = asExpr(q"($fn2).tupled")
    }

    object Array extends ArrayModule {
      def apply[A: Type](args: Expr[A]*): Expr[Array[A]] =
        asExpr[Array[A]](q"_root_.scala.Array[${Type[A]}](..${args})")

      def map[A: Type, B: Type](array: Expr[Array[A]])(fExpr: Expr[A => B]): Expr[Array[B]] =
        asExpr(q"$array.map[${Type[B]}]($fExpr)")

      // TODO: write it in similar way to MacroUtils.convertCollection
      def to[A: Type, C: Type](array: Expr[Array[A]])(
          factoryExpr: Expr[scala.collection.compat.Factory[A, C]]
      ): Expr[C] =
        asExpr(q"$array.to($factoryExpr)")

      def iterator[A: Type](array: Expr[Array[A]]): Expr[Iterator[A]] = asExpr(q"$array.iterator")
    }

    object Option extends OptionModule {
      def apply[A: Type](a: Expr[A]): Expr[Option[A]] = asExpr[Option[A]](q"_root_.scala.Option[${Type[A]}]($a)")
      def empty[A: Type]: Expr[Option[A]] = asExpr[Option[A]](q"_root_.scala.Option.empty[${Type[A]}]")
      val None: Expr[scala.None.type] = asExpr[scala.None.type](q"_root_.scala.None")
      def map[A: Type, B: Type](opt: Expr[Option[A]])(f: Expr[A => B]): Expr[Option[B]] =
        asExpr[Option[B]](q"$opt.map[${Type[B]}]($f)")
      def fold[A: Type, B: Type](opt: Expr[Option[A]])(onNone: Expr[B])(onSome: Expr[A => B]): Expr[B] =
        asExpr[B](q"$opt.fold[${Type[B]}]($onNone)($onSome)")
      def getOrElse[A: Type](opt: Expr[Option[A]])(orElse: Expr[A]): Expr[A] =
        asExpr[A](q"$opt.getOrElse[${Type[A]}]($orElse)")
    }

    object Either extends EitherModule {
      def fold[L: Type, R: Type, A: Type](either: Expr[Either[L, R]])(left: Expr[L => A])(
          right: Expr[R => A]
      ): Expr[A] =
        asExpr(q"""$either.fold[${Type[A]}]($left, $right)""")

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

    object Iterable extends IterableModule {
      def map[A: Type, B: Type](iterable: Expr[Iterable[A]])(fExpr: Expr[A => B]): Expr[Iterable[B]] =
        asExpr(q"$iterable.map[${Type[B]}]($fExpr)")

      // TODO: write it in similar way to MacroUtils.convertCollection
      def to[A: Type, C: Type](iterable: Expr[Iterable[A]])(
          factoryExpr: Expr[scala.collection.compat.Factory[A, C]]
      ): Expr[C] = asExpr(q"$iterable.to($factoryExpr)")

      def iterator[A: Type](iterable: Expr[Iterable[A]]): Expr[Iterator[A]] = asExpr(q"$iterable.iterator")
    }

    object Map extends MapModule {
      def iterator[K: Type, V: Type](map: Expr[Map[K, V]]): Expr[Iterator[(K, V)]] = asExpr(q"$map.iterator")
    }

    object Iterator extends IteratorModule {
      def map[A: Type, B: Type](iterator: Expr[Iterator[A]])(fExpr: Expr[A => B]): Expr[Iterator[B]] =
        asExpr(q"$iterator.map[${Type[B]}]($fExpr)")

      // TODO: write it in similar way to MacroUtils.convertCollection
      def to[A: Type, C: Type](iterator: Expr[Iterator[A]])(
          factoryExpr: Expr[scala.collection.compat.Factory[A, C]]
      ): Expr[C] = asExpr(q"$iterator.to($factoryExpr)")

      def zipWithIndex[A: Type](it: Expr[Iterator[A]]): Expr[Iterator[(A, Int)]] = asExpr(q"$it.zipWithIndex")
    }

    def ifElse[A: Type](cond: Expr[Boolean])(ifBranch: Expr[A])(elseBranch: Expr[A]): Expr[A] =
      asExpr(q"if ($cond) { $ifBranch } else { $elseBranch }")

    def block[A: Type](statements: List[Expr[Unit]], expr: Expr[A]): Expr[A] = asExpr[A](q"..$statements; $expr")

    def summonImplicit[A: Type]: Option[Expr[A]] = scala.util
      .Try(c.inferImplicitValue(Type[A], silent = true, withMacrosDisabled = false))
      .toOption
      .filterNot(_ == EmptyTree)
      .map(asExpr[A](_))

    def eq[A: Type, B: Type](a: Expr[A], b: Expr[B]): Expr[Boolean] = asExpr(q"$a == $b")

    def asInstanceOf[A: Type, B: Type](expr: Expr[A]): Expr[B] = asExpr[B](q"${expr}.asInstanceOf[${Type[B]}]")

    def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = {
      val wideningChecked = expr.widenExpr[B]
      if (Type[A] =:= Type[B]) wideningChecked
      else asExpr[B](q"($expr : ${Type[B]})")
    }

    def suppressUnused[A: Type](expr: Expr[A]): Expr[Unit] = asExpr(q"val _ = $expr")

    def prettyPrint[A](expr: Expr[A]): String =
      expr
        .toString()
        .replaceAll("\\$\\d+", "")
        .replace("$u002E", ".")

    def typeOf[A](expr: Expr[A]): Type[A] = Type.platformSpecific.fromUntyped(expr.staticType.finalResultType)
  }
}
