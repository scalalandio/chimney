package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ExprsPlatform extends Exprs { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  final override type Expr[A] = c.Expr[A]

  object Expr extends ExprModule {
    val Unit: Expr[Unit] = c.Expr(q"()")
    def Array[A: Type](args: Expr[A]*): Expr[Array[A]] = c.Expr(q"_root_.scala.Array[${Type[A]}](..${args})")
    object Option extends OptionModule {
      def apply[A: Type](a: Expr[A]): Expr[Option[A]] = c.Expr(q"_root_.scala.Option[${Type[A]}]($a)")
      def empty[A: Type]: Expr[Option[A]] = c.Expr(q"_root_.scala.Option.empty[${Type[A]}]")
      def apply[A: Type]: Expr[A => Option[A]] = c.Expr(q"_root_.scala.Option.apply[${Type[A]}](_)")
      val None: Expr[scala.None.type] = c.Expr(q"_root_.scala.None")
    }

    object Either extends EitherModule {
      def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] =
        c.Expr(q"new _root_.scala.util.Left[${Type[L]}, ${Type[R]}]($value)")
      def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] =
        c.Expr(q"new _root_.scala.util.Right[${Type[L]}, ${Type[R]}]($value)")
    }

    def asInstanceOf[T: Type, U: Type](expr: Expr[T]): Expr[U] = c.Expr(q"${expr}.asInstanceOf[${Type[U]}]")
  }

//  protected object exprImpl extends ExprDefinitionsImpl {
//
//    override def PartialResultValue[T: Type](value: Expr[T]): Expr[partial.Result.Value[T]] =
//      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.Value[${Type[T]}]($value)")
//
//    override def PartialResultErrorsMerge(
//        errors1: Expr[partial.Result.Errors],
//        errors2: Expr[partial.Result.Errors]
//    ): Expr[partial.Result.Errors] =
//      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.Errors.merge($errors1, $errors2)")
//
//    override def PartialResultErrorsMergeResultNullable[T: Type](
//        errorsNullable: Expr[partial.Result.Errors],
//        result: Expr[partial.Result[T]]
//    ): Expr[partial.Result.Errors] =
//      c.Expr(
//        q"_root_.io.scalaland.chimney.partial.Result.Errors.__mergeResultNullable[${Type[T]}]($errorsNullable, $result)"
//      )
//    override def PartialResultEmpty[T: Type]: Expr[partial.Result[T]] =
//      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.fromEmpty[${Type[T]}]")
//    override def PartialResultFunction[S: Type, T: Type](f: Expr[S => T]): Expr[S => partial.Result[T]] =
//      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.fromFunction[${Type[S]}, ${Type[T]}]($f)")
//    override def PartialResultTraverse[M: Type, A: Type, B: Type](
//        it: Expr[Iterator[A]],
//        f: Expr[A => partial.Result[B]],
//        failFast: Expr[Boolean]
//    ): Expr[partial.Result[M]] =
//      c.Expr(
//        q"_root_.io.scalaland.chimney.partial.Result.traverse[${Type[M]}, ${Type[A]}, ${Type[B]}]($it, $f, $failFast)"
//      )
//    override def PartialResultSequence[M: Type, A: Type](
//        it: Expr[Iterator[partial.Result[A]]],
//        failFast: Expr[Boolean]
//    ): Expr[partial.Result[M]] =
//      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.sequence[${Type[M]}, ${Type[A]}]($it, $failFast)")
//    override def PartialResultMap2[A: Type, B: Type, C: Type](
//        fa: Expr[partial.Result[A]],
//        fb: Expr[partial.Result[B]],
//        f: Expr[(A, B) => C],
//        failFast: Expr[Boolean]
//    ): Expr[partial.Result[C]] =
//      c.Expr(
//        q"_root_.io.scalaland.chimney.partial.Result.map2[${Type[A]}, ${Type[B]}, ${Type[C]}]($fa, $fb, $f, $failFast)"
//      )
//    override def PartialResultProduct[A: Type, B: Type](
//        fa: Expr[partial.Result[A]],
//        fb: Expr[partial.Result[B]],
//        failFast: Expr[Boolean]
//    ): Expr[partial.Result[(A, B)]] =
//      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.product[${Type[A]}, ${Type[B]}]($fa, $fb, $failFast)")
//
//    override def PathElementAccessor(targetName: Expr[String]): Expr[partial.PathElement.Accessor] =
//      c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.Accessor($targetName)")
//    override def PathElementIndex(index: Expr[Int]): Expr[partial.PathElement.Index] =
//      c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.Index($index)")
//    override def PathElementMapKey(key: Expr[Any]): Expr[partial.PathElement.MapKey] =
//      c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.MapKey($key)")
//    override def PathElementMapValue(key: Expr[Any]): Expr[partial.PathElement.MapValue] =
//      c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.MapValue($key)")
//  }
}
