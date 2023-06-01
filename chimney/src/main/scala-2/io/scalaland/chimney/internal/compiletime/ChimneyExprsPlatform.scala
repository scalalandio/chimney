package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.partial

private[compiletime] trait ChimneyExprsPlatform extends ChimneyExprs { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}
  import TypeImplicits.*

  object ChimneyExpr extends ChimneyExprModule {

    object Transformer extends TransformerModule {

      def callTransform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.Transformer[From, To]],
          src: Expr[From]
      ): Expr[To] = c.Expr(q"$transformer.transform($src)")

      def lift[From: Type, To: Type](
          toExpr: Expr[From] => Expr[To]
      ): Expr[io.scalaland.chimney.Transformer[From, To]] = {
        val srcTermName = ExprPromise.provideFreshName[From](ExprPromise.NameGenerationStrategy.FromType)
        val srcExpr: Expr[From] = c.Expr[From](q"$srcTermName")
        c.Expr[io.scalaland.chimney.Transformer[From, To]](
          q"""new _root_.io.scalaland.chimney.Transformer[${Type[From]}, ${Type[To]}] {
              def transform($srcTermName: ${Type[From]}): ${Type[To]} = {
                ${toExpr(srcExpr)}
              }
            }"""
        )
      }
    }

    object PartialTransformer extends PartialTransformerModule {

      def callTransform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.PartialTransformer[From, To]],
          src: Expr[From],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[To]] = c.Expr(q"$transformer.transform($src, $failFast)")

      def lift[From: Type, To: Type](
          toExpr: (Expr[From], Expr[Boolean]) => Expr[partial.Result[To]]
      ): Expr[io.scalaland.chimney.PartialTransformer[From, To]] = {
        val srcTermName = ExprPromise.provideFreshName[From](ExprPromise.NameGenerationStrategy.FromType)
        val srcExpr: Expr[From] = c.Expr[From](q"$srcTermName")
        val failFastTermName =
          ExprPromise.provideFreshName[Boolean](ExprPromise.NameGenerationStrategy.FromPrefix("failFast"))
        val failFastExpr: Expr[Boolean] = c.Expr[Boolean](q"$failFastTermName")
        c.Expr[io.scalaland.chimney.PartialTransformer[From, To]](
          q"""new _root_.io.scalaland.chimney.PartialTransformer[${Type[From]}, ${Type[To]}] {
                def transform(
                  $srcTermName: ${Type[From]},
                  $failFastTermName: ${Type[Boolean]}
                ): _root_.io.scalaland.chimney.partial.Result[${Type[To]}] = {
                  ${toExpr(srcExpr, failFastExpr)}
                }
              }"""
        )
      }
    }

    object PartialResult extends PartialResultModule {
      def Value[T: Type](value: Expr[T]): Expr[partial.Result.Value[T]] =
        c.Expr(q"_root_.io.scalaland.chimney.partial.Result.Value[${Type[T]}]($value)")

      object Errors extends ErrorsModule {
        def merge(
            errors1: Expr[partial.Result.Errors],
            errors2: Expr[partial.Result.Errors]
        ): Expr[partial.Result.Errors] =
          c.Expr(q"_root_.io.scalaland.chimney.partial.Result.Errors.merge($errors1, $errors2)")

        def mergeResultNullable[T: Type](
            errorsNullable: Expr[partial.Result.Errors],
            result: Expr[partial.Result[T]]
        ): Expr[partial.Result.Errors] =
          c.Expr(
            q"_root_.io.scalaland.chimney.partial.Result.Errors.__mergeResultNullable[${Type[T]}]($errorsNullable, $result)"
          )
      }

      def fromEmpty[T: Type]: Expr[partial.Result[T]] =
        c.Expr(q"_root_.io.scalaland.chimney.partial.Result.fromEmpty[${Type[T]}]")

      def fromFunction[S: Type, T: Type](f: Expr[S => T]): Expr[S => partial.Result[T]] =
        c.Expr(q"_root_.io.scalaland.chimney.partial.Result.fromFunction[${Type[S]}, ${Type[T]}]($f)")

      def traverse[M: Type, A: Type, B: Type](
          it: Expr[Iterator[A]],
          f: Expr[A => partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[M]] =
        c.Expr(
          q"_root_.io.scalaland.chimney.partial.Result.traverse[${Type[M]}, ${Type[A]}, ${Type[B]}]($it, $f, $failFast)"
        )

      def sequence[M: Type, A: Type](
          it: Expr[Iterator[partial.Result[A]]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[M]] =
        c.Expr(q"_root_.io.scalaland.chimney.partial.Result.sequence[${Type[M]}, ${Type[A]}]($it, $failFast)")

      def map[A: Type, B: Type](pr: Expr[partial.Result[A]])(f: Expr[A => B]): Expr[partial.Result[B]] =
        c.Expr(q"$pr.map[${Type[B]}]($f)")

      def map2[A: Type, B: Type, C: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          f: Expr[(A, B) => C],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[C]] =
        c.Expr(
          q"_root_.io.scalaland.chimney.partial.Result.map2[${Type[A]}, ${Type[B]}, ${Type[C]}]($fa, $fb, $f, $failFast)"
        )

      def product[A: Type, B: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[(A, B)]] =
        c.Expr(q"_root_.io.scalaland.chimney.partial.Result.product[${Type[A]}, ${Type[B]}]($fa, $fb, $failFast)")
    }

    object PathElement extends PathElementModule {
      def Accessor(targetName: Expr[String]): Expr[partial.PathElement.Accessor] =
        c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.Accessor($targetName)")
      def Index(index: Expr[Int]): Expr[partial.PathElement.Index] =
        c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.Index($index)")
      def MapKey(key: Expr[Any]): Expr[partial.PathElement.MapKey] =
        c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.MapKey($key)")
      def MapValue(key: Expr[Any]): Expr[partial.PathElement.MapValue] =
        c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.MapValue($key)")
    }

    object RuntimeDataStore extends RuntimeDataStoreModule {

      val empty: Expr[TransformerDefinitionCommons.RuntimeDataStore] =
        c.Expr(q"_root_.io.scalaland.chimney.dsl.TransformerDefinitionCommons.emptyRuntimeDataStore")

      def extractAt(
          runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore],
          index: Int
      ): Expr[Any] = c.Expr(q"$runtimeDataStore($index)")
    }
  }
}
