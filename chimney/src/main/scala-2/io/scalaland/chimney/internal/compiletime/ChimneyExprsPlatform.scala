package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

private[compiletime] trait ChimneyExprsPlatform extends ChimneyExprs { this: ChimneyDefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  object ChimneyExpr extends ChimneyExprModule {

    object Transformer extends TransformerModule {

      def transform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.Transformer[From, To]],
          src: Expr[From]
      ): Expr[To] = c.Expr[To](q"$transformer.transform($src)")

      def instance[From: Type, To: Type](
          toExpr: Expr[From] => Expr[To]
      ): Expr[io.scalaland.chimney.Transformer[From, To]] = {
        val srcTermName =
          ExprPromise.provideFreshName[From](ExprPromise.NameGenerationStrategy.FromType, ExprPromise.UsageHint.None)
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

      def transform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.PartialTransformer[From, To]],
          src: Expr[From],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[To]] = c.Expr[partial.Result[To]](q"$transformer.transform($src, $failFast)")

      def instance[From: Type, To: Type](
          toExpr: (Expr[From], Expr[Boolean]) => Expr[partial.Result[To]]
      ): Expr[io.scalaland.chimney.PartialTransformer[From, To]] = {
        val srcTermName =
          ExprPromise.provideFreshName[From](ExprPromise.NameGenerationStrategy.FromType, ExprPromise.UsageHint.None)
        val srcExpr: Expr[From] = c.Expr[From](q"$srcTermName")
        val failFastTermName =
          ExprPromise.provideFreshName[Boolean](
            ExprPromise.NameGenerationStrategy.FromPrefix("failFast"),
            ExprPromise.UsageHint.None
          )
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
      object Value extends ValueModule {
        def apply[A: Type](value: Expr[A]): Expr[partial.Result.Value[A]] =
          c.Expr[partial.Result.Value[A]](q"_root_.io.scalaland.chimney.partial.Result.Value[${Type[A]}]($value)")

        def value[A: Type](valueExpr: Expr[partial.Result.Value[A]]): Expr[A] =
          c.Expr[A](q"$valueExpr.value")
      }

      object Errors extends ErrorsModule {
        def merge(
            errors1: Expr[partial.Result.Errors],
            errors2: Expr[partial.Result.Errors]
        ): Expr[partial.Result.Errors] =
          c.Expr[partial.Result.Errors](q"_root_.io.scalaland.chimney.partial.Result.Errors.merge($errors1, $errors2)")

        def mergeResultNullable[A: Type](
            errorsNullable: Expr[partial.Result.Errors],
            result: Expr[partial.Result[A]]
        ): Expr[partial.Result.Errors] =
          c.Expr[partial.Result.Errors](
            q"_root_.io.scalaland.chimney.partial.Result.Errors.__mergeResultNullable[${Type[A]}]($errorsNullable, $result)"
          )
      }

      def fromEmpty[A: Type]: Expr[partial.Result[A]] =
        c.Expr[partial.Result[A]](q"_root_.io.scalaland.chimney.partial.Result.fromEmpty[${Type[A]}]")

      def fromFunction[A: Type, B: Type](f: Expr[A => B]): Expr[A => partial.Result[B]] =
        c.Expr[A => partial.Result[B]](
          q"_root_.io.scalaland.chimney.partial.Result.fromFunction[${Type[A]}, ${Type[B]}]($f)"
        )

      def traverse[M: Type, A: Type, B: Type](
          it: Expr[Iterator[A]],
          f: Expr[A => partial.Result[B]],
          failFast: Expr[Boolean],
          factory: Expr[Factory[B, M]]
      ): Expr[partial.Result[M]] =
        c.Expr[partial.Result[M]](
          q"_root_.io.scalaland.chimney.partial.Result.traverse[${Type[M]}, ${Type[A]}, ${Type[B]}]($it, $f, $failFast)($factory)"
        )

      def sequence[M: Type, A: Type](
          it: Expr[Iterator[partial.Result[A]]],
          failFast: Expr[Boolean],
          factory: Expr[Factory[A, M]]
      ): Expr[partial.Result[M]] =
        c.Expr[partial.Result[M]](
          q"_root_.io.scalaland.chimney.partial.Result.sequence[${Type[M]}, ${Type[A]}]($it, $failFast)($factory)"
        )

      def flatMap[A: Type, B: Type](pr: Expr[partial.Result[A]])(
          f: Expr[A => partial.Result[B]]
      ): Expr[partial.Result[B]] =
        c.Expr[partial.Result[B]](q"$pr.flatMap[${Type[B]}]($f)")

      def flatten[A: Type](pr: Expr[partial.Result[partial.Result[A]]]): Expr[partial.Result[A]] =
        c.Expr[partial.Result[A]](q"$pr.flatten[${Type[A]}]")

      def map[A: Type, B: Type](pr: Expr[partial.Result[A]])(f: Expr[A => B]): Expr[partial.Result[B]] =
        c.Expr[partial.Result[B]](q"$pr.map[${Type[B]}]($f)")

      def map2[A: Type, B: Type, C: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          f: Expr[(A, B) => C],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[C]] =
        c.Expr[partial.Result[C]](
          q"_root_.io.scalaland.chimney.partial.Result.map2[${Type[A]}, ${Type[B]}, ${Type[C]}]($fa, $fb, $f, $failFast)"
        )

      def product[A: Type, B: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[(A, B)]] =
        c.Expr[partial.Result[(A, B)]](
          q"_root_.io.scalaland.chimney.partial.Result.product[${Type[A]}, ${Type[B]}]($fa, $fb, $failFast)"
        )

      def prependErrorPath[A: Type](
          fa: Expr[partial.Result[A]],
          path: Expr[partial.PathElement]
      ): Expr[partial.Result[A]] = c.Expr[partial.Result[A]](q"$fa.prependErrorPath($path)")
    }

    object PathElement extends PathElementModule {
      def Accessor(targetName: Expr[String]): Expr[partial.PathElement.Accessor] =
        c.Expr[partial.PathElement.Accessor](q"_root_.io.scalaland.chimney.partial.PathElement.Accessor($targetName)")
      def Index(index: Expr[Int]): Expr[partial.PathElement.Index] =
        c.Expr[partial.PathElement.Index](q"_root_.io.scalaland.chimney.partial.PathElement.Index($index)")
      def MapKey(key: Expr[Any]): Expr[partial.PathElement.MapKey] =
        c.Expr[partial.PathElement.MapKey](q"_root_.io.scalaland.chimney.partial.PathElement.MapKey($key)")
      def MapValue(value: Expr[Any]): Expr[partial.PathElement.MapValue] =
        c.Expr[partial.PathElement.MapValue](q"_root_.io.scalaland.chimney.partial.PathElement.MapValue($value)")
    }

    object RuntimeDataStore extends RuntimeDataStoreModule {

      val empty: Expr[TransformerDefinitionCommons.RuntimeDataStore] =
        c.Expr[TransformerDefinitionCommons.RuntimeDataStore](
          q"_root_.io.scalaland.chimney.dsl.TransformerDefinitionCommons.emptyRuntimeDataStore"
        )

      def extractAt(
          runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore],
          index: Int
      ): Expr[Any] = c.Expr[Any](q"$runtimeDataStore($index)")
    }

    object Patcher extends PatcherModule {

      def patch[A: Type, Patch: Type](
          patcher: Expr[io.scalaland.chimney.Patcher[A, Patch]],
          obj: Expr[A],
          patch: Expr[Patch]
      ): Expr[A] = c.Expr[A](q"$patcher.patch($obj, $patch)")

      def instance[A: Type, Patch: Type](
          f: (Expr[A], Expr[Patch]) => Expr[A]
      ): Expr[io.scalaland.chimney.Patcher[A, Patch]] = {
        val objTermName =
          ExprPromise.provideFreshName[A](ExprPromise.NameGenerationStrategy.FromType, ExprPromise.UsageHint.None)
        val patchTermName =
          ExprPromise.provideFreshName[Patch](ExprPromise.NameGenerationStrategy.FromType, ExprPromise.UsageHint.None)

        val objExpr: Expr[A] = c.Expr[A](q"$objTermName")
        val patchExpr: Expr[Patch] = c.Expr[Patch](q"$patchTermName")

        c.Expr[io.scalaland.chimney.Patcher[A, Patch]](
          q"""new _root_.io.scalaland.chimney.Patcher[${Type[A]}, ${Type[Patch]}] {
              def patch($objTermName: ${Type[A]}, $patchTermName: ${Type[Patch]}): ${Type[A]} = {
                ${f(objExpr, patchExpr)}
              }
            }"""
        )
      }
    }
  }
}
