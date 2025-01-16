package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.integrations
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
            q"_root_.io.scalaland.chimney.internal.runtime.ResultUtils.mergeNullable[${Type[A]}]($errorsNullable, $result)"
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

      def unsealErrorPath[A: Type](
          fa: Expr[partial.Result[A]]
      ): Expr[partial.Result[A]] = c.Expr[partial.Result[A]](q"$fa.unsealErrorPath")
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
      def Const(targetPath: Expr[String]): Expr[partial.PathElement.Const] =
        c.Expr[partial.PathElement.Const](q"_root_.io.scalaland.chimney.partial.PathElement.Const($targetPath)")
      def Computed(targetPath: Expr[String]): Expr[partial.PathElement.Computed] =
        c.Expr[partial.PathElement.Computed](q"_root_.io.scalaland.chimney.partial.PathElement.Computed($targetPath)")
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

    object PartialOuterTransformer extends PartialOuterTransformerModule {

      def transformWithTotalInner[From: Type, To: Type, InnerFrom: Type, InnerTo: Type](
          partialOuterTransformer: Expr[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]],
          src: Expr[From],
          failFast: Expr[Boolean],
          inner: Expr[InnerFrom => InnerTo]
      ): Expr[partial.Result[To]] =
        c.Expr[partial.Result[To]](q"$partialOuterTransformer.transformWithTotalInner($src, $failFast, $inner)")

      def transformWithPartialInner[From: Type, To: Type, InnerFrom: Type, InnerTo: Type](
          partialOuterTransformer: Expr[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]],
          src: Expr[From],
          failFast: Expr[Boolean],
          inner: Expr[InnerFrom => partial.Result[InnerTo]]
      ): Expr[partial.Result[To]] =
        c.Expr[partial.Result[To]](q"$partialOuterTransformer.transformWithPartialInner($src, $failFast, $inner)")
    }

    object TotalOuterTransformer extends TotalOuterTransformerModule {

      def transformWithTotalInner[From: Type, To: Type, InnerFrom: Type, InnerTo: Type](
          totalOuterTransformer: Expr[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]],
          src: Expr[From],
          inner: Expr[InnerFrom => InnerTo]
      ): Expr[To] =
        c.Expr[To](q"$totalOuterTransformer.transformWithTotalInner($src, $inner)")

      def transformWithPartialInner[From: Type, To: Type, InnerFrom: Type, InnerTo: Type](
          totalOuterTransformer: Expr[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]],
          src: Expr[From],
          failFast: Expr[Boolean],
          inner: Expr[InnerFrom => partial.Result[InnerTo]]
      ): Expr[partial.Result[To]] =
        c.Expr[partial.Result[To]](q"$totalOuterTransformer.transformWithPartialInner($src, $failFast, $inner)")
    }

    object DefaultValue extends DefaultValueModule {

      def provide[Value: Type](
          defaultValue: Expr[integrations.DefaultValue[Value]]
      ): Expr[Value] = c.Expr[Value](q"$defaultValue.provide()")
    }

    object OptionalValue extends OptionalValueModule {

      def empty[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]]
      ): Expr[Optional] = c.Expr[Optional](q"$optionalValue.empty")

      def of[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          value: Expr[Value]
      ): Expr[Optional] = c.Expr[Optional](q"$optionalValue.of($value)")

      def fold[Optional: Type, Value: Type, A: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          onNone: Expr[A],
          onSome: Expr[Value => A]
      ): Expr[A] = c.Expr[A](q"$optionalValue.fold($optional, $onNone, $onSome)")

      def getOrElse[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          onNone: Expr[Value]
      ): Expr[Value] = c.Expr[Value](q"$optionalValue.getOrElse($optional, $onNone)")

      def orElse[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          optional2: Expr[Optional]
      ): Expr[Optional] = c.Expr[Optional](q"$optionalValue.orElse($optional, $optional2)")
    }

    object PartiallyBuildIterable extends PartiallyBuildIterableModule {

      def partialFactory[Collection: Type, Item: Type](
          partiallyBuildIterable: Expr[integrations.PartiallyBuildIterable[Collection, Item]]
      ): Expr[Factory[Item, partial.Result[Collection]]] =
        c.Expr[Factory[Item, partial.Result[Collection]]](q"$partiallyBuildIterable.partialFactory")

      def iterator[Collection: Type, Item: Type](
          partiallyBuildIterable: Expr[integrations.PartiallyBuildIterable[Collection, Item]],
          collection: Expr[Collection]
      ): Expr[Iterator[Item]] = c.Expr[Iterator[Item]](q"$partiallyBuildIterable.iterator($collection)")

      def to[Collection: Type, Item: Type, Collection2: Type](
          partiallyBuildIterable: Expr[integrations.PartiallyBuildIterable[Collection, Item]],
          collection: Expr[Collection],
          factory: Expr[Factory[Item, Collection2]]
      ): Expr[Collection2] = c.Expr[Collection2](q"$partiallyBuildIterable.to($collection, $factory)")
    }

    object TotallyBuildIterable extends TotallyBuildIterableModule {

      def totalFactory[Collection: Type, Item: Type](
          totallyBuildIterable: Expr[integrations.TotallyBuildIterable[Collection, Item]]
      ): Expr[Factory[Item, Collection]] = c.Expr[Factory[Item, Collection]](q"$totallyBuildIterable.totalFactory")

      def iterator[Collection: Type, Item: Type](
          totallyBuildIterable: Expr[integrations.TotallyBuildIterable[Collection, Item]],
          collection: Expr[Collection]
      ): Expr[Iterator[Item]] = c.Expr[Iterator[Item]](q"$totallyBuildIterable.iterator($collection)")

      def to[Collection: Type, Item: Type, Collection2: Type](
          totallyBuildIterable: Expr[integrations.TotallyBuildIterable[Collection, Item]],
          collection: Expr[Collection],
          factory: Expr[Factory[Item, Collection2]]
      ): Expr[Collection2] = c.Expr[Collection2](q"$totallyBuildIterable.to($collection, $factory)")
    }
  }
}
