package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.integrations
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

private[compiletime] trait ChimneyExprs { this: ChimneyDefinitions =>

  protected val ChimneyExpr: ChimneyExprModule
  protected trait ChimneyExprModule { this: ChimneyExpr.type =>

    val Transformer: TransformerModule
    trait TransformerModule { this: Transformer.type =>

      def transform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.Transformer[From, To]],
          src: Expr[From]
      ): Expr[To]

      def instance[From: Type, To: Type](f: Expr[From] => Expr[To]): Expr[io.scalaland.chimney.Transformer[From, To]]
    }

    val PartialTransformer: PartialTransformerModule
    trait PartialTransformerModule { this: PartialTransformer.type =>

      def transform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.PartialTransformer[From, To]],
          src: Expr[From],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[To]]

      def instance[From: Type, To: Type](
          toExpr: (Expr[From], Expr[Boolean]) => Expr[partial.Result[To]]
      ): Expr[io.scalaland.chimney.PartialTransformer[From, To]]
    }

    val PartialResult: PartialResultModule
    trait PartialResultModule { this: PartialResult.type =>

      val Value: ValueModule
      trait ValueModule { this: Value.type =>
        def apply[A: Type](value: Expr[A]): Expr[partial.Result.Value[A]]

        def value[A: Type](valueExpr: Expr[partial.Result.Value[A]]): Expr[A]
      }

      val Errors: ErrorsModule
      trait ErrorsModule { this: Errors.type =>

        def merge(
            errors1: Expr[partial.Result.Errors],
            errors2: Expr[partial.Result.Errors]
        ): Expr[partial.Result.Errors]

        def mergeResultNullable[A: Type](
            errorsNullable: Expr[partial.Result.Errors],
            result: Expr[partial.Result[A]]
        ): Expr[partial.Result.Errors]
      }

      def fromEmpty[A: Type]: Expr[partial.Result[A]]

      def fromFunction[A: Type, B: Type](f: Expr[A => B]): Expr[A => partial.Result[B]]

      def traverse[M: Type, A: Type, B: Type](
          it: Expr[Iterator[A]],
          f: Expr[A => partial.Result[B]],
          failFast: Expr[Boolean],
          factory: Expr[Factory[B, M]]
      ): Expr[partial.Result[M]]

      def sequence[M: Type, A: Type](
          it: Expr[Iterator[partial.Result[A]]],
          failFast: Expr[Boolean],
          factory: Expr[Factory[A, M]]
      ): Expr[partial.Result[M]]

      def flatMap[A: Type, B: Type](pr: Expr[partial.Result[A]])(
          f: Expr[A => partial.Result[B]]
      ): Expr[partial.Result[B]]

      def flatten[A: Type](pr: Expr[partial.Result[partial.Result[A]]]): Expr[partial.Result[A]]

      def map[A: Type, B: Type](pr: Expr[partial.Result[A]])(f: Expr[A => B]): Expr[partial.Result[B]]

      def map2[A: Type, B: Type, C: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          f: Expr[(A, B) => C],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[C]]

      def product[A: Type, B: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[(A, B)]]

      def prependErrorPath[A: Type](
          fa: Expr[partial.Result[A]],
          path: Expr[partial.PathElement]
      ): Expr[partial.Result[A]]
    }

    val PathElement: PathElementModule
    trait PathElementModule { this: PathElement.type =>
      def Accessor(targetName: Expr[String]): Expr[partial.PathElement.Accessor]
      def Index(index: Expr[Int]): Expr[partial.PathElement.Index]
      def MapKey(key: Expr[Any]): Expr[partial.PathElement.MapKey]
      def MapValue(key: Expr[Any]): Expr[partial.PathElement.MapValue]
    }

    val RuntimeDataStore: RuntimeDataStoreModule
    trait RuntimeDataStoreModule { this: RuntimeDataStore.type =>

      def empty: Expr[TransformerDefinitionCommons.RuntimeDataStore]

      def extractAt(
          runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore],
          index: Int
      ): Expr[Any]
    }

    val Patcher: PatcherModule
    trait PatcherModule { this: Patcher.type =>

      def patch[A: Type, Patch: Type](
          patcher: Expr[io.scalaland.chimney.Patcher[A, Patch]],
          obj: Expr[A],
          patch: Expr[Patch]
      ): Expr[A]

      def instance[A: Type, Patch: Type](
          f: (Expr[A], Expr[Patch]) => Expr[A]
      ): Expr[io.scalaland.chimney.Patcher[A, Patch]]
    }

    val DefaultValue: DefaultValueModule
    trait DefaultValueModule { this: DefaultValue.type =>

      def provide[Value: Type](defaultValue: Expr[integrations.DefaultValue[Value]]): Expr[Value]
    }

    val OptionalValue: OptionalValueModule
    trait OptionalValueModule { this: OptionalValue.type =>

      def empty[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]]
      ): Expr[Optional]

      def of[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          value: Expr[Value]
      ): Expr[Optional]

      def fold[Optional: Type, Value: Type, A: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          onNone: Expr[A],
          onSome: Expr[Value => A]
      ): Expr[A]

      def getOrElse[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          onNone: Expr[Value]
      ): Expr[Value]

      def orElse[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          optional2: Expr[Optional]
      ): Expr[Optional]
    }

    val PartiallyBuildIterable: PartiallyBuildIterableModule
    trait PartiallyBuildIterableModule { this: PartiallyBuildIterable.type =>

      def partialFactory[Collection: Type, Item: Type](
          partiallyBuildIterable: Expr[integrations.PartiallyBuildIterable[Collection, Item]]
      ): Expr[Factory[Item, partial.Result[Collection]]]

      def iterator[Collection: Type, Item: Type](
          partiallyBuildIterable: Expr[integrations.PartiallyBuildIterable[Collection, Item]],
          collection: Expr[Collection]
      ): Expr[Iterator[Item]]

      def to[Collection: Type, Item: Type, Collection2: Type](
          partiallyBuildIterable: Expr[integrations.PartiallyBuildIterable[Collection, Item]],
          collection: Expr[Collection],
          factory: Expr[Factory[Item, Collection2]]
      ): Expr[Collection2]
    }

    val TotallyBuildIterable: TotallyBuildIterableModule
    trait TotallyBuildIterableModule { this: TotallyBuildIterable.type =>

      def totalFactory[Collection: Type, Item: Type](
          totallyBuildIterable: Expr[integrations.TotallyBuildIterable[Collection, Item]]
      ): Expr[Factory[Item, Collection]]

      def iterator[Collection: Type, Item: Type](
          totallyBuildIterable: Expr[integrations.TotallyBuildIterable[Collection, Item]],
          collection: Expr[Collection]
      ): Expr[Iterator[Item]]

      def to[Collection: Type, Item: Type, Collection2: Type](
          totallyBuildIterable: Expr[integrations.TotallyBuildIterable[Collection, Item]],
          collection: Expr[Collection],
          factory: Expr[Factory[Item, Collection2]]
      ): Expr[Collection2]
    }
  }

  implicit final protected class TransformerExprOps[From: Type, To: Type](
      private val transformerExpr: Expr[io.scalaland.chimney.Transformer[From, To]]
  ) {

    def transform(src: Expr[From]): Expr[To] = ChimneyExpr.Transformer.transform(transformerExpr, src)
  }

  implicit final protected class PartialTransformerExprOps[From: Type, To: Type](
      private val transformerExpr: Expr[io.scalaland.chimney.PartialTransformer[From, To]]
  ) {

    def transform(src: Expr[From], failFast: Expr[Boolean]): Expr[partial.Result[To]] =
      ChimneyExpr.PartialTransformer.transform(transformerExpr, src, failFast)
  }

  implicit final protected class PartialResultExprOps[A: Type](private val resultExpr: Expr[partial.Result[A]]) {

    def flatMap[B: Type](fExpr: Expr[A => partial.Result[B]]): Expr[partial.Result[B]] =
      ChimneyExpr.PartialResult.flatMap(resultExpr)(fExpr)
    def map[B: Type](fExpr: Expr[A => B]): Expr[partial.Result[B]] = ChimneyExpr.PartialResult.map(resultExpr)(fExpr)

    def prependErrorPath(path: Expr[partial.PathElement]): Expr[partial.Result[A]] =
      ChimneyExpr.PartialResult.prependErrorPath(resultExpr, path)
  }

  implicit final protected class PartialResultFlattenExprOps[A: Type](
      private val resultExpr: Expr[partial.Result[partial.Result[A]]]
  ) {

    def flatten: Expr[partial.Result[A]] = ChimneyExpr.PartialResult.flatten(resultExpr)
  }

  implicit final protected class PartialResultValueExprOps[A: Type](
      private val valueExpr: Expr[partial.Result.Value[A]]
  ) {

    def value: Expr[A] = ChimneyExpr.PartialResult.Value.value(valueExpr)
  }

  implicit final protected class RuntimeDataStoreExprOps(
      private val runtimeDataStoreExpr: Expr[TransformerDefinitionCommons.RuntimeDataStore]
  ) {

    def apply(index: Int): Expr[Any] = ChimneyExpr.RuntimeDataStore.extractAt(runtimeDataStoreExpr, index)
  }

  implicit final protected class PatcherExprOps[A: Type, Patch: Type](
      private val patcherExpr: Expr[io.scalaland.chimney.Patcher[A, Patch]]
  ) {

    def patch(obj: Expr[A], patch: Expr[Patch]): Expr[A] =
      ChimneyExpr.Patcher.patch(patcherExpr, obj, patch)
  }

  implicit final protected class DefaultValueOps[Value: Type](
      private val defaultValueExpr: Expr[integrations.DefaultValue[Value]]
  ) {

    def provide(): Expr[Value] = ChimneyExpr.DefaultValue.provide(defaultValueExpr)
  }

  implicit final protected class OptionalValueOps[Optional: Type, Value: Type](
      private val optionalValueExpr: Expr[integrations.OptionalValue[Optional, Value]]
  ) {

    def empty: Expr[Optional] = ChimneyExpr.OptionalValue.empty(optionalValueExpr)

    def of(value: Expr[Value]): Expr[Optional] = ChimneyExpr.OptionalValue.of(optionalValueExpr, value)

    def fold[A: Type](optional: Expr[Optional], onNone: Expr[A], onSome: Expr[Value => A]): Expr[A] =
      ChimneyExpr.OptionalValue.fold(optionalValueExpr, optional, onNone, onSome)

    def getOrElse(optional: Expr[Optional], onNone: Expr[Value]): Expr[Value] =
      ChimneyExpr.OptionalValue.getOrElse(optionalValueExpr, optional, onNone)

    def orElse(optional: Expr[Optional], optional2: Expr[Optional]): Expr[Optional] =
      ChimneyExpr.OptionalValue.orElse(optionalValueExpr, optional, optional2)
  }

  implicit final protected class PartiallyBuildIterableOps[Collection: Type, Item: Type](
      private val partiallyBuildIterableExpr: Expr[integrations.PartiallyBuildIterable[Collection, Item]]
  ) {

    def partialFactory: Expr[Factory[Item, partial.Result[Collection]]] =
      ChimneyExpr.PartiallyBuildIterable.partialFactory(partiallyBuildIterableExpr)

    def iterator(collection: Expr[Collection]): Expr[Iterator[Item]] =
      ChimneyExpr.PartiallyBuildIterable.iterator(partiallyBuildIterableExpr, collection)

    def to[Collection2: Type](
        collection: Expr[Collection],
        factory: Expr[Factory[Item, Collection2]]
    ): Expr[Collection2] =
      ChimneyExpr.PartiallyBuildIterable.to(partiallyBuildIterableExpr, collection, factory)
  }

  implicit final protected class TotallyBuildIterableOps[Collection: Type, Item: Type](
      private val totallyBuildIterableExpr: Expr[integrations.TotallyBuildIterable[Collection, Item]]
  ) {

    def totalFactory: Expr[Factory[Item, Collection]] =
      ChimneyExpr.TotallyBuildIterable.totalFactory(totallyBuildIterableExpr)

    def iterator(collection: Expr[Collection]): Expr[Iterator[Item]] =
      ChimneyExpr.TotallyBuildIterable.iterator(totallyBuildIterableExpr, collection)

    def to[Collection2: Type](
        collection: Expr[Collection],
        factory: Expr[Factory[Item, Collection2]]
    ): Expr[Collection2] =
      ChimneyExpr.TotallyBuildIterable.to(totallyBuildIterableExpr, collection, factory)
  }
}
