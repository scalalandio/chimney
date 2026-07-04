package io.scalaland.chimney.internal.compiletime

import hearth.fp.effect.MIO
import hearth.fp.syntax.*
import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.integrations
import io.scalaland.chimney.partial

import scala.collection.Factory

/** NB: the `Transformer.instance`/`PartialTransformer.instance`/`Patcher.instance` builders generate the instance as
  * {{{
  * {
  *   def transformbody$macro$N(int$macro$M: Int): Out = <derived body> // ValDefBuilder.ofDefN
  *   new Transformer[Int, Out] { def transform(src: Int): Out = transformbody$macro$N(src) }
  * }
  * }}}
  * The derivation sees the generated def's `FreshName.FromType`-named parameter - this keeps the type-derived names in
  * error messages ("derivation from int: ...") - while the instance method keeps its literal parameter names
  * (`src`/`failFast`/`obj`/`patch`).
  *
  * CROSS-QUOTES USAGE CONTRACT: an expr that is spliced has to be created inside the expr that is splicing it. This is
  * Hearth's canonical `ValDefBuilder`-recipe: the body derivation runs as a lazy `MIO` value (`.traverse`) OUTSIDE any
  * quote - under the macro-entry scope, where every helper, cache and definition it touches lives - and hearth's
  * `ofDefN` owns/heals the body into the generated def. The only thing evaluated inside the instance-method
  * `Expr.splice` is the def-CALL builder (a plain name application), which is invoked at splice-evaluation time and so
  * is created inside the expr that splices it. Nothing derived under one splice is ever consumed by another.
  *
  * (The previous architecture - `direct`+`await` running the whole derivation inside the instance-method splice on
  * Scala 2, and a hand-rolled "derive-first-against-a-promised-symbol + changeOwner" Scala 3 override
  * (`prependFreshValCompat`/`withMacroEntryCtxCompat`, hearth#317/#318) - is GONE. Running whole derivations inside a
  * splice is not supported by Hearth's published API surface: `LambdaBuilder`'s body re-owning uses the macro-entry
  * `Symbol.spliceOwner` while `freshTerm` follows `CrossQuotes.ctx`, so rule-built lambdas mix owners under a nested
  * ctx - and trait-level caches would additionally need per-splice scoping, see `MacroCommonsCompat.TypeCache`.)
  *
  * TODO(hearth-migration): most of this module is a layer of trivial `Expr.quote`/`Expr.splice` forwarders that the
  * rules could inline directly. The `instance` builders (and anything else that carries real logic) should be kept, but
  * the rest of the module can go.
  */
private[compiletime] trait ChimneyExprs { this: ChimneyDefinitions & hearth.MacroCommons =>

  protected object ChimneyExpr {

    object Transformer {

      def transform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.Transformer[From, To]],
          src: Expr[From]
      ): Expr[To] = Expr.quote {
        Expr.splice(transformer).transform(Expr.splice(src))
      }

      /** Builds a `Transformer[From, To]` instance expr - see the trait's ScalaDoc for the generated shape and the
        * cross-quotes usage-contract rationale.
        */
      def instance[From: Type, To: Type](
          deriveBody: Expr[From] => MIO[Expr[To]]
      ): MIO[Expr[io.scalaland.chimney.Transformer[From, To]]] =
        ValDefBuilder
          .ofDef1[From, To](FreshName.FromPrefix("transformbody"))
          .traverse { case (_, fromRef) => deriveBody(fromRef) }
          .map(_.build.use { callTransformBody =>
            Expr.quote {
              new io.scalaland.chimney.Transformer[From, To] {
                def transform(src: From): To = Expr.splice(callTransformBody(Expr.quote(src)))
              }
            }
          })
    }

    object PartialTransformer {

      def transform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.PartialTransformer[From, To]],
          src: Expr[From],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[To]] = Expr.quote {
        Expr.splice(transformer).transform(Expr.splice(src), Expr.splice(failFast))
      }

      /** Builds a `PartialTransformer[From, To]` instance expr - see [[ChimneyExpr.Transformer.instance]]. */
      def instance[From: Type, To: Type](
          deriveBody: (Expr[From], Expr[Boolean]) => MIO[Expr[partial.Result[To]]]
      ): MIO[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] = {
        implicit val BooleanType: Type[Boolean] = Type.of[Boolean]
        implicit val PartialResultTo: Type[partial.Result[To]] = ChimneyType.PartialResult[To]
        ValDefBuilder
          .ofDef2[From, Boolean, partial.Result[To]](
            FreshName.FromPrefix("transformbody"),
            FreshName.FromType,
            FreshName.FromPrefix("failfast")
          )
          .traverse { case (_, (fromRef, failFastRef)) => deriveBody(fromRef, failFastRef) }
          .map(_.build.use { callTransformBody =>
            Expr.quote {
              new io.scalaland.chimney.PartialTransformer[From, To] {
                def transform(src: From, failFast: Boolean): partial.Result[To] =
                  Expr.splice(callTransformBody(Expr.quote(src), Expr.quote(failFast)))
              }
            }
          })
      }
    }

    object PartialResult {

      object Value {
        def apply[A: Type](value: Expr[A]): Expr[partial.Result.Value[A]] = Expr.quote {
          partial.Result.Value[A](Expr.splice(value))
        }

        def value[A: Type](valueExpr: Expr[partial.Result.Value[A]]): Expr[A] = Expr.quote {
          Expr.splice(valueExpr).value
        }
      }

      object Errors {

        def merge(
            errors1: Expr[partial.Result.Errors],
            errors2: Expr[partial.Result.Errors]
        ): Expr[partial.Result.Errors] = Expr.quote {
          partial.Result.Errors.merge(Expr.splice(errors1), Expr.splice(errors2))
        }

        def mergeResultNullable[A: Type](
            errorsNullable: Expr[partial.Result.Errors],
            result: Expr[partial.Result[A]]
        ): Expr[partial.Result.Errors] = Expr.quote {
          io.scalaland.chimney.internal.runtime.ResultUtils.mergeNullable[A](
            Expr.splice(errorsNullable),
            Expr.splice(result)
          )
        }
      }

      def fromEmpty[A: Type]: Expr[partial.Result[A]] = Expr.quote {
        partial.Result.fromEmpty[A]
      }

      def fromFunction[A: Type, B: Type](f: Expr[A => B]): Expr[A => partial.Result[B]] = Expr.quote {
        partial.Result.fromFunction[A, B](Expr.splice(f))
      }

      def traverse[M: Type, A: Type, B: Type](
          it: Expr[Iterator[A]],
          f: Expr[A => partial.Result[B]],
          failFast: Expr[Boolean],
          factory: Expr[Factory[B, M]]
      ): Expr[partial.Result[M]] = Expr.quote {
        partial.Result.traverse[M, A, B](Expr.splice(it), Expr.splice(f), Expr.splice(failFast))(Expr.splice(factory))
      }

      def sequence[M: Type, A: Type](
          it: Expr[Iterator[partial.Result[A]]],
          failFast: Expr[Boolean],
          factory: Expr[Factory[A, M]]
      ): Expr[partial.Result[M]] = Expr.quote {
        partial.Result.sequence[M, A](Expr.splice(it), Expr.splice(failFast))(Expr.splice(factory))
      }

      def flatMap[A: Type, B: Type](pr: Expr[partial.Result[A]])(
          f: Expr[A => partial.Result[B]]
      ): Expr[partial.Result[B]] = Expr.quote {
        Expr.splice(pr).flatMap[B](Expr.splice(f))
      }

      def flatten[A: Type](pr: Expr[partial.Result[partial.Result[A]]]): Expr[partial.Result[A]] = Expr.quote {
        Expr.splice(pr).flatten[A]
      }

      def map[A: Type, B: Type](pr: Expr[partial.Result[A]])(f: Expr[A => B]): Expr[partial.Result[B]] = Expr.quote {
        Expr.splice(pr).map[B](Expr.splice(f))
      }

      def map2[A: Type, B: Type, C: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          f: Expr[(A, B) => C],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[C]] = Expr.quote {
        partial.Result.map2[A, B, C](
          Expr.splice(fa),
          Expr.splice(fb),
          Expr.splice(f),
          Expr.splice(failFast)
        )
      }

      def product[A: Type, B: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[(A, B)]] = Expr.quote {
        partial.Result.product[A, B](Expr.splice(fa), Expr.splice(fb), Expr.splice(failFast))
      }

      def prependErrorPath[A: Type](
          fa: Expr[partial.Result[A]],
          path: Expr[partial.PathElement]
      ): Expr[partial.Result[A]] = Expr.quote {
        Expr.splice(fa).prependErrorPath(Expr.splice(path))
      }

      def unsealErrorPath[A: Type](
          fa: Expr[partial.Result[A]]
      ): Expr[partial.Result[A]] = Expr.quote {
        Expr.splice(fa).unsealErrorPath
      }
    }

    object PathElement {

      def Accessor(targetName: Expr[String]): Expr[partial.PathElement.Accessor] = Expr.quote {
        partial.PathElement.Accessor(Expr.splice(targetName))
      }

      def Index(index: Expr[Int]): Expr[partial.PathElement.Index] = Expr.quote {
        partial.PathElement.Index(Expr.splice(index))
      }

      def MapKey(key: Expr[Any]): Expr[partial.PathElement.MapKey] = Expr.quote {
        partial.PathElement.MapKey(Expr.splice(key))
      }

      def MapValue(key: Expr[Any]): Expr[partial.PathElement.MapValue] = Expr.quote {
        partial.PathElement.MapValue(Expr.splice(key))
      }

      def Const(targetPath: Expr[String]): Expr[partial.PathElement.Const] = Expr.quote {
        partial.PathElement.Const(Expr.splice(targetPath))
      }

      def Computed(targetPath: Expr[String]): Expr[partial.PathElement.Computed] = Expr.quote {
        partial.PathElement.Computed(Expr.splice(targetPath))
      }
    }

    object RuntimeDataStore {

      def empty: Expr[TransformerDefinitionCommons.RuntimeDataStore] = Expr.quote {
        io.scalaland.chimney.dsl.TransformerDefinitionCommons.emptyRuntimeDataStore
      }

      def extractAt(
          runtimeDataStore: Expr[TransformerDefinitionCommons.RuntimeDataStore],
          index: Int
      ): Expr[Any] = {
        val indexExpr = Expr(index)
        Expr.quote {
          Expr.splice(runtimeDataStore).apply(Expr.splice(indexExpr))
        }
      }
    }

    object Patcher {

      def patch[A: Type, Patch: Type](
          patcher: Expr[io.scalaland.chimney.Patcher[A, Patch]],
          obj: Expr[A],
          patch: Expr[Patch]
      ): Expr[A] = Expr.quote {
        Expr.splice(patcher).patch(Expr.splice(obj), Expr.splice(patch))
      }

      /** Builds a `Patcher[A, Patch]` instance expr - see [[ChimneyExpr.Transformer.instance]]. */
      def instance[A: Type, Patch: Type](
          deriveBody: (Expr[A], Expr[Patch]) => MIO[Expr[A]]
      ): MIO[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
        ValDefBuilder
          .ofDef2[A, Patch, A](FreshName.FromPrefix("patchbody"))
          .traverse { case (_, (objRef, patchRef)) => deriveBody(objRef, patchRef) }
          .map(_.build.use { callPatchBody =>
            Expr.quote {
              new io.scalaland.chimney.Patcher[A, Patch] {
                def patch(obj: A, patch: Patch): A = Expr.splice(callPatchBody(Expr.quote(obj), Expr.quote(patch)))
              }
            }
          })
    }

    object PartialOuterTransformer {

      def transformWithTotalInner[From: Type, To: Type, InnerFrom: Type, InnerTo: Type](
          partialOuterTransformer: Expr[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]],
          src: Expr[From],
          failFast: Expr[Boolean],
          inner: Expr[InnerFrom => InnerTo]
      ): Expr[partial.Result[To]] = Expr.quote {
        Expr
          .splice(partialOuterTransformer)
          .transformWithTotalInner(Expr.splice(src), Expr.splice(failFast), Expr.splice(inner))
      }

      def transformWithPartialInner[From: Type, To: Type, InnerFrom: Type, InnerTo: Type](
          partialOuterTransformer: Expr[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]],
          src: Expr[From],
          failFast: Expr[Boolean],
          inner: Expr[InnerFrom => partial.Result[InnerTo]]
      ): Expr[partial.Result[To]] = Expr.quote {
        Expr
          .splice(partialOuterTransformer)
          .transformWithPartialInner(Expr.splice(src), Expr.splice(failFast), Expr.splice(inner))
      }
    }

    object TotalOuterTransformer {

      def transformWithTotalInner[From: Type, To: Type, InnerFrom: Type, InnerTo: Type](
          totalOuterTransformer: Expr[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]],
          src: Expr[From],
          inner: Expr[InnerFrom => InnerTo]
      ): Expr[To] = Expr.quote {
        Expr.splice(totalOuterTransformer).transformWithTotalInner(Expr.splice(src), Expr.splice(inner))
      }

      def transformWithPartialInner[From: Type, To: Type, InnerFrom: Type, InnerTo: Type](
          totalOuterTransformer: Expr[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]],
          src: Expr[From],
          failFast: Expr[Boolean],
          inner: Expr[InnerFrom => partial.Result[InnerTo]]
      ): Expr[partial.Result[To]] = Expr.quote {
        Expr
          .splice(totalOuterTransformer)
          .transformWithPartialInner(Expr.splice(src), Expr.splice(failFast), Expr.splice(inner))
      }
    }

    object DefaultValue {

      def provide[Value: Type](defaultValue: Expr[integrations.DefaultValue[Value]]): Expr[Value] = Expr.quote {
        Expr.splice(defaultValue).provide()
      }
    }

    object OptionalValue {

      def empty[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]]
      ): Expr[Optional] = Expr.quote {
        Expr.splice(optionalValue).empty
      }

      def of[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          value: Expr[Value]
      ): Expr[Optional] = Expr.quote {
        Expr.splice(optionalValue).of(Expr.splice(value))
      }

      def fold[Optional: Type, Value: Type, A: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          onNone: Expr[A],
          onSome: Expr[Value => A]
      ): Expr[A] = Expr.quote {
        Expr.splice(optionalValue).fold(Expr.splice(optional), Expr.splice(onNone), Expr.splice(onSome))
      }

      def getOrElse[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          onNone: Expr[Value]
      ): Expr[Value] = Expr.quote {
        Expr.splice(optionalValue).getOrElse(Expr.splice(optional), Expr.splice(onNone))
      }

      def orElse[Optional: Type, Value: Type](
          optionalValue: Expr[integrations.OptionalValue[Optional, Value]],
          optional: Expr[Optional],
          optional2: Expr[Optional]
      ): Expr[Optional] = Expr.quote {
        Expr.splice(optionalValue).orElse(Expr.splice(optional), Expr.splice(optional2))
      }
    }

    object PartiallyBuildIterable {

      def partialFactory[Collection: Type, Item: Type](
          partiallyBuildIterable: Expr[integrations.PartiallyBuildIterable[Collection, Item]]
      ): Expr[Factory[Item, partial.Result[Collection]]] = Expr.quote {
        Expr.splice(partiallyBuildIterable).partialFactory
      }

      def iterator[Collection: Type, Item: Type](
          partiallyBuildIterable: Expr[integrations.PartiallyBuildIterable[Collection, Item]],
          collection: Expr[Collection]
      ): Expr[Iterator[Item]] = Expr.quote {
        Expr.splice(partiallyBuildIterable).iterator(Expr.splice(collection))
      }

      def to[Collection: Type, Item: Type, Collection2: Type](
          partiallyBuildIterable: Expr[integrations.PartiallyBuildIterable[Collection, Item]],
          collection: Expr[Collection],
          factory: Expr[Factory[Item, Collection2]]
      ): Expr[Collection2] = Expr.quote {
        Expr.splice(partiallyBuildIterable).to(Expr.splice(collection), Expr.splice(factory))
      }
    }

    object TotallyBuildIterable {

      def totalFactory[Collection: Type, Item: Type](
          totallyBuildIterable: Expr[integrations.TotallyBuildIterable[Collection, Item]]
      ): Expr[Factory[Item, Collection]] = Expr.quote {
        Expr.splice(totallyBuildIterable).totalFactory
      }

      def iterator[Collection: Type, Item: Type](
          totallyBuildIterable: Expr[integrations.TotallyBuildIterable[Collection, Item]],
          collection: Expr[Collection]
      ): Expr[Iterator[Item]] = Expr.quote {
        Expr.splice(totallyBuildIterable).iterator(Expr.splice(collection))
      }

      def to[Collection: Type, Item: Type, Collection2: Type](
          totallyBuildIterable: Expr[integrations.TotallyBuildIterable[Collection, Item]],
          collection: Expr[Collection],
          factory: Expr[Factory[Item, Collection2]]
      ): Expr[Collection2] = Expr.quote {
        Expr.splice(totallyBuildIterable).to(Expr.splice(collection), Expr.splice(factory))
      }
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
    def map2[B: Type, C: Type](result2Expr: Expr[partial.Result[B]], failFast: Expr[Boolean])(
        fExpr: Expr[(A, B) => C]
    ): Expr[partial.Result[C]] =
      ChimneyExpr.PartialResult.map2(resultExpr, result2Expr, fExpr, failFast)

    def prependErrorPath(path: Expr[partial.PathElement]): Expr[partial.Result[A]] =
      ChimneyExpr.PartialResult.prependErrorPath(resultExpr, path)

    def unsealErrorPath: Expr[partial.Result[A]] =
      ChimneyExpr.PartialResult.unsealErrorPath(resultExpr)
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

  implicit final protected class PartialOuterTransformerOps[From: Type, To: Type, InnerFrom: Type, InnerTo: Type](
      private val
      partialOuterTransformer: Expr[integrations.PartialOuterTransformer[From, To, InnerFrom, InnerTo]]
  ) {

    def transformWithTotalInner(
        src: Expr[From],
        failFast: Expr[Boolean],
        inner: Expr[InnerFrom => InnerTo]
    ): Expr[partial.Result[To]] =
      ChimneyExpr.PartialOuterTransformer.transformWithTotalInner(partialOuterTransformer, src, failFast, inner)

    def transformWithPartialInner(
        src: Expr[From],
        failFast: Expr[Boolean],
        inner: Expr[InnerFrom => partial.Result[InnerTo]]
    ): Expr[partial.Result[To]] =
      ChimneyExpr.PartialOuterTransformer.transformWithPartialInner(partialOuterTransformer, src, failFast, inner)
  }

  implicit final protected class TotalOuterTransformerOps[From: Type, To: Type, InnerFrom: Type, InnerTo: Type](
      private val
      totalOuterTransformer: Expr[integrations.TotalOuterTransformer[From, To, InnerFrom, InnerTo]]
  ) {

    def transformWithTotalInner(
        src: Expr[From],
        inner: Expr[InnerFrom => InnerTo]
    ): Expr[To] = ChimneyExpr.TotalOuterTransformer.transformWithTotalInner(totalOuterTransformer, src, inner)

    def transformWithPartialInner(
        src: Expr[From],
        failFast: Expr[Boolean],
        inner: Expr[InnerFrom => partial.Result[InnerTo]]
    ): Expr[partial.Result[To]] =
      ChimneyExpr.TotalOuterTransformer.transformWithPartialInner(totalOuterTransformer, src, failFast, inner)
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
