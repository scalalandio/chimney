package io.scalaland.chimney.internal.compiletime

import hearth.fp.DirectStyle
import hearth.fp.effect.MIO
import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.integrations
import io.scalaland.chimney.partial

import scala.collection.Factory
import scala.util.control.NonFatal

/** NB: in `Transformer.instance`/`PartialTransformer.instance`/`Patcher.instance` the method parameters keep their
  * literal names (`src`/`failFast`/`obj`/`patch`) but a `FreshName.FromType`-named val is prepended (`val int$macro$N =
  * src`) and the derivation sees THAT reference - this keeps the type-derived names in error messages ("derivation from
  * int: ...").
  *
  * TODO(hearth-migration): most of this module is a layer of trivial `Expr.quote`/`Expr.splice` forwarders that the
  * rules could inline directly. The `instance` builders (and anything else that carries real logic, like the fresh-name
  * val prepending below) should be kept, but the rest of the module can go.
  */
private[compiletime] trait ChimneyExprs { this: ChimneyDefinitions & hearth.MacroCommons =>

  /** Runs `thunk` in MIO's direct style: `await` unwraps `MIO` values inside it (needed because
    * `ChimneyExpr.*.instance` take pure `Expr => Expr` functions).
    *
    * NonFatal exceptions of the block itself become failures (MIO's `scoped` lets them fly); `RunSafe`'s own
    * error-passing uses a ControlThrowable, which NonFatal does not intercept, so awaiting failed results still works.
    */
  private def direct[A](thunk: DirectStyle.RunSafe[MIO] => A): MIO[A] =
    MIO
      .scoped { runSafe =>
        try Right(thunk(runSafe))
        catch { case NonFatal(error) => Left(error) }
      }
      .flatMap {
        case Right(value) => MIO.pure(value)
        case Left(error)  => MIO.fail(error)
      }

  /** Builds a `Transformer[From, To]` instance expr, running the body derivation through `deriveBody`.
    *
    * HEARTH 0.4.0 ISSUE WORKAROUND (hearth#318, Scala 3): the shared default (used on Scala 2) keeps the [[direct]] +
    * `await`-inside-the-quote shape. On Scala 3 MIO's `await` hops to a `DirectStyleExecutor` thread, so exprs quoted
    * in the instance-method splice (e.g. `failFast`) and the awaited derivation result belong to different splice
    * evaluations and `-Xcheck-macros` aborts with "ScopeException: Expression created in a splice was used outside of
    * that splice". The Scala 3 `PlatformBridge` overrides these three builders: mint a fresh `FromType`-named val
    * symbol first, run the derivation against its `Ref` (plain MIO, no direct style), and only then construct the
    * instance quote, binding the val to the method parameter inside the splice.
    *
    * Re-verified against 0.4.0-16-gd4adc1c-SNAPSHOT (it.30): running the WHOLE derivation inside the splice pinned via
    * the new `withMacroEntryCtx` trades the ScopeException for owner mismatches (entry-owned Hearth defs inside
    * nested-splice-owned quote fragments), so the derive-first overrides stay.
    */
  protected def transformerInstanceCompat[From: Type, To: Type](
      deriveBody: Expr[From] => MIO[Expr[To]]
  ): MIO[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    direct { await =>
      ChimneyExpr.Transformer.instance[From, To] { (src: Expr[From]) =>
        await(deriveBody(src))
      }
    }

  /** Builds a `PartialTransformer[From, To]` instance expr - see [[transformerInstanceCompat]]. */
  protected def partialTransformerInstanceCompat[From: Type, To: Type](
      deriveBody: (Expr[From], Expr[Boolean]) => MIO[Expr[partial.Result[To]]]
  ): MIO[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    direct { await =>
      ChimneyExpr.PartialTransformer.instance[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
        await(deriveBody(src, failFast))
      }
    }

  /** Builds a `Patcher[A, Patch]` instance expr - see [[transformerInstanceCompat]]. */
  protected def patcherInstanceCompat[A: Type, Patch: Type](
      deriveBody: (Expr[A], Expr[Patch]) => MIO[Expr[A]]
  ): MIO[Expr[io.scalaland.chimney.Patcher[A, Patch]]] =
    direct { await =>
      ChimneyExpr.Patcher.instance[A, Patch] { (obj: Expr[A], patch: Expr[Patch]) =>
        await(deriveBody(obj, patch))
      }
    }

  protected object ChimneyExpr {

    object Transformer {

      def transform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.Transformer[From, To]],
          src: Expr[From]
      ): Expr[To] = Expr.quote {
        Expr.splice(transformer).transform(Expr.splice(src))
      }

      def instance[From: Type, To: Type](
          toExpr: Expr[From] => Expr[To]
      ): Expr[io.scalaland.chimney.Transformer[From, To]] = Expr.quote {
        new io.scalaland.chimney.Transformer[From, To] {
          def transform(src: From): To = Expr.splice {
            // Prepend a FromType-named val so that derivation/errors see e.g. `int`;
            // suppressUnused because the derived body might not reference the source at all (e.g. singleton target);
            // srcRef captured BEFORE withMacroEntryCtxCompat (it must be quoted in the splice's own scope), the
            // derivation itself runs under the macro-entry ctx so nothing it creates/caches is splice-scoped and
            // owners stay consistent - see MacroCommonsCompat for both Scala 3 -Xcheck-macros pitfalls.
            val srcRef = Expr.quote(src)
            withMacroEntryCtxCompat {
              prependFreshValCompat[From, To](srcRef) { fromRef =>
                Expr.quote {
                  Expr.splice(Expr.suppressUnused(fromRef))
                  Expr.splice(toExpr(fromRef))
                }
              }
            }
          }
        }
      }
    }

    object PartialTransformer {

      def transform[From: Type, To: Type](
          transformer: Expr[io.scalaland.chimney.PartialTransformer[From, To]],
          src: Expr[From],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[To]] = Expr.quote {
        Expr.splice(transformer).transform(Expr.splice(src), Expr.splice(failFast))
      }

      def instance[From: Type, To: Type](
          toExpr: (Expr[From], Expr[Boolean]) => Expr[partial.Result[To]]
      ): Expr[io.scalaland.chimney.PartialTransformer[From, To]] = Expr.quote {
        new io.scalaland.chimney.PartialTransformer[From, To] {
          def transform(src: From, failFast: Boolean): partial.Result[To] = Expr.splice {
            // Prepend a FromType-named val so that derivation/errors see e.g. `int`;
            // suppressUnused because the derived body might not reference the source at all (e.g. singleton target);
            // srcRef/failFastRef captured BEFORE withMacroEntryCtxCompat (they must be quoted in the splice's own
            // scope), the derivation itself runs under the macro-entry ctx so nothing it creates/caches is
            // splice-scoped and owners stay consistent - see MacroCommonsCompat for both Scala 3 pitfalls.
            val srcRef = Expr.quote(src)
            val failFastRef = Expr.quote(failFast)
            withMacroEntryCtxCompat {
              implicit val PartialResultTo: Type[partial.Result[To]] = ChimneyType.PartialResult[To]
              prependFreshValCompat[From, partial.Result[To]](srcRef) { fromRef =>
                Expr.quote {
                  Expr.splice(Expr.suppressUnused(fromRef))
                  Expr.splice(toExpr(fromRef, failFastRef))
                }
              }
            }
          }
        }
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

      def instance[A: Type, Patch: Type](
          f: (Expr[A], Expr[Patch]) => Expr[A]
      ): Expr[io.scalaland.chimney.Patcher[A, Patch]] = Expr.quote {
        new io.scalaland.chimney.Patcher[A, Patch] {
          def patch(obj: A, patch: Patch): A = Expr.splice {
            // Prepend FromType-named vals so that derivation/errors see e.g. `user`;
            // suppressUnused because the derived body might not reference them at all (e.g. singleton target);
            // objRef/patchRef captured BEFORE withMacroEntryCtxCompat (they must be quoted in the splice's own
            // scope), the derivation itself runs under the macro-entry ctx (see MacroCommonsCompat). Nesting two
            // prepends produces `{ val a = obj; { val p = patch; body } }` (nested, not a flat two-val block) -
            // semantically identical.
            val objRef = Expr.quote(obj)
            val patchRef0 = Expr.quote(patch)
            withMacroEntryCtxCompat {
              prependFreshValCompat[A, A](objRef) { objRef2 =>
                prependFreshValCompat[Patch, A](patchRef0) { patchRef2 =>
                  Expr.quote {
                    Expr.splice(Expr.suppressUnused(objRef2))
                    Expr.splice(Expr.suppressUnused(patchRef2))
                    Expr.splice(f(objRef2, patchRef2))
                  }
                }
              }
            }
          }
        }
      }
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
