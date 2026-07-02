package io.scalaland.chimney.internal.compiletime2.derivation.transformer.rules

import hearth.fp.syntax.*
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformIterableToIterableRuleModule`.
  *
  * Differences vs the old version:
  *   - `ExprPromise.promise(...).traverse(...)` becomes `LambdaBuilder.of1/of2(...).traverse(...)` (the lambdas are
  *     passed to the runtime `partial.Result.traverse`/`Iterator.map` helpers - legitimate `LambdaBuilder` uses);
  *     `mapPartialMaps`' `toKeyP.fulfilAsLambda2(toValueP)(...)` (two independently-traversed promises merged into one
  *     2-ary lambda) becomes a single `LambdaBuilder.of2[FromK, FromV]` whose `traverse` runs both derivations
  *     `parTuple`d (same both-run + error-aggregation semantics, same generated lambda),
  *   - the indexed partial per-item lambda (`partialP.fulfilAsLambda2(idxPromise)(...).tupled`) has no LambdaBuilder
  *     counterpart (`ExprPromise`'s free names could be re-bound under a wider lambda; builders cannot) - instead the
  *     built `InnerFrom => partial.Result[InnerTo]` lambda is bound to a val and applied inside a directly cross-quoted
  *     `((InnerFrom, Int)) => partial.Result[InnerTo]` lambda ([[indexedPartialLambda]]); the old code emitted
  *     `{ (from, idx) => body }.tupled` (Function2 + tupled wrapper), the new code emits
  *     `{ val inner = from => body; pair => inner(pair._1).unsealErrorPath.prependErrorPath(Index(pair._2)) }` -
  *     the same runtime semantics and per-element dispatch count, slightly different tree shape,
  *   - `mergeTotal`/`mergePartial` keep their old signatures but delegate to new expr-level
  *     `mergeTotalExprs`/`mergePartialExprs` (exposed for TransformMapToMapRule's lazy fallback handling),
  *   - `.log` becomes `.logInfo`, `upcastToExprOf` becomes `upcast`, `Type.Tuple2` becomes `ScalaType.Tuple2`,
  *     `Expr.Function2.instance` becomes `LambdaBuilder.of2().buildWith`, iterator `map/to/zipWithIndex/concat` expr
  *     ops come from the `ScalaIteratorExprOps` compat ops,
  *   - the collection-item `LambdaBuilder` uses `FreshName.FromType` naming where the old code used
  *     `NameGenerationStrategy.FromExpr(ctx.src)` (Hearth's `FreshName.FromExpr` needs an expr in context that
  *     `LambdaBuilder.of1` does not provide) - affects only generated fresh names.
  */
private[compiletime2] trait TransformIterableToIterableRuleModule {
  this: Derivation & TransformProductToProductRuleModule & hearth.MacroCommons =>

  import ChimneyType.Implicits.*, ScalaType.Implicits.*
  import TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformIterableToIterableRule extends Rule("IterableToIterable") {

    @scala.annotation.nowarn
    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      mapCollections[From, To] match {
        case Right(srcToResult) =>
          lazy val fallbackToResult = mapFallbackCollections[From, To]

          val merge = ctx match {
            case TransformationContext.ForTotal(_)             => mergeTotal[To](_, _)
            case TransformationContext.ForPartial(_, failFast) => mergePartial[To](failFast)(_, _)
          }

          (ctx.config.flags.collectionFallbackMerge match {
            case None                            => srcToResult
            case Some(dsls.SourceAppendFallback) =>
              fallbackToResult
                .foldLeft(srcToResult)(merge)
                .logInfo(s"Combined source collection with ${fallbackToResult.size} fallbacks (appended)")
            case Some(dsls.FallbackAppendSource) =>
              fallbackToResult.reverseIterator
                .foldRight(srcToResult)(merge)
                .logInfo(s"Combined source collection with ${fallbackToResult.size} fallbacks (prepended)")
          }).flatMap(DerivationResult.expanded)
        case Left(Some(reason)) => DerivationResult.attemptNextRuleBecause(reason)
        case Left(None)         => DerivationResult.attemptNextRule
      }

    private def mapCollections[From, To](implicit
        ctx: TransformationContext[From, To]
    ): Either[Option[String], DerivationResult[TransformationExpr[To]]] =
      (ctx, Type[From], Type[To]) match {
        case (
              TransformationContext.ForPartial(_, failFast),
              TotallyOrPartiallyBuildIterable(from2),
              TotallyOrPartiallyBuildIterable(to2)
            ) if from2.value.asMap.isDefined && to2.Underlying.isTuple =>
          val Some((fromK, fromV)) = from2.value.asMap: @unchecked
          val ScalaType.Tuple2(toK, toV) = to2.Underlying: @unchecked
          import fromK.Underlying as FromK, fromV.Underlying as FromV, toK.Underlying as ToK, toV.Underlying as ToV
          Right(
            DerivationResult.log(
              s"Resolved ${Type.prettyPrint[From]} (${from2.value}) as map type and ${Type.prettyPrint[To]} (${to2.value}) as iterable of tuple"
            ) >>
              mapPartialMaps[From, To, FromK, FromV, ToK, ToV](
                from2.value.asInstanceOf[TotallyOrPartiallyBuildIterable[From, (FromK, FromV)]],
                to2.value.asInstanceOf[TotallyOrPartiallyBuildIterable[To, (ToK, ToV)]],
                failFast
              )
          )
        case (TransformationContext.ForTotal(_), TotallyOrPartiallyBuildIterable(_), PartiallyBuildIterable(to2)) =>
          Left(
            Some(
              s"Only PartiallyBuildIterable available for ${Type.prettyPrint[To]} (${to2.value}), in total context"
            )
          )
        case (_, TotallyOrPartiallyBuildIterable(from2), TotallyOrPartiallyBuildIterable(to2)) =>
          import from2.{Underlying as InnerFrom, value as fromIterable},
            to2.{Underlying as InnerTo, value as toIterable}
          Right(
            DerivationResult.log(
              s"Resolved ${Type.prettyPrint[From]} (${from2.value}) and ${Type.prettyPrint[To]} (${to2.value}) as iterable types"
            ) >>
              mapIterables[From, To, InnerFrom, InnerTo](fromIterable, toIterable)
          )
        case _ => Left(None)
      }

    private def mapFallbackCollections[From, To](implicit
        ctx: TransformationContext[From, To]
    ): Vector[DerivationResult[TransformationExpr[To]]] =
      ctx.config.filterCurrentOverridesForFallbacks.view
        .map { case TransformerOverride.Fallback(fallback) =>
          import fallback.{Underlying as Fallback, value as fallbackExpr}
          implicit val iterableCtx: TransformationContext[Fallback, To] =
            ctx.updateFromTo[Fallback, To](fallbackExpr, updateFallbacks = _ => Vector.empty)(using Fallback, ctx.To)
          mapCollections[Fallback, To]
        }
        .collect { case Right(value) => value }
        .toVector

    private def mapPartialMaps[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        fromIterable: TotallyOrPartiallyBuildIterable[From, (FromK, FromV)],
        toIterable: TotallyOrPartiallyBuildIterable[To, (ToK, ToV)],
        failFast: Expr[Boolean]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[TransformationExpr[To]] =
      LambdaBuilder
        .of2[FromK, FromV](FreshName.FromPrefix("key"), FreshName.FromPrefix("value"))
        .traverse[DerivationResult, ((Expr[partial.Result[ToK]], Expr[FromK]), Expr[partial.Result[ToV]])] {
          case (key, value) =>
            val toKeyResult = DerivationResult
              .namedScope("Derive Map's key mapping") {
                useOverrideIfPresentOr("everyMapKey", ctx.config.filterCurrentOverridesForEveryMapKey) {
                  deriveRecursiveTransformationExpr[FromK, ToK](
                    key,
                    followFrom = Path(_.everyMapKey),
                    followTo = Path(_.everyMapKey),
                    updateFallbacks = _ => Vector.empty
                  )
                }
              }
              .map(_.ensurePartial -> key)
            val toValueResult = DerivationResult
              .namedScope("Derive Map's value mapping") {
                useOverrideIfPresentOr("everyMapValue", ctx.config.filterCurrentOverridesForEveryMapValue) {
                  deriveRecursiveTransformationExpr[FromV, ToV](
                    value,
                    followFrom = Path(_.everyMapValue),
                    followTo = Path(_.everyMapValue),
                    updateFallbacks = _ => Vector.empty
                  )
                }
              }
              .map(_.ensurePartial)
            toKeyResult.parTuple(toValueResult)
        }
        .flatMap { builder =>
          def partialResultTraverse[ToOrPartialTo: Type](
              factory: Expr[Factory[(ToK, ToV), ToOrPartialTo]]
          ): Expr[partial.Result[ToOrPartialTo]] =
            // We're constructing:
            // '{ partial.Result.traverse(
            //   ${ src }.iterator,
            //   { case (fromKey, fromValue) =>
            //     partial.Result.product(
            //       ${ derivedToKey }.unsealErrorPath.prependErrorPath(partial.PathElement.MapKey(fromKey)))
            //       ${ derivedToValue }.unsealErrorPath.prependErrorPath(partial.PathElement.MapValue(fromKey))),
            //   },
            //   ${ failFast }
            // )(${ factory }) }
            ChimneyExpr.PartialResult
              .traverse[ToOrPartialTo, (FromK, FromV), (ToK, ToV)](
                fromIterable.iterator(ctx.src),
                builder.buildWith { case ((keyResult, key), valueResult) =>
                  ChimneyExpr.PartialResult.product(
                    keyResult.unsealErrorPath.prependErrorPath(
                      ChimneyExpr.PathElement.MapKey(key.upcast[Any]).upcast[partial.PathElement]
                    ),
                    valueResult.unsealErrorPath.prependErrorPath(
                      ChimneyExpr.PathElement.MapValue(key.upcast[Any]).upcast[partial.PathElement]
                    ),
                    failFast
                  )
                }.tupled,
                failFast,
                factory
              )

          toIterable.factory match {
            case Left(totalFactory)    => DerivationResult.partialExpr(partialResultTraverse(totalFactory))
            case Right(partialFactory) => DerivationResult.partialExpr(partialResultTraverse(partialFactory).flatten)
          }
        }

    private def mapIterables[From, To, InnerFrom: Type, InnerTo: Type](
        fromIterable: TotallyOrPartiallyBuildIterable[From, InnerFrom],
        toIterable: TotallyOrPartiallyBuildIterable[To, InnerTo]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[TransformationExpr[To]] =
      LambdaBuilder
        .of1[InnerFrom]()
        .traverse[DerivationResult, TransformationExpr[InnerTo]] { (newFromSrc: Expr[InnerFrom]) =>
          useOverrideIfPresentOr("everyItem", ctx.config.filterCurrentOverridesForEveryItem) {
            DerivationResult
              .namedScope("Derive collection's item mapping") {
                deriveRecursiveTransformationExpr[InnerFrom, InnerTo](
                  newFromSrc,
                  followFrom = Path(_.everyItem),
                  followTo = Path(_.everyItem),
                  updateFallbacks = _ => Vector.empty
                )
              }
          }
        }
        .flatMap { (to2P: LambdaBuilder[InnerFrom => *, TransformationExpr[InnerTo]]) =>
          to2P.foldTransformationExpr { (totalP: LambdaBuilder[InnerFrom => *, Expr[InnerTo]]) =>
            // TODO: restore .map implementation
            if (Type[InnerFrom] =:= Type[InnerTo] && ctx.config.areOverridesEmpty) {
              def srcToFactory[ToOrPartialTo: Type](
                  factory: Expr[Factory[InnerTo, ToOrPartialTo]]
              ): Expr[ToOrPartialTo] =
                // We're constructing:
                // '{ ${ src }.to(Factory[$InnerTo, $ToOrPartialTo]) }
                fromIterable.to[ToOrPartialTo](ctx.src, factory.upcast[Factory[InnerFrom, ToOrPartialTo]])

              toIterable.factory match {
                case Left(totalFactory)    => DerivationResult.totalExpr(srcToFactory(totalFactory))
                case Right(partialFactory) => DerivationResult.partialExpr(srcToFactory(partialFactory))
              }
            } else {
              def srcIteratorMapTo[ToOrPartialTo: Type](
                  factory: Expr[Factory[InnerTo, ToOrPartialTo]]
              ): Expr[ToOrPartialTo] =
                // We're constructing
                // '{ ${ src }.iterator.map(from2 => ${ derivedInnerTo }).to(Factory[$InnerTo, $ToOrPartialTo]) }
                fromIterable.iterator(ctx.src).map(totalP.build[InnerTo]).to[ToOrPartialTo](factory)

              toIterable.factory match {
                case Left(totalFactory)    => DerivationResult.totalExpr(srcIteratorMapTo(totalFactory))
                case Right(partialFactory) => DerivationResult.partialExpr(srcIteratorMapTo(partialFactory))
              }
            }
          } { (partialP: LambdaBuilder[InnerFrom => *, Expr[partial.Result[InnerTo]]]) =>
            ctx match {
              case TransformationContext.ForPartial(src, failFast) =>
                def partialResultTraverse[ToOrPartialTo: Type](
                    factory: Expr[Factory[InnerTo, ToOrPartialTo]]
                ): Expr[partial.Result[ToOrPartialTo]] =
                  // We're constructing:
                  // '{ partial.Result.traverse[To, ($InnerFrom, Int), $InnerTo](
                  //   ${ src }.iterator.zipWithIndex,
                  //   { pair =>
                  //     ${ innerLambda }(pair._1).unsealErrorPath.prependErrorPath(partial.PathElement.Index(pair._2))
                  //   },
                  //   ${ failFast }
                  // )(${ factory }) }
                  ChimneyExpr.PartialResult.traverse[ToOrPartialTo, (InnerFrom, Int), InnerTo](
                    fromIterable.iterator(src).zipWithIndex,
                    indexedPartialLambda[InnerFrom, InnerTo](partialP.build[partial.Result[InnerTo]]),
                    failFast,
                    factory
                  )

                toIterable.factory match {
                  case Left(totalTransformer) =>
                    DerivationResult.partialExpr(partialResultTraverse(totalTransformer))
                  case Right(partialTransformer) =>
                    DerivationResult.partialExpr(partialResultTraverse(partialTransformer).flatten)
                }
              case TransformationContext.ForTotal(_) =>
                DerivationResult.assertionError("Derived Partial Expr for Total Context")
            }
          }
        }

    /** Wraps the derived per-item partial lambda into the `((InnerFrom, Int)) => partial.Result[InnerTo]` shape that
      * `partial.Result.traverse` over `iterator.zipWithIndex` expects (see the trait's ScalaDoc for how this differs
      * from the old `fulfilAsLambda2(idxPromise).tupled`).
      */
    private def indexedPartialLambda[InnerFrom: Type, InnerTo: Type](
        inner: Expr[InnerFrom => partial.Result[InnerTo]]
    ): Expr[((InnerFrom, Int)) => partial.Result[InnerTo]] =
      ValDefs.createVal(inner, FreshName.FromPrefix("inner")).use { innerRef =>
        Expr.quote { (pair: (InnerFrom, Int)) =>
          Expr
            .splice(innerRef)
            .apply(pair._1)
            .unsealErrorPath
            .prependErrorPath(io.scalaland.chimney.partial.PathElement.Index(pair._2))
        }
      }

    // Exposed for TransformMapToMapRuleModule

    def mergeTotal[To: Type](
        result1: DerivationResult[TransformationExpr[To]],
        result2: DerivationResult[TransformationExpr[To]]
    ): DerivationResult[TransformationExpr[To]] = result1.map2(result2)(mergeTotalExprs[To])

    def mergeTotalExprs[To: Type](
        texpr1: TransformationExpr[To],
        texpr2: TransformationExpr[To]
    ): TransformationExpr[To] = {
      val TotallyBuildIterable(to2) = Type[To]: @unchecked
      import to2.{Underlying as InnerTo, value as buildIterable}
      TransformationExpr.fromTotal(
        buildIterable
          .iterator(texpr1.ensureTotal)
          .concat(buildIterable.iterator(texpr2.ensureTotal))
          .to(buildIterable.totalFactory)
      )
    }

    def mergePartial[To: Type](failFast: Expr[Boolean])(
        result1: DerivationResult[TransformationExpr[To]],
        result2: DerivationResult[TransformationExpr[To]]
    ): DerivationResult[TransformationExpr[To]] =
      result1.map2(result2)(mergePartialExprs[To](failFast))

    def mergePartialExprs[To: Type](failFast: Expr[Boolean])(
        texpr1: TransformationExpr[To],
        texpr2: TransformationExpr[To]
    ): TransformationExpr[To] = {
      val TotallyOrPartiallyBuildIterable(to2) = Type[To]: @unchecked
      import to2.{Underlying as InnerTo, value as buildIterable}

      val iterators: TransformationExpr[Iterator[InnerTo]] = (texpr1, texpr2) match {
        case (TransformationExpr.TotalExpr(expr1), TransformationExpr.TotalExpr(expr2)) =>
          TransformationExpr.fromTotal[Iterator[InnerTo]](
            buildIterable.iterator(expr1).concat(buildIterable.iterator(expr2))
          )
        case _ =>
          TransformationExpr.fromPartial[Iterator[InnerTo]](
            texpr1.ensurePartial.map2(texpr2.ensurePartial, failFast)(
              LambdaBuilder.of2[To, To]().buildWith { case (expr1, expr2) =>
                buildIterable.iterator(expr1).concat(buildIterable.iterator(expr2))
              }
            )
          )
      }
      iterators.flatMap { expr =>
        buildIterable.factory match {
          case Left(totalFactor)     => TransformationExpr.fromTotal(expr.to(totalFactor))
          case Right(partialFactory) => TransformationExpr.fromPartial(expr.to(partialFactory))
        }
      }
    }
  }
}
