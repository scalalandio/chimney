package io.scalaland.chimney.internal.compiletime2.derivation.transformer.rules

import hearth.fp.syntax.*
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.compiletime2.DerivationResult
import io.scalaland.chimney.internal.compiletime2.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformMapToMapRuleModule`.
  *
  * Differences vs the old version:
  *   - the old code EAGERLY ran the delegated-to-IterableToIterable expansion (`result.toEither`) to decide whether
  *     MapToMap should pass on to the next rule; `DerivationResult` is now lazy (MIO), so `mapMaps` returns
  *     `DerivationResult[Either[Option[String], TransformationExpr[To]]]` - `Left(reason)` is the
  *     decided-at-derivation-time "attempt next rule" of the delegated expansion; `expand` translates a `Left` of the
  *     main result into `AttemptNextRule` and fallbacks that delegated-and-yielded are skipped during the merge fold
  *     (`.map(_.toOption)` + `Option.fold`) exactly like the old synchronous `collect { case Right(...) }` did,
  *   - the merge fold therefore uses the expr-level `mergeTotalExprs`/`mergePartialExprs` (exposed by the
  *     IterableToIterable module) inside `map2` instead of the old `DerivationResult`-level `mergeTotal`/`mergePartial`
  *     (same sequential `map2` evaluation and error semantics),
  *   - `ExprPromise` pairs + `fulfilAsLambda2(...).tupled` become a single `LambdaBuilder.of2` whose `traverse` runs
  *     both key/value derivations `parTuple`d (same both-run + error-aggregation semantics, same generated lambda); the
  *     tuple+index variant (`Expr.Function2.instance[(FromK, FromV), Int, ...]` with `fulfilAsVal` re-binding) becomes
  *     `LambdaBuilder.of2[(FromK, FromV), Int]` with `ValDefs.createVal(...).traverse` for the `val key = pair._1` /
  *     `val value = pair._2` bindings (same generated code),
  *   - the `displayMacrosLogging` debug `println` now prints a lazy `DerivationResult` (opaque `MIO` toString) instead
  *     of the old computed value,
  *   - `.log` becomes `.logInfo`, `upcastToExprOf` becomes `upcast`, `Type.Tuple2` becomes `ScalaType.Tuple2`,
  *     `Expr.String` becomes `Expr(...)`, `Expr.block`/`Expr.suppressUnused` become `blockExpr`/`Expr.suppressUnused`.
  */
private[compiletime2] trait TransformMapToMapRuleModule {
  this: Derivation & TransformIterableToIterableRuleModule & TransformProductToProductRuleModule &
    hearth.MacroCommons =>

  import ChimneyType.Implicits.*, ScalaType.Implicits.*
  import TransformIterableToIterableRule.{mergePartialExprs, mergeTotalExprs}
  import TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformMapToMapRule extends Rule("MapToMap") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      mapMaps[From, To] match {
        case Right(srcToResult) =>
          srcToResult.flatMap {
            case Left(reason) =>
              // The delegated IterableToIterable expansion decided to pass on to the next rule - so does MapToMap.
              DerivationResult.pure(Rule.ExpansionResult.AttemptNextRule(reason))
            case Right(srcTo) =>
              lazy val fallbackToResult: Vector[DerivationResult[Option[TransformationExpr[To]]]] =
                mapFallbackMaps[From, To].map(_.map(_.toOption))

              val merge: (TransformationExpr[To], TransformationExpr[To]) => TransformationExpr[To] = ctx match {
                case TransformationContext.ForTotal(_)             => mergeTotalExprs[To](_, _)
                case TransformationContext.ForPartial(_, failFast) => mergePartialExprs[To](failFast)(_, _)
              }

              (ctx.config.flags.collectionFallbackMerge match {
                case None                            => DerivationResult.pure(srcTo)
                case Some(dsls.SourceAppendFallback) =>
                  fallbackToResult
                    .foldLeft(DerivationResult.pure(srcTo)) { (acc, fallbackOpt) =>
                      acc.map2(fallbackOpt)((a, opt) => opt.fold(a)(b => merge(a, b)))
                    }
                    .logInfo(s"Combined source Map with ${fallbackToResult.size} fallbacks (appended)")
                case Some(dsls.FallbackAppendSource) =>
                  fallbackToResult.reverseIterator
                    .foldRight(DerivationResult.pure(srcTo)) { (fallbackOpt, acc) =>
                      fallbackOpt.map2(acc)((opt, a) => opt.fold(a)(b => merge(b, a)))
                    }
                    .logInfo(s"Combined source Map with ${fallbackToResult.size} fallbacks (prepended)")
              }).flatMap(DerivationResult.expanded)
          }
        case Left(Some(reason)) => DerivationResult.attemptNextRuleBecause(reason)
        case Left(None)         => DerivationResult.attemptNextRule
      }

    private def mapMaps[From, To](implicit
        ctx: TransformationContext[From, To]
    ): Either[Option[String], DerivationResult[Either[Option[String], TransformationExpr[To]]]] =
      (Type[From], Type[To]) match {
        case (TotallyOrPartiallyBuildMap(fromMap), TotallyOrPartiallyBuildMap(toMap)) =>
          import fromMap.{Key as FromK, Value as FromV}, toMap.{Key as ToK, Value as ToV}
          ctx match {
            case TransformationContext.ForTotal(_) if !ctx.config.areOverridesEmpty =>
              Right(
                mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](fromMap.iterator(ctx.src), toMap.factory)
                  .map(Right(_))
              )
            case TransformationContext.ForPartial(_, failFast) =>
              Right(
                mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
                  fromMap.iterator(ctx.src),
                  toMap.factory,
                  failFast,
                  isConversionFromMap = true
                ).map(Right(_))
              )
            case _ =>
              Right(
                DerivationResult
                  .namedScope(
                    "MapToMap matched in the context of total transformation without overrides - delegating to IterableToIterable (fallbacks handled in MapToMap)"
                  ) {
                    // Removes fallbacks, as are they are handled here (otherwise they would be appended/prepended twice)
                    TransformIterableToIterableRule.expand(
                      ctx.updateFromTo[From, To](ctx.src, updateFallbacks = _ => Vector.empty)(using ctx.From, ctx.To)
                    )
                  }
                  .map {
                    case Rule.ExpansionResult.Expanded(texpr) =>
                      Right(texpr.asInstanceOf[TransformationExpr[To]])
                    case Rule.ExpansionResult.AttemptNextRule(reason) =>
                      Left(reason)
                  }
              )
          }
        case (TotallyOrPartiallyBuildIterable(from2), TotallyOrPartiallyBuildMap(toMap))
            if !ctx.config.areOverridesEmpty && from2.Underlying.isTuple =>
          val ScalaType.Tuple2(fromK, fromV) = from2.Underlying: @unchecked
          import from2.{Underlying as InnerFrom, value as fromIterable}, fromK.Underlying as FromK,
            fromV.Underlying as FromV, toMap.{Key as ToK, Value as ToV}
          ctx match {
            case TransformationContext.ForTotal(_) =>
              Right(
                mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](
                  fromIterable
                    .iterator(ctx.src)
                    .upcast[Iterator[(FromK, FromV)]], // needed because iterable, not map
                  toMap.factory
                ).map(Right(_))
              )
            case TransformationContext.ForPartial(_, failFast) =>
              Right(
                mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
                  fromIterable.iterator(ctx.src).upcast[Iterator[(FromK, FromV)]],
                  toMap.factory,
                  failFast,
                  isConversionFromMap = false
                ).map(Right(_))
              )
          }

        case _ => Left(None)
      }

    private def mapFallbackMaps[From, To](implicit
        ctx: TransformationContext[From, To]
    ): Vector[DerivationResult[Either[Option[String], TransformationExpr[To]]]] =
      ctx.config.filterCurrentOverridesForFallbacks.view
        .map { case TransformerOverride.Fallback(fallback) =>
          import fallback.{Underlying as Fallback, value as fallbackExpr}
          implicit val iterableCtx: TransformationContext[Fallback, To] =
            ctx.updateFromTo[Fallback, To](fallbackExpr, updateFallbacks = _ => Vector.empty)(using Fallback, ctx.To)
          val x = mapMaps[Fallback, To]
          if (ctx.config.flags.displayMacrosLogging) {
            println(s"Fallbacks: ${ctx.config.filterCurrentOverridesForFallbacks}\nHandled as: $x\n")
          }
          x
        }
        .collect { case Right(value) => value }
        .toVector

    private def deriveKeyMapping[From, To, FromK: Type, ToK: Type](
        key: Expr[FromK]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[TransformationExpr[ToK]] =
      DerivationResult.namedScope("Derive Map's key mapping") {
        useOverrideIfPresentOr("everyMapKey", ctx.config.filterCurrentOverridesForEveryMapKey) {
          deriveRecursiveTransformationExpr[FromK, ToK](
            key,
            followFrom = Path(_.everyMapKey),
            followTo = Path(_.everyMapKey),
            updateFallbacks = _ => Vector.empty
          )
        }
      }

    private def deriveValueMapping[From, To, FromV: Type, ToV: Type](
        value: Expr[FromV]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[TransformationExpr[ToV]] =
      DerivationResult.namedScope("Derive Map's value mapping") {
        useOverrideIfPresentOr("everyMapValue", ctx.config.filterCurrentOverridesForEveryMapValue) {
          deriveRecursiveTransformationExpr[FromV, ToV](
            value,
            followFrom = Path(_.everyMapValue),
            followTo = Path(_.everyMapValue),
            updateFallbacks = _ => Vector.empty
          )
        }
      }

    private def mapMapForTotalTransformers[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        iterator: Expr[Iterator[(FromK, FromV)]],
        factoryEither: Either[Expr[Factory[(ToK, ToV), To]], Expr[Factory[(ToK, ToV), partial.Result[To]]]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[TransformationExpr[To]] =
      LambdaBuilder
        .of2[FromK, FromV](FreshName.FromPrefix("key"), FreshName.FromPrefix("value"))
        .traverse[DerivationResult, (Expr[ToK], Expr[ToV])] { case (key, value) =>
          deriveKeyMapping[From, To, FromK, ToK](key)
            .map(_.ensureTotal)
            .parTuple(deriveValueMapping[From, To, FromV, ToV](value).map(_.ensureTotal))
        }
        .flatMap { builder =>
          def iteratorMapTo[ToOrPartialTo: Type](
              factory: Expr[Factory[(ToK, ToV), ToOrPartialTo]]
          ): Expr[ToOrPartialTo] =
            // We're constructing:
            // '{ ${ iterator }.map{ case (key, value) =>
            //    (${ resultToKey }, ${ resultToValue })
            //    }
            // }.to(${ factory }) }
            iterator
              .map[(ToK, ToV)](
                builder.buildWith { case (toKeyResult, toValueResult) =>
                  ScalaExpr.Tuple2(toKeyResult, toValueResult)
                }.tupled
              )
              .to(factory)

          factoryEither match {
            case Left(totalFactory)    => DerivationResult.totalExpr(iteratorMapTo(totalFactory))
            case Right(partialFactory) => DerivationResult.partialExpr(iteratorMapTo(partialFactory))
          }
        }

    private def mapMapForPartialTransformers[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        iterator: Expr[Iterator[(FromK, FromV)]],
        factoryEither: Either[Expr[Factory[(ToK, ToV), To]], Expr[Factory[(ToK, ToV), partial.Result[To]]]],
        failFast: Expr[Boolean],
        isConversionFromMap: Boolean // or from any sequence of tuples
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[TransformationExpr[To]] = {
      if (isConversionFromMap) {
        // We're constructing:
        // '{ partial.Result.traverse[To, ($FromK, $FromV), ($ToK, $ToV)](
        //   ${ iterator },
        //   { case (key, value) =>
        //     val _ = key
        //     val _ = value
        //     partial.Result.product(
        //       ${ resultToKey }.unsealErrorPath.prependErrorPath(partial.PathElement.MapKey(key)),
        //       ${ resultToValue }.unsealErrorPath.prependErrorPath(partial.PathElement.MapValue(key),
        //       ${ failFast }
        //     )
        //   },
        //   ${ failFast }
        // )(${ factory })
        LambdaBuilder
          .of2[FromK, FromV](FreshName.FromPrefix("key"), FreshName.FromPrefix("value"))
          .traverse[
            DerivationResult,
            ((Expr[partial.Result[ToK]], Expr[FromK]), (Expr[partial.Result[ToV]], Expr[FromV]))
          ] { case (key, value) =>
            deriveKeyMapping[From, To, FromK, ToK](key)
              .map(_.ensurePartial -> key)
              .parTuple(deriveValueMapping[From, To, FromV, ToV](value).map(_.ensurePartial -> value))
          }
          .flatMap { builder =>
            val lambda = builder.buildWith { case ((keyResult, key), (valueResult, value)) =>
              blockExpr(
                List(
                  Expr.suppressUnused(key),
                  Expr.suppressUnused(value)
                ),
                ChimneyExpr.PartialResult.product(
                  keyResult.unsealErrorPath.prependErrorPath(
                    ChimneyExpr.PathElement.MapKey(key.upcast[Any]).upcast[partial.PathElement]
                  ),
                  valueResult.unsealErrorPath.prependErrorPath(
                    ChimneyExpr.PathElement.MapValue(key.upcast[Any]).upcast[partial.PathElement]
                  ),
                  failFast
                )
              )
            }.tupled

            def partialResultTraverse[ToOrPartialTo: Type](
                factory: Expr[Factory[(ToK, ToV), ToOrPartialTo]]
            ): Expr[partial.Result[ToOrPartialTo]] =
              ChimneyExpr.PartialResult.traverse[ToOrPartialTo, (FromK, FromV), (ToK, ToV)](
                iterator,
                lambda,
                failFast,
                factory
              )

            factoryEither match {
              case Left(totalFactory)   => DerivationResult.partialExpr(partialResultTraverse(totalFactory))
              case Right(partialResult) => DerivationResult.partialExpr(partialResultTraverse(partialResult).flatten)
            }
          }
      } else {
        // We're constructing:
        // '{ partial.Result.traverse[To, (($FromK, $FromV), Int), ($ToK, $ToV)](
        //   ${ iterator }.zipWithIndex,
        //   { case (pair, idx) =>
        //     partial.Result.product(
        //       {
        //         val key = pair._1
        //         ${ resultToKey }
        //           .unsealErrorPath
        //           .prependErrorPath(partial.PathElement.Accessor("_1"))}
        //           .prependErrorPath(partial.PathElement.Index(idx))
        //       },
        //       {
        //         val value = pair._2
        //         ${ resultToValue }
        //           .unsealErrorPath
        //           .prependErrorPath(partial.PathElement.Accessor("_2"))}
        //           .prependErrorPath(partial.PathElement.Index(idx))
        //       },
        //       ${ failFast }
        //     )
        //   },
        //   ${ failFast }
        // )(${ factory })
        LambdaBuilder
          .of2[(FromK, FromV), Int](FreshName.FromPrefix("pair"), FreshName.FromPrefix("idx"))
          .traverse[DerivationResult, Expr[partial.Result[(ToK, ToV)]]] { case (pairExpr, indexExpr) =>
            val pairGetters = ProductType.parseExtraction[(FromK, FromV)].get.extraction
            val _1 = pairGetters("_1")
            val _2 = pairGetters("_2")
            import _1.{Underlying as From_1, value as getter_1}, _2.{Underlying as From_2, value as getter_2}
            val keyResultVal = ValDefs
              .createVal(getter_1.get(pairExpr).upcast[FromK], FreshName.FromPrefix("key"))
              .traverse[DerivationResult, (Expr[partial.Result[ToK]], Expr[FromK])] { key =>
                deriveKeyMapping[From, To, FromK, ToK](key).map(_.ensurePartial -> key)
              }
            val valueResultVal = ValDefs
              .createVal(getter_2.get(pairExpr).upcast[FromV], FreshName.FromPrefix("value"))
              .traverse[DerivationResult, (Expr[partial.Result[ToV]], Expr[FromV])] { value =>
                deriveValueMapping[From, To, FromV, ToV](value).map(_.ensurePartial -> value)
              }
            keyResultVal.parTuple(valueResultVal).map { case (keyVD, valueVD) =>
              ChimneyExpr.PartialResult.product(
                keyVD
                  .use { case (keyResult, key) => blockExpr(List(Expr.suppressUnused(key)), keyResult) }
                  .unsealErrorPath
                  .prependErrorPath(
                    ChimneyExpr.PathElement.Accessor(Expr("_1")).upcast[partial.PathElement]
                  )
                  .prependErrorPath(
                    ChimneyExpr.PathElement.Index(indexExpr).upcast[partial.PathElement]
                  ),
                valueVD
                  .use { case (valueResult, value) => blockExpr(List(Expr.suppressUnused(value)), valueResult) }
                  .unsealErrorPath
                  .prependErrorPath(
                    ChimneyExpr.PathElement.Accessor(Expr("_2")).upcast[partial.PathElement]
                  )
                  .prependErrorPath(
                    ChimneyExpr.PathElement.Index(indexExpr).upcast[partial.PathElement]
                  ),
                failFast
              )
            }
          }
          .flatMap { builder =>
            val lambda = builder.build[partial.Result[(ToK, ToV)]].tupled

            def partialResultTraverse[ToOrPartialTo: Type](
                factory: Expr[Factory[(ToK, ToV), ToOrPartialTo]]
            ): Expr[partial.Result[ToOrPartialTo]] =
              ChimneyExpr.PartialResult.traverse[ToOrPartialTo, ((FromK, FromV), Int), (ToK, ToV)](
                iterator.zipWithIndex,
                lambda,
                failFast,
                factory
              )

            factoryEither match {
              case Left(totalFactory)   => DerivationResult.partialExpr(partialResultTraverse(totalFactory))
              case Right(partialResult) => DerivationResult.partialExpr(partialResultTraverse(partialResult).flatten)
            }
          }
      }
    }

    private trait TotallyOrPartiallyBuildMap[Collection] {

      type Key
      implicit val Key: Type[Key]

      type Value
      implicit val Value: Type[Value]

      def factory
          : Either[Expr[Factory[(Key, Value), Collection]], Expr[Factory[(Key, Value), partial.Result[Collection]]]]

      def iterator(collection: Expr[Collection]): Expr[Iterator[(Key, Value)]]

      def to[Collection2: Type](
          collection: Expr[Collection],
          factory: Expr[Factory[(Key, Value), Collection2]]
      ): Expr[Collection2]
    }
    private object TotallyOrPartiallyBuildMap {

      def parse[M](implicit M: Type[M]): Option[TotallyOrPartiallyBuildMap[M]] =
        TotallyOrPartiallyBuildIterable.parse[M].flatMap(it => it.value.asMap.map(it -> _)).map {
          case (it, (key, value)) =>
            import it.Underlying as Inner, key.Underlying as Key0, value.Underlying as Value0
            new TotallyOrPartiallyBuildMap[M] {

              type Key = Key0
              val Key: Type[Key] = Key0

              type Value = Value0
              val Value: Type[Value] = Value0

              def factory: Either[Expr[Factory[(Key, Value), M]], Expr[Factory[(Key, Value), partial.Result[M]]]] =
                it.value.factory match {
                  case Left(totalFactory) =>
                    Left(totalFactory.upcast[Factory[(Key, Value), M]])
                  case Right(partialFactory) =>
                    Right(partialFactory.upcast[Factory[(Key, Value), partial.Result[M]]])
                }

              def iterator(collection: Expr[M]): Expr[Iterator[(Key, Value)]] =
                it.value.iterator(collection).upcast[Iterator[(Key0, Value0)]]

              def to[Collection2: Type](
                  collection: Expr[M],
                  factory: Expr[Factory[(Key, Value), Collection2]]
              ): Expr[Collection2] = it.value.to(collection, factory.upcast[Factory[Inner, Collection2]])
            }
        }
      final def unapply[M](M: Type[M]): Option[TotallyOrPartiallyBuildMap[M]] = parse(using M)
    }
  }
}
