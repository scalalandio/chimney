package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

private[compiletime] trait TransformMapToMapRuleModule {
  this: Derivation & TransformIterableToIterableRuleModule & TransformProductToProductRuleModule =>

  import Type.Implicits.*, ChimneyType.Implicits.*
  import TransformIterableToIterableRule.{mergePartial, mergeTotal}
  import TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformMapToMapRule extends Rule("MapToMap") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      mapMaps[From, To] match {
        case Right(srcToResult) =>
          lazy val fallbackToResult = mapFallbackMaps[From, To]

          val merge = ctx match {
            case TransformationContext.ForTotal(_)             => mergeTotal[To](_, _)
            case TransformationContext.ForPartial(_, failFast) => mergePartial[To](failFast)(_, _)
          }

          (ctx.config.flags.collectionFallbackMerge match {
            case None                            => srcToResult
            case Some(dsls.SourceAppendFallback) =>
              fallbackToResult
                .foldLeft(srcToResult)(merge)
                .log(s"Combined source Map with ${fallbackToResult.size} fallbacks (appended)")
            case Some(dsls.FallbackAppendSource) =>
              fallbackToResult.reverseIterator
                .foldRight(srcToResult)(merge)
                .log(s"Combined source Map with ${fallbackToResult.size} fallbacks (prepended)")
          }).flatMap(DerivationResult.expanded)
        case Left(Some(reason)) => DerivationResult.attemptNextRuleBecause(reason)
        case Left(None)         => DerivationResult.attemptNextRule
      }

    private def mapMaps[From, To](implicit
        ctx: TransformationContext[From, To]
    ): Either[Option[String], DerivationResult[TransformationExpr[To]]] = (Type[From], Type[To]) match {
      case (TotallyOrPartiallyBuildMap(fromMap), TotallyOrPartiallyBuildMap(toMap)) =>
        import fromMap.{Key as FromK, Value as FromV}, toMap.{Key as ToK, Value as ToV}
        ctx match {
          case TransformationContext.ForTotal(_) if !ctx.config.areOverridesEmpty =>
            Right(
              mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](fromMap.iterator(ctx.src), toMap.factory)
            )
          case TransformationContext.ForPartial(_, failFast) =>
            Right(
              mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
                fromMap.iterator(ctx.src),
                toMap.factory,
                failFast,
                isConversionFromMap = true
              )
            )
          case _ =>
            val result = DerivationResult
              .namedScope(
                "MapToMap matched in the context of total transformation without overrides - delegating to IterableToIterable (fallbacks handled in MapToMap)"
              ) {
                // Removes fallbacks, as are they are handled here (otherwise they would be appended/prepended twice)
                TransformIterableToIterableRule.expand(
                  ctx.updateFromTo(ctx.src, updateFallbacks = _ => Vector.empty)(ctx.From, ctx.To)
                )
              }
            result.toEither match {
              case Left(errors)                                        => Right(result >> DerivationResult.fail(errors))
              case Right(Rule.ExpansionResult.AttemptNextRule(reason)) => Left(reason)
              case Right(Rule.ExpansionResult.Expanded(texpr))         =>
                Right(result >> DerivationResult.pure(texpr.asInstanceOf[TransformationExpr[To]]))
            }
        }
      case (TotallyOrPartiallyBuildIterable(from2), TotallyOrPartiallyBuildMap(toMap))
          if !ctx.config.areOverridesEmpty && from2.Underlying.isTuple =>
        val Type.Tuple2(fromK, fromV) = from2.Underlying: @unchecked
        import from2.{Underlying as InnerFrom, value as fromIterable}, fromK.Underlying as FromK,
          fromV.Underlying as FromV, toMap.{Key as ToK, Value as ToV}
        ctx match {
          case TransformationContext.ForTotal(_) =>
            Right(
              mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](
                fromIterable
                  .iterator(ctx.src)
                  .upcastToExprOf[Iterator[(FromK, FromV)]], // needed because iterable, not map
                toMap.factory
              )
            )
          case TransformationContext.ForPartial(_, failFast) =>
            Right(
              mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
                fromIterable.iterator(ctx.src).upcastToExprOf[Iterator[(FromK, FromV)]],
                toMap.factory,
                failFast,
                isConversionFromMap = false
              )
            )
        }

      case _ => Left(None)
    }

    private def mapFallbackMaps[From, To](implicit
        ctx: TransformationContext[From, To]
    ): Vector[DerivationResult[TransformationExpr[To]]] =
      ctx.config.filterCurrentOverridesForFallbacks.view
        .map { case TransformerOverride.Fallback(fallback) =>
          import fallback.{Underlying as Fallback, value as fallbackExpr}
          implicit val iterableCtx: TransformationContext[Fallback, To] =
            ctx.updateFromTo[Fallback, To](fallbackExpr, updateFallbacks = _ => Vector.empty)(Fallback, ctx.To)
          val x = mapMaps[Fallback, To]
          if (ctx.config.flags.displayMacrosLogging) {
            println(s"Fallbacks: ${ctx.config.filterCurrentOverridesForFallbacks}\nHandled as: $x\n")
          }
          x
        }
        .collect { case Right(value) => value }
        .toVector

    private def mapMapForTotalTransformers[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        iterator: Expr[Iterator[(FromK, FromV)]],
        factoryEither: Either[Expr[Factory[(ToK, ToV), To]], Expr[Factory[(ToK, ToV), partial.Result[To]]]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[TransformationExpr[To]] = {
      val toKeyResult = ExprPromise
        .promise[FromK](ExprPromise.NameGenerationStrategy.FromPrefix("key"))
        .traverse { key =>
          DerivationResult
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
            .map(_.ensureTotal)
        }
      val toValueResult = ExprPromise
        .promise[FromV](ExprPromise.NameGenerationStrategy.FromPrefix("value"))
        .traverse { value =>
          DerivationResult
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
            .map(_.ensureTotal)
        }

      toKeyResult.parTuple(toValueResult).flatMap { case (toKeyP, toValueP) =>
        def iteratorMapTo[ToOrPartialTo: Type](factory: Expr[Factory[(ToK, ToV), ToOrPartialTo]]): Expr[ToOrPartialTo] =
          // We're constructing:
          // '{ ${ iterator }.map{ case (key, value) =>
          //    (${ resultToKey }, ${ resultToValue })
          //    }
          // }.to(${ factory }) }
          iterator
            .map[(ToK, ToV)](
              toKeyP
                .fulfilAsLambda2(toValueP) { (toKeyResult, toValueResult) =>
                  Expr.Tuple2(toKeyResult, toValueResult)
                }
                .tupled
            )
            .to(factory)

        factoryEither match {
          case Left(totalFactory)    => DerivationResult.totalExpr(iteratorMapTo(totalFactory))
          case Right(partialFactory) => DerivationResult.partialExpr(iteratorMapTo(partialFactory))
        }
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
      val toKeyResult = ExprPromise
        .promise[FromK](ExprPromise.NameGenerationStrategy.FromPrefix("key"))
        .traverse { key =>
          DerivationResult
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
        }
      val toValueResult = ExprPromise
        .promise[FromV](ExprPromise.NameGenerationStrategy.FromPrefix("value"))
        .traverse { value =>
          DerivationResult
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
            .map(_.ensurePartial -> value)
        }

      toKeyResult.parTuple(toValueResult).flatMap { case (toKeyP, toValueP) =>
        def partialResultTraverse[ToOrPartialTo: Type](
            factory: Expr[Factory[(ToK, ToV), ToOrPartialTo]]
        ): Expr[partial.Result[ToOrPartialTo]] =
          if (isConversionFromMap)
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
            ChimneyExpr.PartialResult
              .traverse[ToOrPartialTo, (FromK, FromV), (ToK, ToV)](
                iterator,
                toKeyP
                  .fulfilAsLambda2(toValueP) { case ((keyResult, key), (valueResult, value)) =>
                    Expr.block(
                      List(
                        Expr.suppressUnused(key),
                        Expr.suppressUnused(value)
                      ),
                      ChimneyExpr.PartialResult.product(
                        keyResult.unsealErrorPath.prependErrorPath(
                          ChimneyExpr.PathElement.MapKey(key.upcastToExprOf[Any]).upcastToExprOf[partial.PathElement]
                        ),
                        valueResult.unsealErrorPath.prependErrorPath(
                          ChimneyExpr.PathElement.MapValue(key.upcastToExprOf[Any]).upcastToExprOf[partial.PathElement]
                        ),
                        failFast
                      )
                    )
                  }
                  .tupled,
                failFast,
                factory
              )
          else
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
            ChimneyExpr.PartialResult
              .traverse[ToOrPartialTo, ((FromK, FromV), Int), (ToK, ToV)](
                iterator.zipWithIndex,
                Expr.Function2
                  .instance[(FromK, FromV), Int, partial.Result[(ToK, ToV)]] { (pairExpr, indexExpr) =>
                    val pairGetters = ProductType.parseExtraction[(FromK, FromV)].get.extraction
                    val _1 = pairGetters("_1")
                    val _2 = pairGetters("_2")
                    import _1.{Underlying as From_1, value as getter_1}, _2.{Underlying as From_2, value as getter_2}
                    ChimneyExpr.PartialResult.product(
                      toKeyP
                        .fulfilAsVal(getter_1.get(pairExpr).upcastToExprOf[FromK])
                        .use { case (keyResult, key) => Expr.block(List(Expr.suppressUnused(key)), keyResult) }
                        .unsealErrorPath
                        .prependErrorPath(
                          ChimneyExpr.PathElement.Accessor(Expr.String("_1")).upcastToExprOf[partial.PathElement]
                        )
                        .prependErrorPath(
                          ChimneyExpr.PathElement.Index(indexExpr).upcastToExprOf[partial.PathElement]
                        ),
                      toValueP
                        .fulfilAsVal(getter_2.get(pairExpr).upcastToExprOf[FromV])
                        .use { case (valueResult, value) =>
                          Expr.block(List(Expr.suppressUnused(value)), valueResult)
                        }
                        .unsealErrorPath
                        .prependErrorPath(
                          ChimneyExpr.PathElement.Accessor(Expr.String("_2")).upcastToExprOf[partial.PathElement]
                        )
                        .prependErrorPath(
                          ChimneyExpr.PathElement.Index(indexExpr).upcastToExprOf[partial.PathElement]
                        ),
                      failFast
                    )
                  }
                  .tupled,
                failFast,
                factory
              )

        factoryEither match {
          case Left(totalFactory)   => DerivationResult.partialExpr(partialResultTraverse(totalFactory))
          case Right(partialResult) => DerivationResult.partialExpr(partialResultTraverse(partialResult).flatten)
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
                    Left(totalFactory.upcastToExprOf[Factory[(Key, Value), M]])
                  case Right(partialFactory) =>
                    Right(partialFactory.upcastToExprOf[Factory[(Key, Value), partial.Result[M]]])
                }

              def iterator(collection: Expr[M]): Expr[Iterator[(Key, Value)]] =
                it.value.iterator(collection).upcastToExprOf[Iterator[(Key0, Value0)]]

              def to[Collection2: Type](
                  collection: Expr[M],
                  factory: Expr[Factory[(Key, Value), Collection2]]
              ): Expr[Collection2] = it.value.to(collection, factory.upcastToExprOf[Factory[Inner, Collection2]])
            }
        }
      final def unapply[M](M: Type[M]): Option[TotallyOrPartiallyBuildMap[M]] = parse(using M)
    }
  }
}
