package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

private[compiletime] trait TransformIterableToIterableRuleModule {
  this: Derivation & TransformProductToProductRuleModule =>

  import Type.Implicits.*, ChimneyType.Implicits.*
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
            case None => srcToResult
            case Some(dsls.SourceAppendFallback) =>
              fallbackToResult
                .foldLeft(srcToResult)(merge)
                .log(s"Combined source collection with ${fallbackToResult.size} fallbacks (appended)")
            case Some(dsls.FallbackAppendSource) =>
              fallbackToResult
                .foldRight(srcToResult)(merge)
                .log(s"Combined source collection with ${fallbackToResult.size} fallbacks (prepended)")
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
          val Type.Tuple2(toK, toV) = to2.Underlying: @unchecked
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
            ctx.updateFromTo[Fallback, To](fallbackExpr, updateFallbacks = _ => Vector.empty)(Fallback, ctx.To)
          mapCollections[Fallback, To]
        }
        .collect { case Right(value) => value }
        .toVector

    private def mapPartialMaps[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        fromIterable: TotallyOrPartiallyBuildIterable[From, (FromK, FromV)],
        toIterable: TotallyOrPartiallyBuildIterable[To, (ToK, ToV)],
        failFast: Expr[Boolean]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[TransformationExpr[To]] = {
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
            .map(_.ensurePartial)
        }

      toKeyResult.parTuple(toValueResult).flatMap { case (toKeyP, toValueP) =>
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
              toKeyP
                .fulfilAsLambda2(toValueP) { case ((keyResult, key), valueResult) =>
                  ChimneyExpr.PartialResult.product(
                    keyResult.unsealErrorPath.prependErrorPath(
                      ChimneyExpr.PathElement.MapKey(key.upcastToExprOf[Any]).upcastToExprOf[partial.PathElement]
                    ),
                    valueResult.unsealErrorPath.prependErrorPath(
                      ChimneyExpr.PathElement.MapValue(key.upcastToExprOf[Any]).upcastToExprOf[partial.PathElement]
                    ),
                    failFast
                  )
                }
                .tupled,
              failFast,
              factory
            )

        toIterable.factory match {
          case Left(totalFactory)    => DerivationResult.partialExpr(partialResultTraverse(totalFactory))
          case Right(partialFactory) => DerivationResult.partialExpr(partialResultTraverse(partialFactory).flatten)
        }
      }
    }

    private def mapIterables[From, To, InnerFrom: Type, InnerTo: Type](
        fromIterable: TotallyOrPartiallyBuildIterable[From, InnerFrom],
        toIterable: TotallyOrPartiallyBuildIterable[To, InnerTo]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[TransformationExpr[To]] =
      ExprPromise
        .promise[InnerFrom](ExprPromise.NameGenerationStrategy.FromExpr(ctx.src))
        .traverse { (newFromSrc: Expr[InnerFrom]) =>
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
        .flatMap { (to2P: ExprPromise[InnerFrom, TransformationExpr[InnerTo]]) =>
          to2P.foldTransformationExpr { (totalP: ExprPromise[InnerFrom, Expr[InnerTo]]) =>
            // TODO: restore .map implementation
            if (Type[InnerFrom] =:= Type[InnerTo] && ctx.config.areOverridesEmpty) {
              def srcToFactory[ToOrPartialTo: Type](
                  factory: Expr[Factory[InnerTo, ToOrPartialTo]]
              ): Expr[ToOrPartialTo] =
                // We're constructing:
                // '{ ${ src }.to(Factory[$InnerTo, $ToOrPartialTo]) }
                fromIterable.to[ToOrPartialTo](ctx.src, factory.upcastToExprOf[Factory[InnerFrom, ToOrPartialTo]])

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
                fromIterable.iterator(ctx.src).map(totalP.fulfilAsLambda[InnerTo]).to[ToOrPartialTo](factory)

              toIterable.factory match {
                case Left(totalFactory)    => DerivationResult.totalExpr(srcIteratorMapTo(totalFactory))
                case Right(partialFactory) => DerivationResult.partialExpr(srcIteratorMapTo(partialFactory))
              }
            }
          } { (partialP: ExprPromise[InnerFrom, Expr[partial.Result[InnerTo]]]) =>
            ctx match {
              case TransformationContext.ForPartial(src, failFast) =>
                def partialResultTraverse[ToOrPartialTo: Type](
                    factory: Expr[Factory[InnerTo, ToOrPartialTo]]
                ): Expr[partial.Result[ToOrPartialTo]] =
                  // We're constructing:
                  // '{ partial.Result.traverse[To, ($InnerFrom, Int), $InnerTo](
                  //   ${ src }.iterator.zipWithIndex,
                  //   { case (value, index) =>
                  //     ${ resultTo }.unsealErrorPath.prependErrorPath(partial.PathElement.Index(index))
                  //   },
                  //   ${ failFast }
                  // )(${ factory }) }
                  ChimneyExpr.PartialResult.traverse[ToOrPartialTo, (InnerFrom, Int), InnerTo](
                    fromIterable.iterator(src).zipWithIndex,
                    partialP
                      .fulfilAsLambda2(
                        ExprPromise.promise[Int](ExprPromise.NameGenerationStrategy.FromPrefix("idx"))
                      ) { (result: Expr[partial.Result[InnerTo]], idx: Expr[Int]) =>
                        result.unsealErrorPath.prependErrorPath(
                          ChimneyExpr.PathElement.Index(idx).upcastToExprOf[partial.PathElement]
                        )
                      }
                      .tupled,
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

    // Exposed for TransformMapToMapRuleModule

    def mergeTotal[To: Type](
        result1: DerivationResult[TransformationExpr[To]],
        result2: DerivationResult[TransformationExpr[To]]
    ): DerivationResult[TransformationExpr[To]] = result1.map2(result2) { case (texpr1, texpr2) =>
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
    ): DerivationResult[TransformationExpr[To]] = {
      val TotallyOrPartiallyBuildIterable(to2) = Type[To]: @unchecked
      import to2.{Underlying as InnerTo, value as buildIterable}

      result1
        .map2(result2) {
          case (TransformationExpr.TotalExpr(expr1), TransformationExpr.TotalExpr(expr2)) =>
            TransformationExpr.fromTotal[Iterator[InnerTo]](
              buildIterable.iterator(expr1).concat(buildIterable.iterator(expr2))
            )
          case (texpr1, texpr2) =>
            TransformationExpr.fromPartial[Iterator[InnerTo]](
              texpr1.ensurePartial.map2(texpr2.ensurePartial, failFast)(Expr.Function2.instance { (expr1, expr2) =>
                buildIterable.iterator(expr1).concat(buildIterable.iterator(expr2))
              })
            )
        }
        .map { iterators =>
          iterators.flatMap { expr =>
            buildIterable.factory match {
              case Left(totalFactor)     => TransformationExpr.fromTotal(expr.to(totalFactor))
              case Right(partialFactory) => TransformationExpr.fromPartial(expr.to(partialFactory))
            }
          }
        }
    }
  }
}
