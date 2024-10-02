package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

private[compiletime] trait TransformMapToMapRuleModule {
  this: Derivation & TransformIterableToIterableRuleModule & TransformProductToProductRuleModule =>

  import Type.Implicits.*, ChimneyType.Implicits.*, TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformMapToMapRule extends Rule("MapToMap") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (ctx, Type[From], Type[To]) match {
        case (TransformationContext.ForTotal(_), TotallyOrPartiallyBuildMap(fromMap), TotallyOrPartiallyBuildMap(toMap))
            if !ctx.config.areOverridesEmpty =>
          import fromMap.{Key as FromK, Value as FromV}, toMap.{Key as ToK, Value as ToV}
          mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](fromMap.iterator(ctx.src), toMap.factory)
        case (
              TransformationContext.ForTotal(_),
              TotallyOrPartiallyBuildIterable(from2),
              TotallyOrPartiallyBuildMap(toMap)
            ) if !ctx.config.areOverridesEmpty && from2.Underlying.isTuple =>
          val Type.Tuple2(fromK, fromV) = from2.Underlying: @unchecked
          import from2.{Underlying as InnerFrom, value as fromIterable}, fromK.Underlying as FromK,
            fromV.Underlying as FromV, toMap.{Key as ToK, Value as ToV}
          mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](
            fromIterable.iterator(ctx.src).upcastToExprOf[Iterator[(FromK, FromV)]], // needed because iterable, not map
            toMap.factory
          )
        case (
              TransformationContext.ForPartial(_, failFast),
              TotallyOrPartiallyBuildMap(fromMap),
              TotallyOrPartiallyBuildMap(toMap)
            ) =>
          import fromMap.{Key as FromK, Value as FromV}, toMap.{Key as ToK, Value as ToV}
          mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
            fromMap.iterator(ctx.src),
            toMap.factory,
            failFast,
            isConversionFromMap = true
          )
        case (
              TransformationContext.ForPartial(_, failFast),
              TotallyOrPartiallyBuildIterable(from2),
              TotallyOrPartiallyBuildMap(toMap)
            ) if from2.Underlying.isTuple =>
          val Type.Tuple2(fromK, fromV) = from2.Underlying: @unchecked
          import from2.{Underlying as InnerFrom, value as fromIterable}, fromK.Underlying as FromK,
            fromV.Underlying as FromV, toMap.{Key as ToK, Value as ToV}
          mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
            fromIterable.iterator(ctx.src).upcastToExprOf[Iterator[(FromK, FromV)]],
            toMap.factory,
            failFast,
            isConversionFromMap = false
          )
        case (_, _, Type.Map(_, _)) | (_, Type.Map(_, _), _) =>
          DerivationResult.namedScope(
            "MapToMap matched in the context of total transformation without overrides - delegating to IterableToIterable"
          ) {
            TransformIterableToIterableRule.expand(ctx)
          }
        case _ => DerivationResult.attemptNextRule
      }

    private def mapMapForTotalTransformers[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        iterator: Expr[Iterator[(FromK, FromV)]],
        factoryEither: Either[Expr[Factory[(ToK, ToV), To]], Expr[Factory[(ToK, ToV), partial.Result[To]]]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] = {
      val toKeyResult = ExprPromise
        .promise[FromK](ExprPromise.NameGenerationStrategy.FromPrefix("key"))
        .traverse { key =>
          useOverrideIfPresentOr("everyMapKey", ctx.config.filterCurrentOverridesForEveryMapKey) {
            deriveRecursiveTransformationExpr[FromK, ToK](key, Path.Root.everyMapKey, Path.Root.everyMapKey)
          }.map(_.ensureTotal)
        }
      val toValueResult = ExprPromise
        .promise[FromV](ExprPromise.NameGenerationStrategy.FromPrefix("value"))
        .traverse { value =>
          useOverrideIfPresentOr("everyMapValue", ctx.config.filterCurrentOverridesForEveryMapValue) {
            deriveRecursiveTransformationExpr[FromV, ToV](value, Path.Root.everyMapValue, Path.Root.everyMapValue)
          }.map(_.ensureTotal)
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
          case Left(totalFactory)    => DerivationResult.expandedTotal(iteratorMapTo(totalFactory))
          case Right(partialFactory) => DerivationResult.expandedPartial(iteratorMapTo(partialFactory))
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
    ): DerivationResult[Rule.ExpansionResult[To]] = {
      val toKeyResult = ExprPromise
        .promise[FromK](ExprPromise.NameGenerationStrategy.FromPrefix("key"))
        .traverse { key =>
          useOverrideIfPresentOr("everyMapKey", ctx.config.filterCurrentOverridesForEveryMapKey) {
            deriveRecursiveTransformationExpr[FromK, ToK](key, Path.Root.everyMapKey, Path.Root.everyMapKey)
          }.map(_.ensurePartial -> key)
        }
      val toValueResult = ExprPromise
        .promise[FromV](ExprPromise.NameGenerationStrategy.FromPrefix("value"))
        .traverse { value =>
          useOverrideIfPresentOr("everyMapValue", ctx.config.filterCurrentOverridesForEveryMapValue) {
            deriveRecursiveTransformationExpr[FromV, ToV](value, Path.Root.everyMapValue, Path.Root.everyMapValue)
          }.map(_.ensurePartial -> value)
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
            //       ${ resultToKey }.prependErrorPath(partial.PathElement.MapKey(key)),
            //       ${ resultToValue }.prependErrorPath(partial.PathElement.MapValue(key),
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
                        keyResult.prependErrorPath(
                          ChimneyExpr.PathElement.MapKey(key.upcastToExprOf[Any]).upcastToExprOf[partial.PathElement]
                        ),
                        valueResult.prependErrorPath(
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
            //           .prependErrorPath(partial.PathElement.Accessor("_1"))}
            //           .prependErrorPath(partial.PathElement.Index(idx))
            //       },
            //       {
            //         val value = pair._2
            //         ${ resultToValue }
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
          case Left(totalFactory)   => DerivationResult.expandedPartial(partialResultTraverse(totalFactory))
          case Right(partialResult) => DerivationResult.expandedPartial(partialResultTraverse(partialResult).flatten)
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

      def unapply[M](implicit M: Type[M]): Option[TotallyOrPartiallyBuildMap[M]] =
        TotallyOrPartiallyBuildIterable.unapply[M].flatMap(it => it.value.asMap.map(it -> _)).map {
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
    }
  }
}
