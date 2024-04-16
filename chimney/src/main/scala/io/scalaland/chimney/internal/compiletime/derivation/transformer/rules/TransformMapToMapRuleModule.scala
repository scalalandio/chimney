package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

private[compiletime] trait TransformMapToMapRuleModule { this: Derivation with TransformIterableToIterableRuleModule =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformMapToMapRule extends Rule("MapToMap") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (ctx, Type[From], Type[To]) match {
        case (TransformationContext.ForTotal(_), Type.Map(fromK, fromV), Type.Map(toK, toV))
            if !ctx.config.areOverridesEmpty =>
          import fromK.Underlying as FromK, fromV.Underlying as FromV, toK.Underlying as ToK, toV.Underlying as ToV
          mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](
            ctx.src.upcastToExprOf[scala.collection.Map[FromK, FromV]].iterator
          )
        case (TransformationContext.ForTotal(_), IterableOrArray(from2), Type.Map(toK, toV))
            if !ctx.config.areOverridesEmpty =>
          // val Type.Tuple2(fromK, fromV) = from2: @unchecked
          val (fromK, fromV) = Type.Tuple2.unapply(from2.Underlying).get
          import from2.{Underlying as InnerFrom, value as fromIorA}, fromK.Underlying as FromK,
            fromV.Underlying as FromV, toK.Underlying as ToK, toV.Underlying as ToV
          mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](
            fromIorA.iterator(ctx.src).upcastToExprOf[Iterator[(FromK, FromV)]]
          )
        case (TransformationContext.ForPartial(_, failFast), Type.Map(fromK, fromV), Type.Map(toK, toV)) =>
          import fromK.Underlying as FromK, fromV.Underlying as FromV, toK.Underlying as ToK, toV.Underlying as ToV
          mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
            ctx.src.upcastToExprOf[scala.collection.Map[FromK, FromV]].iterator,
            failFast,
            isConversionFromMap = true
          )
        case (TransformationContext.ForPartial(_, failFast), IterableOrArray(from2), Type.Map(toK, toV)) =>
          // val Type.Tuple2(fromK, fromV) = from2: @unchecked
          val (fromK, fromV) = Type.Tuple2.unapply(from2.Underlying).get
          import from2.{Underlying as InnerFrom, value as fromIorA}, fromK.Underlying as FromK,
            fromV.Underlying as FromV, toK.Underlying as ToK, toV.Underlying as ToV
          mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
            fromIorA.iterator(ctx.src).upcastToExprOf[Iterator[(FromK, FromV)]],
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
        iterator: Expr[Iterator[(FromK, FromV)]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] = {
      val toKeyResult = ExprPromise
        .promise[FromK](ExprPromise.NameGenerationStrategy.FromPrefix("key"))
        .traverse { key =>
          deriveRecursiveTransformationExpr[FromK, ToK](key, Path.Root.everyMapKey).map(_.ensureTotal)
        }
      val toValueResult = ExprPromise
        .promise[FromV](ExprPromise.NameGenerationStrategy.FromPrefix("value"))
        .traverse { value =>
          deriveRecursiveTransformationExpr[FromV, ToV](value, Path.Root.everyMapValue).map(_.ensureTotal)
        }

      val factoryResult = DerivationResult.summonImplicit[Factory[(ToK, ToV), To]]

      toKeyResult.parTuple(toValueResult).parTuple(factoryResult).flatMap { case ((toKeyP, toValueP), factory) =>
        // We're constructing:
        // '{ ${ iterator }.map{ case (key, value) =>
        //    (${ resultToKey }, ${ resultToValue })
        //    }
        // }.to(${ factory }) }
        DerivationResult.expandedTotal(
          iterator
            .map[(ToK, ToV)](
              toKeyP
                .fulfilAsLambda2(toValueP) { (toKeyResult, toValueResult) =>
                  Expr.Tuple2(toKeyResult, toValueResult)
                }
                .tupled
            )
            .to(factory)
        )
      }
    }

    private def mapMapForPartialTransformers[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        iterator: Expr[Iterator[(FromK, FromV)]],
        failFast: Expr[Boolean],
        isConversionFromMap: Boolean // or from any sequence of tuples
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] = {
      val toKeyResult = ExprPromise
        .promise[FromK](ExprPromise.NameGenerationStrategy.FromPrefix("key"))
        .traverse { key =>
          deriveRecursiveTransformationExpr[FromK, ToK](key, Path.Root.everyMapKey).map(_.ensurePartial -> key)
        }
      val toValueResult = ExprPromise
        .promise[FromV](ExprPromise.NameGenerationStrategy.FromPrefix("value"))
        .traverse { value =>
          deriveRecursiveTransformationExpr[FromV, ToV](value, Path.Root.everyMapValue).map(_.ensurePartial -> value)
        }

      val factoryResult = DerivationResult.summonImplicit[Factory[(ToK, ToV), To]]

      toKeyResult.parTuple(toValueResult).parTuple(factoryResult).flatMap { case ((toKeyP, toValueP), factory) =>
        if (isConversionFromMap) {
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
          DerivationResult.expandedPartial(
            ChimneyExpr.PartialResult
              .traverse[To, (FromK, FromV), (ToK, ToV)](
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
          )
        } else {
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
          DerivationResult.expandedPartial(
            ChimneyExpr.PartialResult
              .traverse[To, ((FromK, FromV), Int), (ToK, ToV)](
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
                        .use { case (valueResult, value) => Expr.block(List(Expr.suppressUnused(value)), valueResult) }
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
          )
        }
      }
    }
  }
}
