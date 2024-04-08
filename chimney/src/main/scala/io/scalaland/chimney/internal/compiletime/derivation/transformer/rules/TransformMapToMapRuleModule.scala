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
          mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](ctx.src.upcastExpr[Map[FromK, FromV]].iterator)
        case (TransformationContext.ForTotal(_), IterableOrArray(from2), Type.Map(toK, toV))
            if !ctx.config.areOverridesEmpty =>
          // val Type.Tuple2(fromK, fromV) = from2: @unchecked
          val (fromK, fromV) = Type.Tuple2.unapply(from2.Underlying).get
          import from2.{Underlying as InnerFrom, value as fromIorA}, fromK.Underlying as FromK,
            fromV.Underlying as FromV, toK.Underlying as ToK, toV.Underlying as ToV
          mapMapForTotalTransformers[From, To, FromK, FromV, ToK, ToV](
            fromIorA.iterator(ctx.src).upcastExpr[Iterator[(FromK, FromV)]]
          )
        case (TransformationContext.ForPartial(_, failFast), Type.Map(fromK, fromV), Type.Map(toK, toV)) =>
          import fromK.Underlying as FromK, fromV.Underlying as FromV, toK.Underlying as ToK, toV.Underlying as ToV
          mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
            ctx.src.upcastExpr[Map[FromK, FromV]].iterator,
            failFast
          )
        case (TransformationContext.ForPartial(_, failFast), IterableOrArray(from2), Type.Map(toK, toV)) =>
          // val Type.Tuple2(fromK, fromV) = from2: @unchecked
          val (fromK, fromV) = Type.Tuple2.unapply(from2.Underlying).get
          import from2.{Underlying as InnerFrom, value as fromIorA}, fromK.Underlying as FromK,
            fromV.Underlying as FromV, toK.Underlying as ToK, toV.Underlying as ToV
          mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](
            fromIorA.iterator(ctx.src).upcastExpr[Iterator[(FromK, FromV)]],
            failFast
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
        failFast: Expr[Boolean]
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
          deriveRecursiveTransformationExpr[FromV, ToV](value, Path.Root.everyMapValue).map(_.ensurePartial)
        }

      val factoryResult = DerivationResult.summonImplicit[Factory[(ToK, ToV), To]]

      toKeyResult.parTuple(toValueResult).parTuple(factoryResult).flatMap { case ((toKeyP, toValueP), factory) =>
        // We're constructing:
        // '{ partial.Result.traverse[To, ($FromK, $FromV), ($ToK, $ToV)](
        //   ${ iterator },
        //   { case (key, value) =>
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
                .fulfilAsLambda2(toValueP) { case ((keyResult, key), valueResult) =>
                  ChimneyExpr.PartialResult.product(
                    keyResult.prependErrorPath(
                      ChimneyExpr.PathElement.MapKey(key.upcastExpr[Any]).upcastExpr[partial.PathElement]
                    ),
                    valueResult.prependErrorPath(
                      ChimneyExpr.PathElement.MapValue(key.upcastExpr[Any]).upcastExpr[partial.PathElement]
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
