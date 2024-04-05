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
        case (TransformationContext.ForPartial(_, failFast), Type.Map(fromK, fromV), Type.Map(toK, toV)) =>
          import fromK.Underlying as FromK, fromV.Underlying as FromV, toK.Underlying as ToK, toV.Underlying as ToV
          mapMapForPartialTransformers[From, To, FromK, FromV, ToK, ToV](failFast)
        case (_, Type.Map(_, _), _) =>
          DerivationResult.namedScope(
            "MapToMap matched in the context of total transformation - delegating to IterableToIterable"
          ) {
            TransformIterableToIterableRule.expand(ctx)
          }
        case _ => DerivationResult.attemptNextRule
      }

    private def mapMapForPartialTransformers[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        failFast: Expr[Boolean]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] = {
      val toKeyResult = ExprPromise
        .promise[FromK](ExprPromise.NameGenerationStrategy.FromPrefix("key"))
        .traverse { key =>
          deriveRecursiveTransformationExpr[FromK, ToK](key, Path.Root.eachMapKey).map(_.ensurePartial -> key)
        }
      val toValueResult = ExprPromise
        .promise[FromV](ExprPromise.NameGenerationStrategy.FromPrefix("value"))
        .traverse { value =>
          deriveRecursiveTransformationExpr[FromV, ToV](value, Path.Root.eachMapValue).map(_.ensurePartial)
        }

      val factoryResult = DerivationResult.summonImplicit[Factory[(ToK, ToV), To]]

      toKeyResult.parTuple(toValueResult).parTuple(factoryResult).flatMap { case ((toKeyP, toValueP), factory) =>
        // We're constructing:
        // '{ partial.Result.traverse[To, ($FromK, $FromV), ($ToK, $ToV)](
        //   ${ src }.iterator,
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
              ctx.src.upcastExpr[Map[FromK, FromV]].iterator,
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
