package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

private[compiletime] trait TransformMapToMapRuleModule { this: Derivation with TransformIterableToIterableRuleModule =>

  import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformMapToMapRule extends Rule("MapToMap") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (ctx, Type[From], Type[To]) match {
        case (TransformationContext.ForPartial(src, failFast), Type.Map(fromK, fromV), Type.Map(toK, toV)) =>
          ExistentialType.use4(fromK, fromV, toK, toV) {
            implicit FromKey: Type[fromK.Underlying] => implicit FromValue: Type[fromV.Underlying] =>
              implicit ToKey: Type[toK.Underlying] => implicit ToValue: Type[toV.Underlying] =>
                val toKeyResult = ExprPromise
                  .promise[fromK.Underlying](ExprPromise.NameGenerationStrategy.FromPrefix("key"))
                  .traverse { key =>
                    deriveRecursiveTransformationExpr[fromK.Underlying, toK.Underlying](key)
                      .map(
                        _.ensurePartial.prependErrorPath(
                          ChimneyExpr.PathElement.MapKey(key.upcastExpr[Any]).upcastExpr[partial.PathElement]
                        )
                      )
                  }
                val toValueResult = ExprPromise
                  .promise[fromV.Underlying](ExprPromise.NameGenerationStrategy.FromPrefix("value"))
                  .traverse { value =>
                    deriveRecursiveTransformationExpr[fromV.Underlying, toV.Underlying](value)
                      .map(
                        _.ensurePartial.prependErrorPath(
                          ChimneyExpr.PathElement.MapValue(value.upcastExpr[Any]).upcastExpr[partial.PathElement]
                        )
                      )
                  }

                val factoryResult = DerivationResult.summonImplicit[Factory[(toK.Underlying, toV.Underlying), To]]

                toKeyResult.parTuple(toValueResult).parTuple(factoryResult).flatMap {
                  case ((toKeyP, toValueP), factory) =>
                    // We're constructing:
                    // '{ partial.Result.traverse[To, ($fromK, $fromV), ($toK, $toV)](
                    //   ${ src }.iterator,
                    //   { case (key, value) =>
                    //     partial.Result.product(
                    //       ${ resultToKey }.prependErrorPath(partial.PathElement.MapKey(key)),
                    //       ${ resultToValue }.prependErrorPath(partial.PathElement.MapValue(value),
                    //       ${ failFast }
                    //     )
                    //   },
                    //   ${ failFast }
                    // )(${ factory })
                    DerivationResult.expandedPartial(
                      ChimneyExpr.PartialResult
                        .traverse[To, (fromK.Underlying, fromV.Underlying), (toK.Underlying, toV.Underlying)](
                          src.upcastExpr[Map[fromK.Underlying, fromV.Underlying]].iterator,
                          toKeyP.fulfilAsLambda2(toValueP)(ChimneyExpr.PartialResult.product(_, _, failFast)).tupled,
                          failFast,
                          factory
                        )
                    )
                }
          }
        case (_, Type.Map(_, _), _) =>
          // TODO: fallback log
          TransformIterableToIterableRule.expand(ctx)
        case _ => DerivationResult.attemptNextRule
      }
  }
}
