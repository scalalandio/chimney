package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

private[compiletime] trait TransformIterableToIterableRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformIterableToIterableRule extends Rule("IterableToIterable") {

    @scala.annotation.nowarn
    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To], ctx) match {
        case (Type.Map(fromK, fromV), IterableOrArray(to2), TransformationContext.ForPartial(src, failFast))
            if to2.Underlying.isTuple =>
          // val Type.Tuple2(toK, toV) = to2: @unchecked
          val (toK, toV) = Type.Tuple2.unapply(to2.Underlying).get
          import fromK.Underlying as FromK, fromV.Underlying as FromV, toV.Underlying as ToV, toK.Underlying as ToK,
          to2.Underlying as To2
          val toKeyResult = ExprPromise
            .promise[fromK.Underlying](ExprPromise.NameGenerationStrategy.FromPrefix("key"))
            .traverse { key =>
              deriveRecursiveTransformationExpr[fromK.Underlying, toK.Underlying](key).map(_.ensurePartial -> key)
            }
          val toValueResult = ExprPromise
            .promise[fromV.Underlying](ExprPromise.NameGenerationStrategy.FromPrefix("value"))
            .traverse { value =>
              deriveRecursiveTransformationExpr[fromV.Underlying, toV.Underlying](value).map(_.ensurePartial)
            }

          toKeyResult.parTuple(toValueResult).parTuple(to2.value.factory).flatMap {
            case ((toKeyP, toValueP), factory) =>
              DerivationResult.expandedPartial(
                ChimneyExpr.PartialResult
                  .traverse[To, (fromK.Underlying, fromV.Underlying), (toK.Underlying, toV.Underlying)](
                    src.widenExpr[Map[fromK.Underlying, fromV.Underlying]].iterator,
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
                    factory.widenExpr[Factory[(toK.Underlying, toV.Underlying), To]]
                  )
              )
          }
        case (IterableOrArray(from2), IterableOrArray(to2), _) =>
          import from2.{Underlying as From2, value as fromIorA}, to2.{Underlying as To2, value as toIorA}
          ExprPromise
            .promise[from2.Underlying](ExprPromise.NameGenerationStrategy.FromExpr(ctx.src))
            .traverse { (newFromSrc: Expr[from2.Underlying]) =>
              deriveRecursiveTransformationExpr[from2.Underlying, to2.Underlying](newFromSrc)
            }
            .flatMap { (to2P: ExprPromise[from2.Underlying, TransformationExpr[to2.Underlying]]) =>
              to2P.foldTransformationExpr { (totalP: ExprPromise[from2.Underlying, Expr[to2.Underlying]]) =>
                // After mapping we don't know the exact static type here, but we might check the values
                // to see if we could skip .to(factory) part
                lazy val mappedFrom = fromIorA.map(ctx.src)(totalP.fulfilAsLambda[to2.Underlying])
                if (Type[from2.Underlying] =:= Type[to2.Underlying]) {
                  toIorA.factory.flatMap { (factory: Expr[Factory[to2.Underlying, To]]) =>
                    // We're constructing:
                    // '{ ${ src }.to(Factory[$To, $to2]) }
                    DerivationResult.expandedTotal(
                      fromIorA.to[To](ctx.src)(factory.upcastExpr[Factory[from2.Underlying, To]])
                    )
                  }
                } else if (mappedFrom.Underlying =:= Type[To]) {
                  // We're constructing:
                  // '{ ${ src }.map(from2 => ${ derivedTo2 }) }
                  import mappedFrom.{Underlying, value as expr}
                  DerivationResult.expandedTotal(expr.upcastExpr[To])
                } else {
                  // We're constructing
                  // '{ ${ src }.iterator.map(from2 => ${ derivedTo2 }).to(Factory[$To, $to2]) }
                  toIorA.factory.flatMap { (factory: Expr[Factory[to2.Underlying, To]]) =>
                    DerivationResult.expandedTotal(
                      fromIorA.iterator(ctx.src).map(totalP.fulfilAsLambda[to2.Underlying]).to[To](factory)
                    )
                  }
                }
              } { (partialP: ExprPromise[from2.Underlying, Expr[partial.Result[to2.Underlying]]]) =>
                ctx match {
                  case TransformationContext.ForPartial(src, failFast) =>
                    // We're constructing:
                    // '{ partial.Result.traverse[To, ($from2, Int), $to2](
                    //   ${ src }.iterator.zipWithIndex,
                    //   { case (value, index) =>
                    //     ${ resultTo }.prependErrorPath(partial.PathElement.Index(index))
                    //   },
                    //   ${ failFast }
                    // )(${ factory })
                    toIorA.factory.flatMap { (factory: Expr[Factory[to2.Underlying, To]]) =>
                      DerivationResult.expandedPartial(
                        ChimneyExpr.PartialResult.traverse[To, (from2.Underlying, Int), to2.Underlying](
                          fromIorA.iterator(src).zipWithIndex,
                          partialP
                            .fulfilAsLambda2(
                              ExprPromise.promise[Int](ExprPromise.NameGenerationStrategy.FromPrefix("idx"))
                            ) { (result: Expr[partial.Result[to2.Underlying]], idx: Expr[Int]) =>
                              result.prependErrorPath(
                                ChimneyExpr.PathElement.Index(idx).upcastExpr[partial.PathElement]
                              )
                            }
                            .tupled,
                          failFast,
                          factory
                        )
                      )
                    }
                  case TransformationContext.ForTotal(_) =>
                    DerivationResult.assertionError("Derived Partial Expr for Total Context")
                }
              }
            }
        case _ =>
          DerivationResult.attemptNextRule
      }

    implicit private class IorAOps[M: Type, A: Type](private val iora: IterableOrArray[M, A]) {

      def factory: DerivationResult[Expr[Factory[A, M]]] = DerivationResult.summonImplicit[Factory[A, M]]
    }
  }
}
