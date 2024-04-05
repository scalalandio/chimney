package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.annotation.unused
import scala.collection.compat.Factory

private[compiletime] trait TransformIterableToIterableRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformIterableToIterableRule extends Rule("IterableToIterable") {

    @scala.annotation.nowarn
    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To], ctx) match {
        case (Type.Map(fromK, fromV), IterableOrArray(to2), TransformationContext.ForPartial(_, failFast))
            if to2.Underlying.isTuple =>
          // val Type.Tuple2(toK, toV) = to2: @unchecked
          val (toK, toV) = Type.Tuple2.unapply(to2.Underlying).get
          import to2.Underlying as InnerTo, fromK.Underlying as FromK, fromV.Underlying as FromV, toV.Underlying as ToV,
            toK.Underlying as ToK
          mapPartialMaps[From, To, InnerTo, FromK, FromV, ToK, ToV](to2.value, failFast)
        case (IterableOrArray(from2), IterableOrArray(to2), _) =>
          import from2.{Underlying as InnerFrom, value as fromIorA}, to2.{Underlying as InnerTo, value as toIorA}
          mapIterables[From, To, InnerFrom, InnerTo](fromIorA, toIorA)
        case _ =>
          DerivationResult.attemptNextRule
      }

    private def mapPartialMaps[From, To, InnerTo: Type, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        toIorA: IterableOrArray[To, InnerTo],
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

      toKeyResult.parTuple(toValueResult).parTuple(toIorA.factory).flatMap { case ((toKeyP, toValueP), factory) =>
        // We're constructing:
        // '{ partial.Result.traverse(
        //   ${ src }.iterator,
        //   { case (fromKey, fromValue) =>
        //     partial.Result.product(
        //       ${ derivedToKey }.prependErrorPath(partial.PathElement.MapKey(fromKey)))
        //       ${ derivedToValue }.prependErrorPath(partial.PathElement.MapValue(fromKey))),
        //   },
        //   ${ failFast }
        // )(${ factory }) }
        DerivationResult.expandedPartial(
          ChimneyExpr.PartialResult
            .traverse[To, (FromK, FromV), (ToK, ToV)](
              ctx.src.widenExpr[Map[FromK, FromV]].iterator,
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
              factory.widenExpr[Factory[(ToK, ToV), To]]
            )
        )
      }
    }

    private def mapIterables[From, To, InnerFrom: Type, InnerTo: Type](
        fromIorA: IterableOrArray[From, InnerFrom],
        toIorA: IterableOrArray[To, InnerTo]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      ExprPromise
        .promise[InnerFrom](ExprPromise.NameGenerationStrategy.FromExpr(ctx.src))
        .traverse { (newFromSrc: Expr[InnerFrom]) =>
          deriveRecursiveTransformationExpr[InnerFrom, InnerTo](newFromSrc, Path.Root.eachItem)
        }
        .flatMap { (to2P: ExprPromise[InnerFrom, TransformationExpr[InnerTo]]) =>
          to2P.foldTransformationExpr { (totalP: ExprPromise[InnerFrom, Expr[InnerTo]]) =>
            // After mapping we don't know the exact static type here, but we might check the values
            // to see if we could skip .to(factory) part
            lazy val mappedFrom = fromIorA.map(ctx.src)(totalP.fulfilAsLambda[InnerTo])
            if (Type[InnerFrom] =:= Type[InnerTo]) {
              toIorA.factory.flatMap { (factory: Expr[Factory[InnerTo, To]]) =>
                // We're constructing:
                // '{ ${ src }.to(Factory[$To, $InnerTo]) }
                DerivationResult.expandedTotal(
                  fromIorA.to[To](ctx.src)(factory.upcastExpr[Factory[InnerFrom, To]])
                )
              }
            } else if (mappedFrom.Underlying =:= Type[To]) {
              // We're constructing:
              // '{ ${ src }.map(from2 => ${ derivedInnerTo }) }
              import mappedFrom.{Underlying, value as expr}
              DerivationResult.expandedTotal(expr.upcastExpr[To])
            } else {
              // We're constructing
              // '{ ${ src }.iterator.map(from2 => ${ derivedInnerTo }).to(Factory[$To, $InnerTo]) }
              toIorA.factory.flatMap { (factory: Expr[Factory[InnerTo, To]]) =>
                DerivationResult.expandedTotal(
                  fromIorA.iterator(ctx.src).map(totalP.fulfilAsLambda[InnerTo]).to[To](factory)
                )
              }
            }
          } { (partialP: ExprPromise[InnerFrom, Expr[partial.Result[InnerTo]]]) =>
            ctx match {
              case TransformationContext.ForPartial(src, failFast) =>
                // We're constructing:
                // '{ partial.Result.traverse[To, ($InnerFrom, Int), $InnerTo](
                //   ${ src }.iterator.zipWithIndex,
                //   { case (value, index) =>
                //     ${ resultTo }.prependErrorPath(partial.PathElement.Index(index))
                //   },
                //   ${ failFast }
                // )(${ factory }) }
                toIorA.factory.flatMap { (factory: Expr[Factory[InnerTo, To]]) =>
                  DerivationResult.expandedPartial(
                    ChimneyExpr.PartialResult.traverse[To, (InnerFrom, Int), InnerTo](
                      fromIorA.iterator(src).zipWithIndex,
                      partialP
                        .fulfilAsLambda2(
                          ExprPromise.promise[Int](ExprPromise.NameGenerationStrategy.FromPrefix("idx"))
                        ) { (result: Expr[partial.Result[InnerTo]], idx: Expr[Int]) =>
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

    implicit private class IorAOps[M: Type, A: Type](@unused private val iora: IterableOrArray[M, A]) {

      def factory: DerivationResult[Expr[Factory[A, M]]] = DerivationResult.summonImplicit[Factory[A, M]]
    }
  }
}
