package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.compat.Factory

private[compiletime] trait TransformIterableToIterableRuleModule {
  this: Derivation & TransformProductToProductRuleModule =>

  import Type.Implicits.*, ChimneyType.Implicits.*, TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformIterableToIterableRule extends Rule("IterableToIterable") {

    @scala.annotation.nowarn
    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (ctx, Type[From], Type[To]) match {
        case (
              TransformationContext.ForPartial(_, failFast),
              TotallyOrPartiallyBuildIterable(from2),
              TotallyOrPartiallyBuildIterable(to2)
            ) if from2.value.asMap.isDefined && to2.Underlying.isTuple =>
          val Some((fromK, fromV)) = from2.value.asMap: @unchecked
          val Type.Tuple2(toK, toV) = to2.Underlying: @unchecked
          import fromK.Underlying as FromK, fromV.Underlying as FromV, toK.Underlying as ToK, toV.Underlying as ToV
          DerivationResult.log(
            s"Resolved ${Type.prettyPrint[From]} (${from2.value}) as map type and ${Type.prettyPrint[To]} (${to2.value}) as iterable of tuple"
          ) >>
            mapPartialMaps[From, To, FromK, FromV, ToK, ToV](
              from2.value.asInstanceOf[TotallyOrPartiallyBuildIterable[From, (FromK, FromV)]],
              to2.value.asInstanceOf[TotallyOrPartiallyBuildIterable[To, (ToK, ToV)]],
              failFast
            )
        case (TransformationContext.ForTotal(_), TotallyOrPartiallyBuildIterable(_), PartiallyBuildIterable(to2)) =>
          DerivationResult.attemptNextRuleBecause(
            s"Only PartiallyBuildIterable available for ${Type.prettyPrint[To]} (${to2.value}), in total context"
          )
        case (_, TotallyOrPartiallyBuildIterable(from2), TotallyOrPartiallyBuildIterable(to2)) =>
          import from2.{Underlying as InnerFrom, value as fromIterable},
            to2.{Underlying as InnerTo, value as toIterable}
          DerivationResult.log(
            s"Resolved ${Type.prettyPrint[From]} (${from2.value}) and ${Type.prettyPrint[To]} (${to2.value}) as iterable types"
          ) >>
            mapIterables[From, To, InnerFrom, InnerTo](fromIterable, toIterable)
        case _ => DerivationResult.attemptNextRule
      }

    private def mapPartialMaps[From, To, FromK: Type, FromV: Type, ToK: Type, ToV: Type](
        fromIterable: TotallyOrPartiallyBuildIterable[From, (FromK, FromV)],
        toIterable: TotallyOrPartiallyBuildIterable[To, (ToK, ToV)],
        failFast: Expr[Boolean]
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
          }.map(_.ensurePartial)
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
          //       ${ derivedToKey }.prependErrorPath(partial.PathElement.MapKey(fromKey)))
          //       ${ derivedToValue }.prependErrorPath(partial.PathElement.MapValue(fromKey))),
          //   },
          //   ${ failFast }
          // )(${ factory }) }
          ChimneyExpr.PartialResult
            .traverse[ToOrPartialTo, (FromK, FromV), (ToK, ToV)](
              fromIterable.iterator(ctx.src),
              toKeyP
                .fulfilAsLambda2(toValueP) { case ((keyResult, key), valueResult) =>
                  ChimneyExpr.PartialResult.product(
                    keyResult.prependErrorPath(
                      ChimneyExpr.PathElement.MapKey(key.upcastToExprOf[Any]).upcastToExprOf[partial.PathElement]
                    ),
                    valueResult.prependErrorPath(
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
          case Left(totalFactory)    => DerivationResult.expandedPartial(partialResultTraverse(totalFactory))
          case Right(partialFactory) => DerivationResult.expandedPartial(partialResultTraverse(partialFactory).flatten)
        }
      }
    }

    private def mapIterables[From, To, InnerFrom: Type, InnerTo: Type](
        fromIterable: TotallyOrPartiallyBuildIterable[From, InnerFrom],
        toIterable: TotallyOrPartiallyBuildIterable[To, InnerTo]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      ExprPromise
        .promise[InnerFrom](ExprPromise.NameGenerationStrategy.FromExpr(ctx.src))
        .traverse { (newFromSrc: Expr[InnerFrom]) =>
          useOverrideIfPresentOr("everyItem", ctx.config.filterCurrentOverridesForEveryItem) {
            deriveRecursiveTransformationExpr[InnerFrom, InnerTo](newFromSrc, Path.Root.everyItem, Path.Root.everyItem)
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
                case Left(totalFactory)    => DerivationResult.expandedTotal(srcToFactory(totalFactory))
                case Right(partialFactory) => DerivationResult.expandedPartial(srcToFactory(partialFactory))
              }
            } else {
              def srcIteratorMapTo[ToOrPartialTo: Type](
                  factory: Expr[Factory[InnerTo, ToOrPartialTo]]
              ): Expr[ToOrPartialTo] =
                // We're constructing
                // '{ ${ src }.iterator.map(from2 => ${ derivedInnerTo }).to(Factory[$InnerTo, $ToOrPartialTo]) }
                fromIterable.iterator(ctx.src).map(totalP.fulfilAsLambda[InnerTo]).to[ToOrPartialTo](factory)

              toIterable.factory match {
                case Left(totalFactory)    => DerivationResult.expandedTotal(srcIteratorMapTo(totalFactory))
                case Right(partialFactory) => DerivationResult.expandedPartial(srcIteratorMapTo(partialFactory))
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
                  //     ${ resultTo }.prependErrorPath(partial.PathElement.Index(index))
                  //   },
                  //   ${ failFast }
                  // )(${ factory }) }
                  ChimneyExpr.PartialResult.traverse[ToOrPartialTo, (InnerFrom, Int), InnerTo](
                    fromIterable.iterator(src).zipWithIndex,
                    partialP
                      .fulfilAsLambda2(
                        ExprPromise.promise[Int](ExprPromise.NameGenerationStrategy.FromPrefix("idx"))
                      ) { (result: Expr[partial.Result[InnerTo]], idx: Expr[Int]) =>
                        result.prependErrorPath(
                          ChimneyExpr.PathElement.Index(idx).upcastToExprOf[partial.PathElement]
                        )
                      }
                      .tupled,
                    failFast,
                    factory
                  )

                toIterable.factory match {
                  case Left(totalTransformer) =>
                    DerivationResult.expandedPartial(partialResultTraverse(totalTransformer))
                  case Right(partialTransformer) =>
                    DerivationResult.expandedPartial(partialResultTraverse(partialTransformer).flatten)
                }
              case TransformationContext.ForTotal(_) =>
                DerivationResult.assertionError("Derived Partial Expr for Total Context")
            }
          }
        }
  }
}
