package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.internal.compiletime.fp.Implicits.*
import io.scalaland.chimney.partial

private[compiletime] trait TransformOptionToOptionRuleModule {
  this: Derivation & TransformProductToProductRuleModule =>

  import Type.Implicits.*, ChimneyType.Implicits.*, TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformOptionToOptionRule extends Rule("OptionToOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (OptionalValue(_), _) if Type[To] <:< Type[None.type] =>
          DerivationResult
            .notSupportedTransformerDerivation(ctx)
            .log(s"Discovered that target type is ${Type.prettyPrint[None.type]} which we explicitly reject")
        case (OptionalValue(from2), OptionalValue(to2)) =>
          import from2.{Underlying as InnerFrom, value as optionalFrom},
            to2.{Underlying as InnerTo, value as optionalTo}
          DerivationResult.log(
            s"Resolved ${Type.prettyPrint[From]} (${from2.value}) and ${Type.prettyPrint[To]} (${to2.value}) as optional types"
          ) >> {
            def srcToResult = mapOptions[From, To, InnerFrom, InnerTo](optionalFrom, optionalTo)

            def fallbackToResult = mapFallbackOptions[From, To, InnerTo](optionalTo)

            val merge = ctx match {
              case TransformationContext.ForTotal(_)             => mergeTotal(optionalTo)
              case TransformationContext.ForPartial(_, failFast) => mergePartial(optionalTo, failFast)
            }

            (ctx.config.flags.optionFallbackMerge match {
              case None =>
                srcToResult
              case Some(dsls.SourceOrElseFallback) =>
                srcToResult.parMap2(fallbackToResult)((srcTo, fallbackTo) => fallbackTo.foldLeft(srcTo)(merge))
              case Some(dsls.FallbackOrElseSource) =>
                srcToResult.parMap2(fallbackToResult)((srcTo, fallbackTo) => fallbackTo.foldRight(srcTo)(merge))
            }).flatMap(DerivationResult.expanded(_))
          }
        case _ =>
          DerivationResult.attemptNextRule
      }

    private def mapOptions[From, To, InnerFrom: Type, InnerTo: Type](
        optionalFrom: OptionalValue[From, InnerFrom],
        optionalTo: OptionalValue[To, InnerTo]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[TransformationExpr[To]] =
      ExprPromise
        .promise[InnerFrom](ExprPromise.NameGenerationStrategy.FromType)
        .traverse { (newFromExpr: Expr[InnerFrom]) =>
          useOverrideIfPresentOr("matchingSome", ctx.config.filterCurrentOverridesForSome) {
            deriveRecursiveTransformationExpr[InnerFrom, InnerTo](
              newFromExpr,
              Path(_.matching[Some[InnerFrom]].select("value")),
              Path(_.matching[Some[InnerTo]].select("value"))
            )
          }
        }
        .flatMap { (derivedToExprPromise: ExprPromise[InnerFrom, TransformationExpr[InnerTo]]) =>
          derivedToExprPromise.foldTransformationExpr { (totalP: ExprPromise[InnerFrom, Expr[InnerTo]]) =>
            // We're constructing:
            // ${ src }.fold[$To](None, innerFrom: $InnerFrom => Some(${ innerFrom }))
            // but working with every OptionalValue
            DerivationResult.totalExpr(
              optionalFrom.fold[To](
                ctx.src,
                optionalTo.empty,
                totalP.map(optionalTo.of).fulfilAsLambda[To]
              )
            )
          } { (partialP: ExprPromise[InnerFrom, Expr[partial.Result[InnerTo]]]) =>
            // We're constructing:
            // ${ src }.fold[$To](partial.Result.Value(None)) { innerFrom: $InnerFrom =>
            //   ${ derivedResultInnerTo }.map(Option(_))
            // }
            // but working with every OptionalValue
            DerivationResult.partialExpr(
              optionalFrom.fold[partial.Result[To]](
                ctx.src,
                ChimneyExpr.PartialResult.Value(optionalTo.empty).upcastToExprOf[partial.Result[To]],
                partialP
                  .map { (derivedResultTo2: Expr[partial.Result[InnerTo]]) =>
                    derivedResultTo2.map(Expr.Function1.instance { (param: Expr[InnerTo]) =>
                      optionalTo.of(param)
                    })
                  }
                  .fulfilAsLambda[partial.Result[To]]
              )
            )
          }
        }

    private def mapFallbackOptions[From, To, InnerTo: Type](optionalTo: OptionalValue[To, InnerTo])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Vector[TransformationExpr[To]]] = ctx.config.filterCurrentOverridesForFallbacks.view
      .map { case TransformerOverride.Fallback(fallback) =>
        import fallback.{Underlying as Fallback, value as fallbackExpr}
        Type[Fallback] match {
          case OptionalValue(fallback2) =>
            import fallback2.{Underlying as InnerFallback, value as optionalFallback}
            implicit val fallbackCtx: TransformationContext[Fallback, To] =
              ctx.updateFromTo[Fallback, To](fallbackExpr, updateFallbacks = _ => Vector.empty)(Fallback, ctx.To)
            Some(mapOptions[Fallback, To, InnerFallback, InnerTo](optionalFallback, optionalTo))
          case _ => None
        }
      }
      .collect { case Some(result) => result }
      .toVector
      .sequence

    private def mergeTotal[To, InnerTo](
        optionalTo: OptionalValue[To, InnerTo]
    ): (TransformationExpr[To], TransformationExpr[To]) => TransformationExpr[To] =
      (texpr1, texpt2) => TransformationExpr.fromTotal(optionalTo.orElse(texpr1.ensureTotal, texpt2.ensureTotal))

    private def mergePartial[To: Type, InnerTo](
        optionalTo: OptionalValue[To, InnerTo],
        failFast: Expr[Boolean]
    ): (TransformationExpr[To], TransformationExpr[To]) => TransformationExpr[To] = {
      case (TransformationExpr.TotalExpr(expr1), TransformationExpr.TotalExpr(expr2)) =>
        TransformationExpr.fromTotal(optionalTo.orElse(expr1, expr2))
      case (texpr1, texpr2) =>
        TransformationExpr.fromPartial(
          ChimneyExpr.PartialResult.map2(
            texpr1.ensurePartial,
            texpr2.ensurePartial,
            Expr.Function2.instance(optionalTo.orElse(_, _)),
            failFast
          )
        )
    }

  }
}
