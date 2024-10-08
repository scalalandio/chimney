package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.dsl.{PreferPartialTransformer, PreferTotalTransformer}
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial
import io.scalaland.chimney.partial.Result

private[compiletime] trait TransformImplicitOuterTransformerRuleModule { this: Derivation =>

  import ChimneyType.Implicits.*

  protected object TransformImplicitOuterTransformerRule extends Rule("ImplicitOuterTransformer") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      transformWithImplicitOuterTransformerIfAvailable[From, To]

    private def transformWithImplicitOuterTransformerIfAvailable[From, To](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] = ctx match {
      case TransformationContext.ForTotal(src) =>
        summonTotalOuterTransformer[From, To].fold(DerivationResult.attemptNextRule[To]) { totalOuterTransformer =>
          useTotalOuterTransformer(totalOuterTransformer, src, None)
        }
      case TransformationContext.ForPartial(src, failFast) =>
        import ctx.config.flags.implicitConflictResolution
        (summonTotalOuterTransformer[From, To], summonPartialOuterTransformer[From, To]) match {
          case (Some(total), Some(partial)) if implicitConflictResolution.isEmpty =>
            import total.{InnerFrom as InnerFromT, InnerTo as InnerToT}
            import partial.{InnerFrom as InnerFromP, InnerTo as InnerToP}
            DerivationResult.ambiguousImplicitOuterPriority(total.instance, partial.instance)
          case (Some(totalOuterTransformer), partialOuterTransformerOpt)
              if partialOuterTransformerOpt.isEmpty || implicitConflictResolution.contains(PreferTotalTransformer) =>
            useTotalOuterTransformer(totalOuterTransformer, src, Some(failFast))
          case (totalOuterTransformerOpt, Some(partialOuterTransformer))
              if totalOuterTransformerOpt.isEmpty || implicitConflictResolution.contains(PreferPartialTransformer) =>
            usePartialOuterTransformer(partialOuterTransformer, src, failFast)
          case _ => DerivationResult.attemptNextRule
        }
    }

    private def useTotalOuterTransformer[From, To](
        totalOuterTransformer: TotalOuterTransformer[From, To],
        src: Expr[From],
        failFast: Option[Expr[Boolean]]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] = {
      import totalOuterTransformer.{InnerFrom, InnerTo}
      ExprPromise
        .promise[InnerFrom](ExprPromise.NameGenerationStrategy.FromType, ExprPromise.UsageHint.None)
        .traverse { (innerFromExpr: Expr[InnerFrom]) =>
          deriveRecursiveTransformationExpr[InnerFrom, InnerTo](innerFromExpr, Path(_.everyItem), Path(_.everyItem))
        }
        .flatMap { promise =>
          promise.foldTransformationExpr { (onTotal: ExprPromise[InnerFrom, Expr[InnerTo]]) =>
            DerivationResult.expandedTotal(
              totalOuterTransformer.transformWithTotalInner(src, onTotal.fulfilAsLambda[InnerTo])
            )
          } { (onPartial: ExprPromise[InnerFrom, Expr[Result[InnerTo]]]) =>
            failFast.fold(
              DerivationResult.assertionError[Rule.ExpansionResult[To]]("Derived Partial Expr for Total Context")
            ) { failFast =>
              DerivationResult.expandedPartial(
                totalOuterTransformer
                  .transformWithPartialInner(src, failFast, onPartial.fulfilAsLambda[partial.Result[InnerTo]])
              )
            }
          }
        }
    }

    private def usePartialOuterTransformer[From, To](
        partialOuterTransformer: PartialOuterTransformer[From, To],
        src: Expr[From],
        failFast: Expr[Boolean]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] = {
      import partialOuterTransformer.{InnerFrom, InnerTo}
      ExprPromise
        .promise[InnerFrom](ExprPromise.NameGenerationStrategy.FromType, ExprPromise.UsageHint.None)
        .traverse { (innerFromExpr: Expr[InnerFrom]) =>
          deriveRecursiveTransformationExpr[InnerFrom, InnerTo](innerFromExpr, Path(_.everyItem), Path(_.everyItem))
        }
        .flatMap { promise =>
          promise.foldTransformationExpr { (onTotal: ExprPromise[InnerFrom, Expr[InnerTo]]) =>
            DerivationResult.expandedPartial(
              partialOuterTransformer
                .transformWithTotalInner(src, failFast, onTotal.fulfilAsLambda[InnerTo])
            )
          } { (onPartial: ExprPromise[InnerFrom, Expr[partial.Result[InnerTo]]]) =>
            DerivationResult.expandedPartial(
              partialOuterTransformer
                .transformWithPartialInner(src, failFast, onPartial.fulfilAsLambda[partial.Result[InnerTo]])
            )
          }
        }
    }
  }
}
