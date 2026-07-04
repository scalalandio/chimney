package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.MIO
import hearth.fp.syntax.*
import io.scalaland.chimney.dsl.{PreferPartialTransformer, PreferTotalTransformer}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial
import io.scalaland.chimney.partial.Result

private[compiletime] trait TransformImplicitOuterTransformerRuleModule {
  this: Derivation & TransformProductToProductRuleModule & hearth.MacroCommons =>

  import ChimneyType.Implicits.*, TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformImplicitOuterTransformerRule extends Rule("ImplicitOuterTransformer") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      transformWithImplicitOuterTransformerIfAvailable[From, To]

    private def transformWithImplicitOuterTransformerIfAvailable[From, To](implicit
        ctx: TransformationContext[From, To]
    ): MIO[Rule.ExpansionResult[To]] = ctx match {
      case TransformationContext.ForTotal(src) =>
        summonTotalOuterTransformer[From, To].fold(attemptNextRule[To]) { totalOuterTransformer =>
          useTotalOuterTransformer(totalOuterTransformer, src, None)
        }
      case TransformationContext.ForPartial(src, failFast) =>
        import ctx.config.flags.implicitConflictResolution
        (summonTotalOuterTransformer[From, To], summonPartialOuterTransformer[From, To]) match {
          case (Some(total), Some(partial)) if implicitConflictResolution.isEmpty =>
            import total.{InnerFrom as InnerFromT, InnerTo as InnerToT}
            import partial.{InnerFrom as InnerFromP, InnerTo as InnerToP}
            ambiguousImplicitOuterPriority(total.instance, partial.instance)
          case (Some(totalOuterTransformer), partialOuterTransformerOpt)
              if partialOuterTransformerOpt.isEmpty || implicitConflictResolution.contains(PreferTotalTransformer) =>
            useTotalOuterTransformer(totalOuterTransformer, src, Some(failFast))
          case (totalOuterTransformerOpt, Some(partialOuterTransformer))
              if totalOuterTransformerOpt.isEmpty || implicitConflictResolution.contains(PreferPartialTransformer) =>
            usePartialOuterTransformer(partialOuterTransformer, src, failFast)
          case _ => attemptNextRule
        }
    }

    private def useTotalOuterTransformer[From, To](
        totalOuterTransformer: TotalOuterTransformer[From, To],
        src: Expr[From],
        failFast: Option[Expr[Boolean]]
    )(implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] = {
      import totalOuterTransformer.{InnerFrom, InnerTo}
      LambdaBuilder
        .of1[InnerFrom]()
        .traverse { (innerFromExpr: Expr[InnerFrom]) =>
          useOverrideIfPresentOr("everyItem", ctx.config.filterCurrentOverridesForEveryItem) {
            deriveRecursiveTransformationExpr[InnerFrom, InnerTo](innerFromExpr, Path(_.everyItem), Path(_.everyItem))
          }
        }
        .flatMap { (builder: LambdaBuilder[InnerFrom => *, TransformationExpr[InnerTo]]) =>
          builder.foldTransformationExpr { (onTotal: LambdaBuilder[InnerFrom => *, Expr[InnerTo]]) =>
            expandedTotal(
              totalOuterTransformer.transformWithTotalInner(src, onTotal.build[InnerTo])
            )
          } { (onPartial: LambdaBuilder[InnerFrom => *, Expr[Result[InnerTo]]]) =>
            failFast.fold(
              MIO.fail[Rule.ExpansionResult[To]](new AssertionError("Derived Partial Expr for Total Context"))
            ) { failFast =>
              expandedPartial(
                totalOuterTransformer
                  .transformWithPartialInner(src, failFast, onPartial.build[partial.Result[InnerTo]])
              )
            }
          }
        }
    }

    private def usePartialOuterTransformer[From, To](
        partialOuterTransformer: PartialOuterTransformer[From, To],
        src: Expr[From],
        failFast: Expr[Boolean]
    )(implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] = {
      import partialOuterTransformer.{InnerFrom, InnerTo}
      LambdaBuilder
        .of1[InnerFrom]()
        .traverse { (innerFromExpr: Expr[InnerFrom]) =>
          useOverrideIfPresentOr("everyItem", ctx.config.filterCurrentOverridesForEveryItem) {
            deriveRecursiveTransformationExpr[InnerFrom, InnerTo](innerFromExpr, Path(_.everyItem), Path(_.everyItem))
          }
        }
        .flatMap { (builder: LambdaBuilder[InnerFrom => *, TransformationExpr[InnerTo]]) =>
          builder.foldTransformationExpr { (onTotal: LambdaBuilder[InnerFrom => *, Expr[InnerTo]]) =>
            expandedPartial(
              partialOuterTransformer
                .transformWithTotalInner(src, failFast, onTotal.build[InnerTo])
            )
          } { (onPartial: LambdaBuilder[InnerFrom => *, Expr[partial.Result[InnerTo]]]) =>
            expandedPartial(
              partialOuterTransformer
                .transformWithPartialInner(src, failFast, onPartial.build[partial.Result[InnerTo]])
            )
          }
        }
    }
  }
}
