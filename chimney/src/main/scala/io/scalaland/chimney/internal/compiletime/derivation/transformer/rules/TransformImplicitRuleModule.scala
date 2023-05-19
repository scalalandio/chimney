package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.dsl.{PreferPartialTransformer, PreferTotalTransformer}
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
trait TransformImplicitRuleModule { this: Derivation =>

  object TransformImplicitRule extends Rule("Implicit") {

    def expand[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      ctx match {
        case totalCtx: TransformerContext.ForTotal[?, ?] =>
          summonTransformer[From, To].fold(DerivationResult.continue[To]) { transformer =>
            DerivationResult.totalExpr(transformer.callTransform(totalCtx.src))
          }
        case partialCtx: TransformerContext.ForPartial[?, ?] =>
          import partialCtx.{src, failFast}
          import partialCtx.config.flags.implicitConflictResolution
          (summonTransformer[From, To], summonPartialTransformer[From, To]) match {
            case (Some(total), Some(partial)) if implicitConflictResolution.isEmpty =>
              reportError(
                s"""Ambiguous implicits while resolving Chimney recursive transformation:
                   |
                   |PartialTransformer[${Type.prettyPrint[From]}, ${Type.prettyPrint[To]}]: ${Expr.prettyPrint(total)}
                   |Transformer[${Type.prettyPrint[From]}, ${Type.prettyPrint[To]}]: ${Expr.prettyPrint(partial)}
                   |
                   |Please eliminate ambiguity from implicit scope or use enableImplicitConflictResolution/withFieldComputed/withFieldComputedPartial to decide which one should be used
                   |""".stripMargin
              )
            case (Some(total), partialOpt)
                if partialOpt.isEmpty || implicitConflictResolution == PreferTotalTransformer =>
              DerivationResult.totalExpr(total.callTransform(src))
            case (totalOpt, Some(partial))
                if totalOpt.isEmpty || implicitConflictResolution == PreferPartialTransformer =>
              DerivationResult.partialExpr(partial.callTransform(src, failFast))
            case _ => DerivationResult.continue
          }
      }
  }
}
