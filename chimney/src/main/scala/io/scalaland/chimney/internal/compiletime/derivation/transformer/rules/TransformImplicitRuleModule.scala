package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.dsl.{PreferPartialTransformer, PreferTotalTransformer}
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait TransformImplicitRuleModule { this: Derivation =>

  protected object TransformImplicitRule extends Rule("Implicit") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      ctx.fold { totalCtx =>
        import totalCtx.src
        summonTransformerSafe[From, To].fold(DerivationResult.continue[To]) { transformer =>
          // We're constructing:
          // '{ ${ totalTransformer }.transform(${ src }) } }
          DerivationResult.totalExpr(transformer.callTransform(src))
        }
      } { partialCtx =>
        import partialCtx.{src, failFast}
        import partialCtx.config.flags.implicitConflictResolution
        (summonTransformerSafe[From, To], summonPartialTransformerSafe[From, To]) match {
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
              if partialOpt.isEmpty || implicitConflictResolution.contains(PreferTotalTransformer) =>
            // We're constructing:
            // '{ ${ totalTransformer }.transform(${ src }) } }
            DerivationResult.totalExpr(total.callTransform(src))
          case (totalOpt, Some(partial))
              if totalOpt.isEmpty || implicitConflictResolution.contains(PreferPartialTransformer) =>
            // We're constructing:
            // '{ ${ partialTransformer }.transform(${ src }, ${ failFast }) } }
            DerivationResult.partialExpr(partial.callTransform(src, failFast))
          case _ => DerivationResult.continue
        }
      }
  }
}
