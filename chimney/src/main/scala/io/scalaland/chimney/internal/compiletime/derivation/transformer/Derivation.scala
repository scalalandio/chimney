package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.{PreferPartialTransformer, PreferTotalTransformer}
import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}
import io.scalaland.chimney.partial

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Derivation extends Definitions with ResultOps {

  final protected def summonTransformerForTransformationResultExpr[From, To](implicit
      ctx: TransformerContext[From, To]
  ): Option[DerivedExpr[To]] = ctx match {
    case totalCtx: TransformerContext.ForTotal[?, ?] =>
      ChimneyExpr.Transformer.summon[From, To].map { transformer =>
        DerivedExpr.TotalExpr(ChimneyExpr.Transformer.transform(transformer, totalCtx.src))
      }
    case partialCtx: TransformerContext.ForPartial[?, ?] =>
      import partialCtx.{src, failFast}
      import partialCtx.config.flags.implicitConflictResolution
      (ChimneyExpr.Transformer.summon[From, To], ChimneyExpr.PartialTransformer.summon[From, To]) match {
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
        case (Some(total), partialOpt) if partialOpt.isEmpty || implicitConflictResolution == PreferTotalTransformer =>
          Some(DerivedExpr.TotalExpr(ChimneyExpr.Transformer.transform(total, src)))
        case (totalOpt, Some(partial)) if totalOpt.isEmpty || implicitConflictResolution == PreferPartialTransformer =>
          Some(DerivedExpr.PartialExpr(ChimneyExpr.PartialTransformer.transform(partial, src, failFast)))
        case _ => None
      }
  }

  /** Intended use case: recursive derivation */
  final protected def deriveTransformationResultExpr[From, To](implicit
      ctx: TransformerContext[From, To]
  ): DerivationResult[DerivedExpr[To]] =
    DerivationResult.namedScope(
      ctx match {
        case _: TransformerContext.ForTotal[?, ?] =>
          s"Deriving Total Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}"
        case _: TransformerContext.ForPartial[?, ?] =>
          s"Deriving Partial Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}"
      }
    ) {
      Rule.expandRules[From, To](rulesAvailableForPlatform)
// TODO: move logging below to the place when we exit recursive call
//        .logSuccess {
//          case DerivedExpr.TotalExpr(expr)   => s"Derived total expression ${Expr.prettyPrint(expr)}"
//          case DerivedExpr.PartialExpr(expr) => s"Derived partial expression ${Expr.prettyPrint(expr)}"
//        }
    }

  /** Intended use case: recursive derivation when we want to attempt summoning first */
  final protected def summonOrElseDeriveTransformationResultExpr[From, To](implicit
      ctx: TransformerContext[From, To]
  ): DerivationResult[DerivedExpr[To]] = summonTransformerForTransformationResultExpr[From, To] match {
    case Some(derivedExpr) => DerivationResult.pure(derivedExpr)
    case None              => deriveTransformationResultExpr[From, To]
  }

  abstract protected class Rule(val name: String) {

    def expand[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[Rule.ExpansionResult[To]]
  }

  protected object Rule {

    sealed trait ExpansionResult[+A]

    object ExpansionResult {
      // successfully expanded transformation expr
      case class Expanded[A](transformationExpr: DerivedExpr[A]) extends ExpansionResult[A]
      // continue expansion with another rule on the list
      case object Continue extends ExpansionResult[Nothing]
    }

    def expandRules[From, To](
        rules: List[Rule]
    )(implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] = {
      rules match {
        case Nil =>
          DerivationResult.notSupportedTransformerDerivation
        case rule :: nextRules =>
          DerivationResult
            .namedScope(s"Attempting expansion of rule ${rule.name}")(
              rule.expand[From, To].logFailure { errors => errors.prettyPrint }
            )
            .flatMap {
              case ExpansionResult.Expanded(transformationExpr) =>
                DerivationResult
                  .log(s"Rule ${rule.name} expanded successfully")
                  .as(transformationExpr.asInstanceOf[DerivedExpr[To]])
              case ExpansionResult.Continue =>
                DerivationResult.log(s"Rule ${rule.name} decided to continue expansion") >>
                  expandRules[From, To](nextRules)
            }
      }
    }
  }

  sealed protected trait DerivedExpr[A] // TODO: rename to TransformationExpr

  protected object DerivedExpr {
    final case class TotalExpr[A](expr: Expr[A]) extends DerivedExpr[A]
    final case class PartialExpr[A](expr: Expr[partial.Result[A]]) extends DerivedExpr[A]
  }

  protected val rulesAvailableForPlatform: List[Rule]
}
