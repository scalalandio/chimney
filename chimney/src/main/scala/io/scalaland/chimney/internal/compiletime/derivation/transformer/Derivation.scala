package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}
import io.scalaland.chimney.internal.compiletime.datatypes

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Derivation
    extends Definitions
    with ResultOps
    with ImplicitSummoning
    with datatypes.ValueClasses
    with rules.TransformationRules {

  /** Intended use case: starting recursive derivation from Gateway */
  final protected def deriveTransformationResultExpr[From, To](implicit
      ctx: TransformationContext[From, To]
  ): DerivationResult[TransformationExpr[To]] =
    DerivationResult.namedScope(
      ctx match {
        case _: TransformationContext.ForTotal[?, ?] =>
          s"Deriving Total Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}"
        case _: TransformationContext.ForPartial[?, ?] =>
          s"Deriving Partial Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]}"
      }
    ) {
      Rule.expandRules[From, To](rulesAvailableForPlatform)
    }

  /** Intended use case: recursive derivation within rules */
  final protected def deriveRecursiveTransformationExpr[NewFrom: Type, NewTo: Type](
      newSrc: Expr[NewFrom]
  )(implicit ctx: TransformationContext[?, ?]): DerivationResult[TransformationExpr[NewTo]] = {
    val newCtx: TransformationContext[NewFrom, NewTo] = ctx.updateFromTo[NewFrom, NewTo](newSrc).updateConfig {
      _.prepareForRecursiveCall
    }
    deriveTransformationResultExpr(newCtx)
      .logSuccess {
        case TransformationExpr.TotalExpr(expr)   => s"Derived recursively total expression ${Expr.prettyPrint(expr)}"
        case TransformationExpr.PartialExpr(expr) => s"Derived recursively partial expression ${Expr.prettyPrint(expr)}"
      }
  }
}
