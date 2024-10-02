package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{datatypes, ChimneyDefinitions, DerivationResult}

private[compiletime] trait Derivation
    extends ChimneyDefinitions
    with Configurations
    with Contexts
    with ImplicitSummoning
    with ResultOps
    with datatypes.IterableOrArrays
    with datatypes.ProductTypes
    with datatypes.SealedHierarchies
    with datatypes.SingletonTypes
    with datatypes.ValueClasses
    with integrations.OptionalValues
    with integrations.PartiallyBuildIterables
    with integrations.TotallyBuildIterables
    with integrations.TotallyOrPartiallyBuildIterables
    with rules.TransformationRules {

  /** Intended use case: starting recursive derivation from Gateway */
  final protected def deriveTransformationResultExpr[From, To](implicit
      ctx: TransformationContext[From, To]
  ): DerivationResult[TransformationExpr[To]] =
    deriveTransformationResultExprUpdatingRules[From, To](identity)

  /** Intended use case: shared logic between what Gateway uses and recursive derivation uses */
  final private def deriveTransformationResultExprUpdatingRules[From, To](
      updateRules: List[Rule] => List[Rule]
  )(implicit
      ctx: TransformationContext[From, To]
  ): DerivationResult[TransformationExpr[To]] =
    DerivationResult.namedScope(
      ctx.fold(_ =>
        s"Deriving Total Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]} with context:\n$ctx"
      )(_ =>
        s"Deriving Partial Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]} with context:\n$ctx"
      )
    ) {
      Rule.expandRules[From, To](updateRules(rulesAvailableForPlatform))
    }

  /** Intended use case: recursive derivation within rules */
  final protected def deriveRecursiveTransformationExpr[NewFrom: Type, NewTo: Type](
      newSrc: Expr[NewFrom],
      followFrom: Path = Path.Root,
      followTo: Path = Path.Root,
      updateRules: List[Rule] => List[Rule] = identity
  )(implicit ctx: TransformationContext[?, ?]): DerivationResult[TransformationExpr[NewTo]] = {
    val newCtx: TransformationContext[NewFrom, NewTo] =
      ctx
        .updateFromTo[NewFrom, NewTo](newSrc)
        .updateConfig(_.prepareForRecursiveCall(followFrom, followTo))
    deriveTransformationResultExprUpdatingRules(updateRules)(newCtx)
      .logSuccess {
        case TransformationExpr.TotalExpr(expr)   => s"Derived recursively total expression ${Expr.prettyPrint(expr)}"
        case TransformationExpr.PartialExpr(expr) => s"Derived recursively partial expression ${Expr.prettyPrint(expr)}"
      }
      .logFailure(errors => s"Errors at recursive derivation: ${errors.prettyPrint}")
  }
}
