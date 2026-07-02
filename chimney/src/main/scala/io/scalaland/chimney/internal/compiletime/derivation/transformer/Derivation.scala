package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{ChimneyDefinitions, DerivationResult}

/** Hearth-based port of `...compiletime.derivation.transformer.Derivation`.
  *
  * Differences vs the old version:
  *   - the `datatypes.*` traits are NOT mixed in here anymore - [[ChimneyDefinitions]] already includes them (they
  *     became part of the compiletime foundation),
  *   - the rule modules are mixed in HERE instead of in the per-platform `DerivationPlatform`s (they are shared code
  *     now; the old platform division only existed for quasiquotes-vs-quotes implementations),
  *   - the self-type mirrors [[ChimneyDefinitions]]'s (the cake is completed by the platform bridges),
  *   - `rulesAvailableForPlatform` is defined HERE, once, in shared code (the old per-platform lists were identical
  *     except for the TransformPartialOptionToNonOptionRule/TransformToOptionRule order - their conditions are disjoint
  *     at that pipeline position (target optional vs target non-optional), so the old Scala 3 order is used for both
  *     platforms).
  */
private[compiletime] trait Derivation
    extends ChimneyDefinitions
    with Configurations
    with Contexts
    with ImplicitSummoning
    with ResultOps
    with integrations.TotalOuterTransformers
    with integrations.PartialOuterTransformers
    with integrations.OptionalValues
    with integrations.PartiallyBuildIterables
    with integrations.TotallyBuildIterables
    with integrations.TotallyOrPartiallyBuildIterables
    with rules.TransformationRules
    with rules.TransformImplicitRuleModule
    with rules.TransformImplicitPartialFallbackToTotalRuleModule
    with rules.TransformImplicitOuterTransformerRuleModule
    with rules.TransformImplicitConversionRuleModule
    with rules.TransformSubtypesRuleModule
    with rules.TransformTypeConstraintRuleModule
    with rules.TransformToSingletonRuleModule
    with rules.TransformValueClassToValueClassRuleModule
    with rules.TransformValueClassToTypeRuleModule
    with rules.TransformTypeToValueClassRuleModule
    with rules.TransformOptionToOptionRuleModule
    with rules.TransformPartialOptionToNonOptionRuleModule
    with rules.TransformToOptionRuleModule
    with rules.TransformEitherToEitherRuleModule
    with rules.TransformMapToMapRuleModule
    with rules.TransformIterableToIterableRuleModule
    with rules.TransformProductToProductRuleModule
    with rules.TransformSealedHierarchyToSealedHierarchyRuleModule {
  this: hearth.MacroCommons & hearth.std.StdExtensions =>

  /** The old per-platform `DerivationPlatform.rulesAvailableForPlatform`, now shared (see the trait's ScalaDoc). */
  override protected val rulesAvailableForPlatform: List[Rule] = List(
    TransformImplicitRule,
    TransformImplicitPartialFallbackToTotalRule,
    TransformImplicitOuterTransformerRule,
    TransformImplicitConversionRule,
    TransformSubtypesRule,
    TransformTypeConstraintRule,
    TransformToSingletonRule,
    TransformValueClassToValueClassRule,
    TransformValueClassToTypeRule,
    TransformTypeToValueClassRule,
    TransformOptionToOptionRule,
    TransformPartialOptionToNonOptionRule,
    TransformToOptionRule,
    TransformEitherToEitherRule,
    TransformMapToMapRule,
    TransformIterableToIterableRule,
    TransformProductToProductRule,
    TransformSealedHierarchyToSealedHierarchyRule
  )

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
      updateFallbacks: TransformerOverride.ForFallback => Vector[TransformerOverride.ForFallback] = Vector(_),
      updateRules: List[Rule] => List[Rule] = identity
  )(implicit ctx: TransformationContext[?, ?]): DerivationResult[TransformationExpr[NewTo]] = {
    val newCtx: TransformationContext[NewFrom, NewTo] =
      ctx.updateFromTo[NewFrom, NewTo](newSrc, followFrom, followTo, updateFallbacks)
    deriveTransformationResultExprUpdatingRules(updateRules)(newCtx)
      .logSuccess {
        case TransformationExpr.TotalExpr(expr)   => s"Derived recursively total expression ${Expr.prettyPrint(expr)}"
        case TransformationExpr.PartialExpr(expr) => s"Derived recursively partial expression ${Expr.prettyPrint(expr)}"
      }
      .logFailure(errors => s"Errors at recursive derivation: ${errors.prettyPrint}")
  }
}
