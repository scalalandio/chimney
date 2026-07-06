package io.scalaland.chimney.internal.compiletime.derivation.transformer

import hearth.fp.effect.{Log, MIO}
import io.scalaland.chimney.internal.compiletime.{ChimneyDefinitions, DerivationError}

/** Rule order in [[rulesAvailableForPlatform]] matters. NB: TransformPartialOptionToNonOptionRule and
  * TransformToOptionRule have disjoint conditions at their pipeline position (target optional vs target non-optional),
  * so their relative order is free.
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
    with ChimneyEngineExtensionApi
    with rules.TransformationRules
    with rules.TransformImplicitRuleModule
    with rules.TransformImplicitPartialFallbackToTotalRuleModule
    with rules.TransformImplicitOuterTransformerRuleModule
    with rules.TransformImplicitConversionRuleModule
    with rules.TransformSpecialCasedRuleModule
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

  override protected val rulesAvailableForPlatform: List[Rule] = List(
    TransformImplicitRule,
    TransformImplicitPartialFallbackToTotalRule,
    TransformImplicitOuterTransformerRule,
    TransformImplicitConversionRule,
    // SPI: engine-aware ChimneyMacroExtension handlers. Below the implicit rules (user/integration implicits win),
    // above the built-in structural rules (a registered handler beats Chimney's default derivation).
    TransformSpecialCasedRule,
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
  ): MIO[TransformationExpr[To]] =
    deriveTransformationResultExprUpdatingRules[From, To](identity)

  /** Intended use case: shared logic between what Gateway uses and recursive derivation uses */
  final private def deriveTransformationResultExprUpdatingRules[From, To](
      updateRules: List[Rule] => List[Rule]
  )(implicit
      ctx: TransformationContext[From, To]
  ): MIO[TransformationExpr[To]] =
    // Cheap constant scope name; the expensive per-derivation detail (prettyPrints + full context dump) is moved into a
    // by-name Log.info that is only built when Info logging is actually rendered.
    Log.namedScope(
      ctx.fold(_ => "Deriving Total Transformer expression")(_ => "Deriving Partial Transformer expression")
    ) {
      // $COVERAGE-OFF$scope detail is only built when Info logging is rendered (off by default, incl. in tests)
      Log.info(
        ctx.fold(_ =>
          s"Deriving Total Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]} with context:\n$ctx"
        )(_ =>
          s"Deriving Partial Transformer expression from ${Type.prettyPrint[From]} to ${Type.prettyPrint[To]} with context:\n$ctx"
        )
      ) >>
        // $COVERAGE-ON$
        Rule.expandRules[From, To](updateRules(rulesAvailableForPlatform))
    }

  /** Intended use case: recursive derivation within rules */
  final protected def deriveRecursiveTransformationExpr[NewFrom: Type, NewTo: Type](
      newSrc: Expr[NewFrom],
      followFrom: Path = Path.Root,
      followTo: Path = Path.Root,
      updateFallbacks: TransformerOverride.ForFallback => Vector[TransformerOverride.ForFallback] = Vector(_),
      updateRules: List[Rule] => List[Rule] = identity
  )(implicit ctx: TransformationContext[?, ?]): MIO[TransformationExpr[NewTo]] = {
    val newCtx: TransformationContext[NewFrom, NewTo] =
      ctx.updateFromTo[NewFrom, NewTo](newSrc, followFrom, followTo, updateFallbacks)
    deriveTransformationResultExprUpdatingRules(updateRules)(newCtx).log
      .valueAsInfo {
        case TransformationExpr.TotalExpr(expr)   => s"Derived recursively total expression ${Expr.prettyPrint(expr)}"
        case TransformationExpr.PartialExpr(expr) => s"Derived recursively partial expression ${Expr.prettyPrint(expr)}"
      }
      .log
      .errorsAsInfo(errors => s"Errors at recursive derivation: ${DerivationError.printErrors(errors)}")
  }
}
