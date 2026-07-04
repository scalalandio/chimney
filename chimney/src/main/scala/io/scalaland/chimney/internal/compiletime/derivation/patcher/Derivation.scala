package io.scalaland.chimney.internal.compiletime.derivation.patcher

import hearth.fp.effect.{Log, MIO}
import io.scalaland.chimney.internal.compiletime.ChimneyDefinitions
import io.scalaland.chimney.internal.compiletime.derivation.transformer

private[compiletime] trait Derivation
    extends ChimneyDefinitions
    with Configurations
    with Contexts
    with ImplicitSummoning
    with transformer.Derivation
    with rules.PatchImplicitRuleModule
    with rules.PatchSubtypeRuleModule
    with rules.PatchOptionWithOptionOptionRuleModule
    with rules.PatchEitherWithOptionEitherRuleModule
    with rules.PatchCollectionWithOptionCollectionRuleModule
    with rules.PatchOptionWithNonOptionRuleModule
    with rules.PatchProductWithProductRuleModule
    with rules.PatchNotMatchedRuleModule {
  this: hearth.MacroCommons & hearth.std.StdExtensions =>

  /** Overrides the transformer-only list from [[transformer.Derivation]]. */
  override protected val rulesAvailableForPlatform: List[Rule] = List(
    PatchImplicitRule,
    TransformImplicitRule,
    TransformImplicitOuterTransformerRule,
    TransformImplicitConversionRule,
    PatchSubtypeRuleModule,
    TransformTypeConstraintRule,
    PatchOptionWithNonOptionRule,
    PatchOptionWithOptionOptionRule,
    PatchEitherWithOptionEitherRule,
    PatchCollectionWithOptionCollectionRule,
    TransformOptionToOptionRule,
    TransformToOptionRule,
    TransformToSingletonRule,
    TransformValueClassToValueClassRule,
    TransformValueClassToTypeRule,
    TransformTypeToValueClassRule,
    TransformEitherToEitherRule,
    TransformMapToMapRule,
    TransformIterableToIterableRule,
    TransformSubtypesRule,
    PatchProductWithProductRule,
    TransformSealedHierarchyToSealedHierarchyRule,
    PatchNotMatchedRule
  )

  final def derivePatcherResultExpr[A, Patch](implicit ctx: PatcherContext[A, Patch]): MIO[Expr[A]] =
    Log.namedScope(
      s"Deriving Patcher expression for ${Type.prettyPrint[A]} with patch ${Type.prettyPrint[Patch]} with context:\n$ctx"
    ) {
      Log.info(
        s"Patching expression will be derived as total transformation from ${Type.prettyPrint[Patch]} to ${Type.prettyPrint[A]} with original ${Type.prettyPrint[A]} as fallback"
      ) >>
        deriveTransformationResultExpr(ctx.toTransformerContext).map(_.ensureTotal)
    }
}
