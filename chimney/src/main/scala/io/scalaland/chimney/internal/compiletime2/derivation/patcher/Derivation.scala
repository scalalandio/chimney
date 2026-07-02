package io.scalaland.chimney.internal.compiletime2.derivation.patcher

import io.scalaland.chimney.internal.compiletime2.{ChimneyDefinitions, DerivationResult}
import io.scalaland.chimney.internal.compiletime2.derivation.transformer

/** Hearth-based port of `...compiletime.derivation.patcher.Derivation`.
  *
  * Differences vs the old version:
  *   - the `datatypes.*` traits are NOT mixed in anymore - [[ChimneyDefinitions]] already includes them (they became
  *     part of the compiletime2 foundation),
  *   - the patcher rule modules are mixed in HERE instead of in the per-platform `DerivationPlatform`s, and
  *     `rulesAvailableForPlatform` (identical between the old platforms) is overridden HERE, once, in shared code -
  *     mirroring what the transformer's [[transformer.Derivation]] does for the transformer rules.
  */
private[compiletime2] trait Derivation
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

  /** The old per-platform patcher `DerivationPlatform.rulesAvailableForPlatform` (identical on Scala 2 and 3), now
    * shared - overrides the transformer-only list from [[transformer.Derivation]].
    */
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

  final def derivePatcherResultExpr[A, Patch](implicit ctx: PatcherContext[A, Patch]): DerivationResult[Expr[A]] =
    DerivationResult.namedScope(
      s"Deriving Patcher expression for ${Type.prettyPrint[A]} with patch ${Type.prettyPrint[Patch]} with context:\n$ctx"
    ) {
      DerivationResult.log(
        s"Patching expression will be derived as total transformation from ${Type.prettyPrint[Patch]} to ${Type.prettyPrint[A]} with original ${Type.prettyPrint[A]} as fallback"
      ) >>
        deriveTransformationResultExpr(ctx.toTransformerContext).map(_.ensureTotal)
    }
}
