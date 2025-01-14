package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.compiletime.{datatypes, ChimneyDefinitions, DerivationResult}
import io.scalaland.chimney.internal.compiletime.derivation.transformer

private[compiletime] trait Derivation
    extends ChimneyDefinitions
    with Configurations
    with Contexts
    with ImplicitSummoning
    with datatypes.IterableOrArrays
    with datatypes.ProductTypes
    with datatypes.SealedHierarchies
    with datatypes.ValueClasses
    with transformer.Derivation {

  // TODO: translate errors?
  // - NotSupportedPatcherDerivation
  // - PatchFieldNotFoundInTargetObj

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
