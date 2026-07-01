package io.scalaland.chimney.internal.compiletime2.derivation.patcher

import io.scalaland.chimney.internal.compiletime2.{ChimneyDefinitions, DerivationResult}
import io.scalaland.chimney.internal.compiletime2.derivation.transformer

/** Hearth-based port of `...compiletime.derivation.patcher.Derivation`.
  *
  * Differences vs the old version: the `datatypes.*` traits are NOT mixed in anymore - [[ChimneyDefinitions]] already
  * includes them (they became part of the compiletime2 foundation).
  */
private[compiletime2] trait Derivation
    extends ChimneyDefinitions
    with Configurations
    with Contexts
    with ImplicitSummoning
    with transformer.Derivation {
  this: hearth.MacroCommons & hearth.std.StdExtensions =>

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
