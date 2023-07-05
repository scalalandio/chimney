package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.NotSupportedPatcherDerivation
import io.scalaland.chimney.internal.compiletime.{datatypes, ChimneyDefinitions, DerivationResult}

private[compiletime] trait Derivation
    extends ChimneyDefinitions
    with Configurations
    with Contexts
    with ImplicitSummoning
    with datatypes.IterableOrArrays
    with datatypes.ProductTypes
    with datatypes.SealedHierarchies
    with datatypes.ValueClasses {

  final def derivePatcherResultExpr[A, Patch](implicit ctx: PatcherContext[A, Patch]): DerivationResult[Expr[A]] = {
    DerivationResult.namedScope(
      s"Deriving Patcher expression for ${Type.prettyPrint[A]} with patch ${Type.prettyPrint[Patch]}"
    ) {
      (Type[A], Type[Patch]) match {
        // TODO: real impl

        case _ =>
          DerivationResult.patcherError(NotSupportedPatcherDerivation(Type.prettyPrint[A], Type.prettyPrint[Patch]))
      }
    }
  }

}
