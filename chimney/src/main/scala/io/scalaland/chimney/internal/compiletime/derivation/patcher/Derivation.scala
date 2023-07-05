package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.NotSupportedPatcherDerivation
import io.scalaland.chimney.internal.compiletime.fp.Implicits.*
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

  final def derivePatcher[A, Patch](implicit ctx: PatcherContext[A, Patch]): DerivationResult[Expr[A]] = {
    DerivationResult.namedScope(
      s"Deriving Patcher expression for ${Type.prettyPrint[A]} with patch ${Type.prettyPrint[Patch]}"
    ) {
      (Type[A], Type[Patch], Type[A]) match {
        case (
              Product.Extraction(objExtractors),
              Product.Extraction(patchExtractors),
              Product.Constructor(objParameters, objConstructor)
            ) =>
          val patchGetters = patchExtractors.filter(_._2.value.sourceType == Product.Getter.SourceType.ConstructorVal)
          val targetParams =
            objParameters.filter(_._2.value.targetType == Product.Parameter.TargetType.ConstructorParameter)
          val targetGetters = objExtractors.filter(_._2.value.sourceType == Product.Getter.SourceType.ConstructorVal)

          patchGetters.toList
            .parTraverse { case (patchFieldName, patchGetter) =>
              resolvePatchMapping[A, Patch](patchFieldName, patchGetter).map(_.map(patchFieldName -> _))
            }
            .map(_.flatten.toMap)
            .flatMap { patchMapping =>
              val patchedArgs = targetParams.map { case (targetParamName, _) =>
                targetParamName -> patchMapping.getOrElse(
                  targetParamName,
                  targetGetters(targetParamName).mapK[Expr] { _ => getter => getter.get(ctx.obj) }
                )
              }

              DerivationResult.pure(objConstructor(patchedArgs))
            }

        case _ =>
          DerivationResult.patcherError(NotSupportedPatcherDerivation(Type.prettyPrint[A], Type.prettyPrint[Patch]))
      }
    }
  }

  private def resolvePatchMapping[A, Patch](patchFieldName: String, patchGetter: Existential[Product.Getter[Patch, *]])(
      implicit ctx: PatcherContext[A, Patch]
  ): DerivationResult[Option[ExistentialExpr]] = {
    // TODO: real impl
    DerivationResult.pure(None)
  }

}
