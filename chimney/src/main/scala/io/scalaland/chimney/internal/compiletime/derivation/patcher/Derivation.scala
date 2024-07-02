package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.compiletime.fp.Implicits.*
import io.scalaland.chimney.internal.compiletime.{
  datatypes,
  ChimneyDefinitions,
  DerivationErrors,
  DerivationResult,
  NotSupportedPatcherDerivation,
  PatchFieldNotFoundInTargetObj
}
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

  import Type.Implicits.*

  final def derivePatcherResultExpr[A, Patch](implicit ctx: PatcherContext[A, Patch]): DerivationResult[Expr[A]] =
    DerivationResult.log(s"Start derivation with context: $ctx") >>
      summonPatcherSafe[A, Patch]
        .map { patcherExpr =>
          DerivationResult.pure(patcherExpr.patch(ctx.obj, ctx.patch))
        }
        .getOrElse {
          DerivationResult.namedScope(
            s"Deriving Patcher expression for ${Type.prettyPrint[A]} with patch ${Type.prettyPrint[Patch]}"
          ) {
            (Type[A], Type[Patch]) match {
              case (
                    ProductType(
                      Product(Product.Extraction(objExtractors), Product.Constructor(objParameters, objConstructor))
                    ),
                    Product.Extraction(patchExtractors)
                  ) =>
                val patchGetters =
                  patchExtractors.filter(_._2.value.sourceType == Product.Getter.SourceType.ConstructorVal)
                val targetParams =
                  objParameters.filter(_._2.value.targetType == Product.Parameter.TargetType.ConstructorParameter)
                val targetGetters =
                  objExtractors.filter(_._2.value.sourceType == Product.Getter.SourceType.ConstructorVal)

                patchGetters.toList
                  .parTraverse { case (patchFieldName, patchGetter) =>
                    resolvePatchMapping[A, Patch](patchFieldName, patchGetter, targetGetters, targetParams)
                      .map(_.map(patchFieldName -> _))
                  }
                  .map(_.flatten.toMap)
                  .flatMap { patchMapping =>
                    val patchedArgs = targetParams.map { case (targetParamName, _) =>
                      targetParamName -> patchMapping.getOrElse(
                        targetParamName,
                        targetGetters(targetParamName).mapK[Expr](_ => getter => getter.get(ctx.obj))
                      )
                    }

                    DerivationResult.pure(objConstructor(patchedArgs))
                  }

              case _ =>
                DerivationResult.patcherError(
                  NotSupportedPatcherDerivation(Type.prettyPrint[A], Type.prettyPrint[Patch])
                )
            }
          }
        }

  private def resolvePatchMapping[A: Type, Patch: Type](
      patchFieldName: String,
      patchGetter: Existential[Product.Getter[Patch, *]],
      targetGetters: Map[String, Existential[Product.Getter[A, *]]],
      targetParams: Map[String, Existential[Product.Parameter]]
  )(implicit
      ctx: PatcherContext[A, Patch]
  ): DerivationResult[Option[ExistentialExpr]] = {
    import patchGetter.Underlying as PatchGetter

    def patchGetterExpr: ExistentialExpr =
      patchGetter.mapK[Expr](_ => getter => getter.get(ctx.patch))

    targetParams.get(patchFieldName) match {
      case Some(targetParam) =>
        import targetParam.Underlying as TargetParam

        if (ctx.config.flags.ignoreNoneInPatch && PatchGetter.isOption && TargetParam.isOption) {
          (PatchGetter, TargetParam) match {
            case (Type.Option(getterInner), Type.Option(targetInner)) =>
              val targetGetter = targetGetters(patchFieldName)
              import targetGetter.Underlying as TargetGetter, getterInner.Underlying as GetterInner,
                targetInner.Underlying as TargetInner
              deriveTransformerForPatcherField[Option[GetterInner], Option[TargetInner]](src =
                patchGetter.value.get(ctx.patch).asInstanceOfExpr[Option[GetterInner]]
              )
                .map { (transformedExpr: Expr[Option[TargetInner]]) =>
                  Some(
                    transformedExpr.orElse(targetGetter.value.get(ctx.obj).upcastToExprOf[Option[TargetInner]]).as_??
                  )
                }
            case _ =>
              // $COVERAGE-OFF$should never happen unless we messed up
              assertionFailed(
                s"Expected both types to be options, got ${Type.prettyPrint[PatchGetter]} and ${Type.prettyPrint[TargetParam]}"
              )
            // $COVERAGE-ON$
          }
        } else if (PatchGetter <:< TargetParam) {
          DerivationResult.pure(Some(patchGetterExpr))
        } else {
          deriveTransformerForPatcherField[PatchGetter, TargetParam](src = patchGetter.value.get(ctx.patch))
            .map { (transformedExpr: Expr[TargetParam]) =>
              Some(transformedExpr.as_??)
            }
            .recoverWith { (errors: DerivationErrors) =>
              PatchGetter match {
                case Type.Option(inner) =>
                  val targetGetter = targetGetters(patchFieldName)
                  import inner.Underlying as Inner, targetGetter.Underlying as TargetGetter
                  PrependDefinitionsTo
                    .prependVal[Option[Inner]](
                      patchGetter.value.get(ctx.patch).upcastToExprOf[Option[Inner]],
                      ExprPromise.NameGenerationStrategy.FromPrefix(patchFieldName)
                    )
                    .traverse { (option: Expr[Option[Inner]]) =>
                      deriveTransformerForPatcherField[Inner, TargetParam](
                        src = option.get
                      ).map { (transformedExpr: Expr[TargetParam]) =>
                        Expr.ifElse(option.isDefined)(transformedExpr)(
                          targetGetter.value.get(ctx.obj).upcastToExprOf[TargetParam]
                        )
                      }
                    }
                    .map { (targetExprBlock: PrependDefinitionsTo[Expr[TargetParam]]) =>
                      Some(targetExprBlock.closeBlockAsExprOf[TargetParam].as_??)
                    }
                case _ =>
                  DerivationResult.fail(errors)
              }
            }
        }

      case None =>
        if (ctx.config.flags.ignoreRedundantPatcherFields)
          DerivationResult.pure(None)
        else
          DerivationResult.patcherError(PatchFieldNotFoundInTargetObj(patchFieldName, Type.prettyPrint(ctx.A)))
    }
  }

  private def deriveTransformerForPatcherField[From: Type, To: Type](src: Expr[From]): DerivationResult[Expr[To]] = {
    val context = TransformationContext.ForTotal
      .create[From, To](
        src,
        TransformerConfiguration()
      )
      .updateConfig(_.allowFromToImplicitSummoning)

    deriveTransformationResultExpr(context).map(_.ensureTotal)
  }
}
