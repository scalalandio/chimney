package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.{NotSupportedPatcherDerivation, PatchFieldNotFoundInTargetObj}
import io.scalaland.chimney.internal.compiletime.fp.Implicits.*
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
    with transformer.Derivation
    with transformer.Gateway {

  import Type.Implicits.*

  final def derivePatcherResultExpr[A, Patch](implicit ctx: PatcherContext[A, Patch]): DerivationResult[Expr[A]] = {
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
              resolvePatchMapping[A, Patch](patchFieldName, patchGetter, targetGetters, targetParams)
                .map(_.map(patchFieldName -> _))
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

  private def resolvePatchMapping[A: Type, Patch: Type](
      patchFieldName: String,
      patchGetter: Existential[Product.Getter[Patch, *]],
      targetGetters: Map[String, Existential[Product.Getter[A, *]]],
      targetParams: Map[String, Existential[Product.Parameter]]
  )(implicit
      ctx: PatcherContext[A, Patch]
  ): DerivationResult[Option[ExistentialExpr]] = {

    def patchGetterExpr: ExistentialExpr =
      patchGetter.mapK[Expr] { _ => getter => getter.get(ctx.patch) }

    targetParams.get(patchFieldName) match {

      // TODO: impl
//      case Some(tParam) if config.ignoreNoneInPatch && bothOptions(patchParamTpe, tParam.resultTypeIn(T)) =>

      case Some(targetParam) if patchGetter.Underlying <:< targetParam.Underlying =>
        DerivationResult.pure(Some(patchGetterExpr))

      case Some(targetParam) =>
        Existential.use2(patchGetter, targetParam) {
          implicit from: Type[patchGetter.Underlying] => implicit to: Type[targetParam.Underlying] => (getter, _) =>
            deriveTransformerForPatcherField[patchGetter.Underlying, targetParam.Underlying](
              src = getter.get(ctx.patch)
            )
              .map { transformedExpr =>
                Some(ExistentialExpr(transformedExpr))
              }
              .recoverWith { errors =>
                patchGetter.Underlying match {
                  case Type.Option(innerTpe) =>
                    ExistentialType.use(innerTpe) { implicit innerT: Type[innerTpe.Underlying] =>
                      ExprPromise
                        .promise[Option[innerTpe.Underlying]](
                          ExprPromise.NameGenerationStrategy.FromPrefix(patchFieldName)
                        )
                        .traverse { option =>
                          deriveTransformerForPatcherField[innerTpe.Underlying, targetParam.Underlying](
                            src = Expr.Option.get[innerTpe.Underlying](option)
                          ).map { transformedExpr =>
                            Expr.ifElse(Expr.Option.isDefined(option))(
                              transformedExpr
                            ) {
                              val targetGetter = targetGetters(patchFieldName)
                              Existential.use(targetGetter) { implicit tgTpe: Type[targetGetter.Underlying] => _ =>
                                Expr.asInstanceOf[targetGetter.Underlying, targetParam.Underlying](
                                  targetGetter.value.get(ctx.obj)
                                )
                              }
                            }
                          }
                        }
                        .map { ep => // TODO: naming
                          val eee = ep // TODO: naming
                            .fulfilAsVal(
                              getter
                                .get(ctx.patch)
                                .asInstanceOfExpr[Option[innerTpe.Underlying]]
                            )
                            .prepend[targetParam.Underlying] //  TODO: rename?
                          Some(ExistentialExpr(eee))
                        }
                    }
                  case _ =>
                    DerivationResult.fail(errors)
                }
              }
        }

      case None =>
        if (ctx.config.ignoreRedundantPatcherFields) {
          DerivationResult.pure(None)
        } else {
          DerivationResult.patcherError(PatchFieldNotFoundInTargetObj(patchFieldName, Type.prettyPrint(ctx.A)))
        }
    }
  }

  private def deriveTransformerForPatcherField[From: Type, To: Type](src: Expr[From]): DerivationResult[Expr[To]] = {
    val context = TransformationContext.ForTotal
      .create[From, To](
        src,
        TransformerConfig(),
        ChimneyExpr.RuntimeDataStore.empty
      )
      .updateConfig(_.allowFromToImplicitSearch)

    deriveFinalTransformationResultExpr(context)
  }

}
