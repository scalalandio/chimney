package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.{DerivationErrors, DerivationResult}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.internal.compiletime.fp.Implicits.*
import io.scalaland.chimney.internal.compiletime.fp.Traverse
import io.scalaland.chimney.partial

private[compiletime] trait TransformProductToProductRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*, ProductType.areNamesMatching

  protected object TransformProductToProductRule extends Rule("ProductToProduct") {

    private type PartialExpr[A] = Expr[partial.Result[A]]

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (Product.Extraction(fromExtractors), Product.Constructor(parameters, constructor)) =>
          mapOverridesAndExtractorsToConstructorArguments[From, To](fromExtractors, parameters, constructor)
        case _ =>
          DerivationResult.attemptNextRuleBecause(
            s"Type ${Type.prettyPrint[To]} does not have a public primary constructor"
          )
      }

    private def mapOverridesAndExtractorsToConstructorArguments[From, To](
        fromExtractors: Product.Getters[From],
        parameters: Product.Parameters,
        constructor: Product.Arguments => Expr[To]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] = {
      import ctx.config.*

      lazy val fromEnabledExtractors = fromExtractors
        .filter { getter =>
          getter._2.value.sourceType match {
            case Product.Getter.SourceType.ConstructorVal => true
            case Product.Getter.SourceType.AccessorMethod => flags.methodAccessors
            case Product.Getter.SourceType.JavaBeanGetter => flags.beanGetters
          }
        }
        .filter { getter =>
          getter._2.value.isLocal || flags.inheritedAccessors
        }

      val usePositionBasedMatching = Type[From].isTuple || Type[To].isTuple
      lazy val ctorParamToGetter = parameters
        .zip(fromEnabledExtractors)
        .map { case ((toName, ctorParam), (fromName, getter)) =>
          val t3 = (fromName, toName, getter)
          ctorParam -> t3
        }
        .toMap

      val verifyNoOverrideUnused = Traverse[List]
        .parTraverse(
          filterOverridesForField(fromName =>
            !parameters.keys.exists(toName => areNamesMatching(fromName, toName))
          ).view.keys.toList
        ) { fromName =>
          val tpeStr = Type.prettyPrint[To]
          val params = parameters.keys.map(n => s"`$n`").mkString(", ")
          DerivationResult.assertionError(
            s"""|Assumed that parameter/setter $fromName is a part of $tpeStr, but wasn't found
                |available methods: $params""".stripMargin
          )
        }

      DerivationResult.log {
        val gettersStr = fromExtractors
          .map { case (k, v) =>
            s"`$k`: ${Type.prettyPrint(v.Underlying)} (${v.value.sourceType}, ${if (v.value.isLocal) "declared"
              else "inherited"})"
          }
          .mkString(", ")
        val constructorStr = parameters
          .map { case (k, v) =>
            s"`$k`: ${Type.prettyPrint(v.Underlying)} (${v.value.targetType}, default = ${v.value.defaultValue
                .map(a => Expr.prettyPrint(a))})"
          }
          .mkString(", ")
        s"Resolved ${Type.prettyPrint[From]} getters: ($gettersStr) and ${Type.prettyPrint[To]} constructor ($constructorStr)"
      } >> verifyNoOverrideUnused >>
        Traverse[List]
          .parTraverse[
            DerivationResult,
            (String, Existential[Product.Parameter]),
            (String, Existential[TransformationExpr])
          ](
            parameters.toList
          ) { case (toName: String, ctorParam: Existential[Product.Parameter]) =>
            import ctorParam.Underlying as CtorParam, ctorParam.value.defaultValue
            // user might have used _.getName in modifier, to define target we know as _.setName
            // so simple .get(toName) might not be enough
            filterOverridesForField(fromName => areNamesMatching(fromName, toName)).headOption
              .map { case (fromName, value) =>
                useOverride[From, To, CtorParam](fromExtractors, fromName, toName, value)
              }
              .orElse {
                val resolvedExtractor =
                  if (usePositionBasedMatching) ctorParamToGetter.get(ctorParam)
                  else
                    fromEnabledExtractors.collectFirst {
                      case (fromName, getter) if areNamesMatching(fromName, toName) => (fromName, toName, getter)
                    }
                resolvedExtractor
                  .map { case (fromName, toName, getter) =>
                    useExtractor[From, To, CtorParam](ctorParam.value.targetType, fromName, toName, getter)
                  }
                  .orElse[DerivationResult[Existential[TransformationExpr]]](
                    if (usePositionBasedMatching)
                      Option(
                        DerivationResult.incompatibleSourceTuple(
                          sourceArity = fromEnabledExtractors.size,
                          targetArity = parameters.size
                        )
                      )
                    else None
                  )
              }
              .orElse(useFallbackValues[From, To, CtorParam](defaultValue))
              .getOrElse[DerivationResult[Existential[TransformationExpr]]] {
                ctorParam.value.targetType match {
                  case Product.Parameter.TargetType.ConstructorParameter =>
                    // TODO: update this for isLocal
                    DerivationResult
                      .missingAccessor[From, To, CtorParam, Existential[TransformationExpr]](
                        toName,
                        fromExtractors.exists { case (fromName, _) => areNamesMatching(fromName, toName) }
                      )
                  case Product.Parameter.TargetType.SetterParameter =>
                    // TODO: update this for isLocal
                    DerivationResult
                      .missingJavaBeanSetterParam[From, To, CtorParam, Existential[TransformationExpr]](
                        ProductType.dropSet(toName),
                        fromExtractors.exists { case (fromName, _) => areNamesMatching(fromName, toName) }
                      )
                }
              }
              .logSuccess(expr => s"Resolved `$toName` field value to ${expr.value.prettyPrint}")
              .map(toName -> _)
          }
          .logSuccess { args =>
            val totals = args.count(_._2.value.isTotal)
            val partials = args.count(_._2.value.isPartial)
            s"Resolved ${args.size} arguments, $totals as total and $partials as partial Expr"
          }
          .map[TransformationExpr[To]] { (resolvedArguments: List[(String, Existential[TransformationExpr])]) =>
            wireArgumentsToConstructor[From, To](resolvedArguments, constructor)
          }
          .flatMap(DerivationResult.expanded)
    }

    private def useOverride[From, To, CtorParam: Type](
        fromExtractors: Product.Getters[From],
        fromName: String,
        toName: String,
        runtimeFieldOverride: RuntimeFieldOverride
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Existential[TransformationExpr]] = runtimeFieldOverride match {
      case RuntimeFieldOverride.Const(runtimeDataIdx) =>
        // We're constructing:
        // '{ ${ runtimeDataStore }(idx).asInstanceOf[$ctorParam] }
        DerivationResult.existential[TransformationExpr, CtorParam](
          TransformationExpr.fromTotal(
            ctx.runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[CtorParam]
          )
        )
      case RuntimeFieldOverride.ConstPartial(runtimeDataIdx) =>
        // We're constructing:
        // '{
        //   ${ runtimeDataStore }(idx)
        //     .asInstanceOf[partial.Result[$ctorParam]]
        //     .prependErrorPath(PathElement.Accessor("fromName"))
        //  }
        DerivationResult.existential[TransformationExpr, CtorParam](
          TransformationExpr.fromPartial(
            ctx
              .runtimeDataStore(runtimeDataIdx)
              .asInstanceOfExpr[partial.Result[CtorParam]]
              .prependErrorPath(
                ChimneyExpr.PathElement.Accessor(Expr.String(fromName)).upcastExpr[partial.PathElement]
              )
          )
        )
      case RuntimeFieldOverride.Computed(runtimeDataIdx) =>
        ctx match {
          case TransformationContext.ForTotal(src) =>
            // We're constructing:
            // '{ ${ runtimeDataStore }(idx).asInstanceOf[$From => $ctorParam](${ src }) }
            DerivationResult.existential[TransformationExpr, CtorParam](
              TransformationExpr.fromTotal(
                ctx.runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[From => CtorParam].apply(src)
              )
            )
          case TransformationContext.ForPartial(src, _) =>
            // We're constructing:
            // '{
            //   partial.Result.fromFunction(
            //     ${ runtimeDataStore }(idx).asInstanceOf[$From => $ctorParam]
            //   )
            //   .apply(${ src })
            //   .prependErrorPath(PathElement.Accessor("fromName"))
            // }
            DerivationResult.existential[TransformationExpr, CtorParam](
              TransformationExpr.fromPartial(
                ChimneyExpr.PartialResult
                  .fromFunction(
                    ctx.runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[From => CtorParam]
                  )
                  .apply(src)
                  .prependErrorPath(
                    ChimneyExpr.PathElement
                      .Accessor(Expr.String(fromName))
                      .upcastExpr[partial.PathElement]
                  )
              )
            )
        }
      case RuntimeFieldOverride.ComputedPartial(runtimeDataIdx) =>
        // We're constructing:
        // '{
        //   ${ runtimeDataStore }(idx)
        //     .asInstanceOf[$From => partial.Result[$ctorParam]](${ src })
        //     .prependErrorPath(PathElement.Accessor("fromName"))
        // }
        DerivationResult.existential[TransformationExpr, CtorParam](
          TransformationExpr.fromPartial(
            ctx
              .runtimeDataStore(runtimeDataIdx)
              .asInstanceOfExpr[From => partial.Result[CtorParam]]
              .apply(ctx.src)
              .prependErrorPath(
                ChimneyExpr.PathElement.Accessor(Expr.String(fromName)).upcastExpr[partial.PathElement]
              )
          )
        )
      case RuntimeFieldOverride.RenamedFrom(sourceName) =>
        fromExtractors
          .collectFirst { case (`sourceName`, getter) =>
            import getter.Underlying as Getter, getter.value.get
            DerivationResult.namedScope(
              s"Recursive derivation for field `$sourceName`: ${Type
                  .prettyPrint[Getter]} renamed into `$toName`: ${Type
                  .prettyPrint[CtorParam]}"
            ) {
              // We're constructing:
              // '{ ${ derivedToElement } } // using ${ src.$name }
              deriveRecursiveTransformationExpr[Getter, CtorParam](get(ctx.src)).transformWith { expr =>
                // If we derived partial.Result[$ctorParam] we are appending
                //  ${ derivedToElement }.prependErrorPath(PathElement.Accessor("fromName"))
                DerivationResult.existential[TransformationExpr, CtorParam](appendPath(expr, sourceName))
              } { errors =>
                appendMissingTransformer[From, To, Getter, CtorParam](errors, toName)
              }
            }
          }
          .getOrElse {
            val tpeStr = Type.prettyPrint[From]
            val methods = fromExtractors.keys.map(n => s"`$n`").mkString(", ")
            DerivationResult.assertionError(
              s"""|Assumed that field $sourceName is a part of $tpeStr, but wasn't found
                  |available methods: $methods""".stripMargin
            )
          }
    }

    private def useExtractor[From, To, CtorParam: Type](
        ctorTargetType: Product.Parameter.TargetType,
        fromName: String,
        toName: String,
        getter: Existential[Product.Getter[From, *]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Existential[TransformationExpr]] =
      if (ctorTargetType == Product.Parameter.TargetType.SetterParameter && !ctx.config.flags.beanSetters)
        DerivationResult
          .notSupportedTransformerDerivation(ctx)
          .log(s"Matched $fromName to $toName but $toName is setter and they are disabled")
      else {
        import getter.Underlying as Getter, getter.value.get
        DerivationResult.namedScope(
          s"Recursive derivation for field `$fromName`: ${Type
              .prettyPrint[Getter]} into matched `$toName`: ${Type.prettyPrint[CtorParam]}"
        ) {
          // We're constructing:
          // '{ ${ derivedToElement } } // using ${ src.$name }
          deriveRecursiveTransformationExpr[Getter, CtorParam](get(ctx.src)).transformWith { expr =>
            // If we derived partial.Result[$ctorParam] we are appending
            //  ${ derivedToElement }.prependErrorPath(PathElement.Accessor("fromName"))
            DerivationResult.existential[TransformationExpr, CtorParam](appendPath(expr, fromName))
          } { errors =>
            appendMissingTransformer[From, To, Getter, CtorParam](errors, toName)
          }
        }
      }

    private def useFallbackValues[From, To, CtorParam: Type](
        defaultValue: Option[Expr[CtorParam]]
    )(implicit ctx: TransformationContext[From, To]): Option[DerivationResult[Existential[TransformationExpr]]] =
      defaultValue
        .filter(_ => ctx.config.flags.processDefaultValues)
        .map { (value: Expr[CtorParam]) =>
          // We're constructing:
          // '{ ${ defaultValue } }
          DerivationResult.existential[TransformationExpr, CtorParam](
            TransformationExpr.fromTotal(value)
          )
        }
        .orElse {
          Option(Expr.Option.None)
            .filter(_ => Type[CtorParam].isOption && ctx.config.flags.optionDefaultsToNone)
            .map(value =>
              // We're constructing:
              // '{ None }
              DerivationResult.existential[TransformationExpr, CtorParam](
                TransformationExpr.fromTotal(value.upcastExpr[CtorParam])
              )
            )

        }
        .orElse {
          Option(Expr.Unit).filter(_ => Type[CtorParam] =:= Type[Unit]).map { value =>
            // We're constructing:
            // '{ () }
            DerivationResult.existential[TransformationExpr, CtorParam](
              TransformationExpr.fromTotal(value.upcastExpr[CtorParam])
            )
          }
        }

    private def wireArgumentsToConstructor[From, To](
        resolvedArguments: List[(String, Existential[TransformationExpr])],
        constructor: Product.Arguments => Expr[To]
    )(implicit ctx: TransformationContext[From, To]): TransformationExpr[To] = {
      val totalConstructorArguments: Map[String, ExistentialExpr] = resolvedArguments.collect {
        case (name, exprE) if exprE.value.isTotal => name -> exprE.mapK[Expr](_ => _.ensureTotal)
      }.toMap

      resolvedArguments.collect {
        case (name, exprE) if exprE.value.isPartial =>
          name -> exprE.mapK[PartialExpr] { implicit ExprE: Type[exprE.Underlying] => _.ensurePartial }
      } match {
        case Nil =>
          // We're constructing:
          // '{ ${ constructor } }
          TransformationExpr.fromTotal(constructor(totalConstructorArguments))
        case (name, res) :: Nil =>
          // We're constructing:
          // '{ ${ res }.map($name => ${ constructor }) }
          import res.{Underlying as Res, value as resultExpr}
          TransformationExpr.fromPartial(
            resultExpr.map(Expr.Function1.instance { (innerExpr: Expr[Res]) =>
              constructor(totalConstructorArguments + (name -> ExistentialExpr(innerExpr)))
            })
          )
        case (name1, res1) :: (name2, res2) :: Nil =>
          // We're constructing:
          // '{ partial.Result.map2(${ res1 }, ${ res2 }, { ($name1, $name2) =>
          //   ${ constructor }
          // }, ${ failFast }) }
          import res1.{Underlying as Res1, value as result1Expr}, res2.{Underlying as Res2, value as result2Expr}
          ctx match {
            case TransformationContext.ForTotal(_) =>
              assertionFailed("Expected partial while got total")
            case TransformationContext.ForPartial(_, failFast) =>
              TransformationExpr.fromPartial(
                ChimneyExpr.PartialResult.map2(
                  result1Expr,
                  result2Expr,
                  Expr.Function2.instance { (inner1Expr: Expr[Res1], inner2Expr: Expr[Res2]) =>
                    constructor(
                      totalConstructorArguments +
                        (name1 -> ExistentialExpr(inner1Expr)) +
                        (name2 -> ExistentialExpr(inner2Expr))
                    )
                  },
                  failFast
                )
              )
          }
        case partialConstructorArguments =>
          // We're constructing:
          // '{
          //   lazy val res1 = ...
          //   lazy val res2 = ...
          //   lazy val res3 = ...
          //   ...
          //
          //   if (${ failFast }) {
          //     res1.flatMap { $name1 =>
          //       res2.flatMap { $name2 =>
          //         res3.flatMap { $name3 =>
          //           ...
          //            resN.map { $nameN => ${ constructor } }
          //         }
          //       }
          //     }
          //   } else {
          //     var allerrors: Errors = null
          //     allerrors = partial.Result.Errors.__mergeResultNullable(allerrors, ${ res1 })
          //     allerrors = partial.Result.Errors.__mergeResultNullable(allerrors, ${ res2 })
          //     allerrors = partial.Result.Errors.__mergeResultNullable(allerrors, ${ res3 })
          //     ...
          //     if (allerrors == null) {
          //       ${ constructor } // using res1.asInstanceOf[partial.Result.Value[Tpe]].value, ...
          //     } else {
          //       allerrors
          //     }
          //   }
          // }
          TransformationExpr.fromPartial(
            partialConstructorArguments
              .traverse[PrependDefinitionsTo, (String, Existential[PartialExpr])] {
                case (name: String, expr: Existential[PartialExpr]) =>
                  // We start by building this initial block of '{ def resN = ${ derivedResultTo } }
                  import expr.{Underlying as Res, value as partialExpr}
                  PrependDefinitionsTo
                    .prependLazyVal(
                      partialExpr,
                      ExprPromise.NameGenerationStrategy.FromPrefix("res")
                    )
                    .map { (inner: Expr[partial.Result[Res]]) =>
                      name -> Existential[PartialExpr, Res](inner)
                    }
              }
              .use { (partialsAsLazy: List[(String, Existential[PartialExpr])]) =>
                val failFastBranch: Expr[partial.Result[To]] = {
                  // Here, we're building:
                  // '{
                  //   res1.flatMap { $name1 =>
                  //     res2.flatMap { $name2 =>
                  //       res3.flatMap { $name3 =>
                  //         ...
                  //          resN.map { $nameN => ${ constructor } }
                  //       }
                  //     }
                  // } }
                  def nestFlatMaps(
                      unusedPartials: List[(String, Existential[PartialExpr])],
                      constructorArguments: Product.Arguments
                  ): Expr[partial.Result[To]] = unusedPartials match {
                    // Should never happen
                    case Nil => ???
                    // last result to compose in - use .map instead of .flatMap
                    case (name, res) :: Nil =>
                      import res.{Underlying as Res, value as resultToMap}
                      resultToMap.map(Expr.Function1.instance[Res, To] { (innerExpr: Expr[Res]) =>
                        constructor(constructorArguments + (name -> ExistentialExpr(innerExpr)))
                      })
                    // use .flatMap
                    case (name, res) :: tail =>
                      import res.{Underlying as Res, value as resultToFlatMap}
                      resultToFlatMap.flatMap(
                        Expr.Function1.instance[Res, partial.Result[To]] { (innerExpr: Expr[Res]) =>
                          nestFlatMaps(tail, constructorArguments + (name -> ExistentialExpr(innerExpr)))
                        }
                      )
                  }

                  nestFlatMaps(partialsAsLazy.toList, totalConstructorArguments)
                }

                val fullErrorBranch: Expr[partial.Result[To]] =
                  // Here, we're building:
                  // '{
                  //   var allerrors: Errors = null
                  //   allerrors = partial.Result.Errors.__mergeResultNullable(allerrors, ${ res1 })
                  //   allerrors = partial.Result.Errors.__mergeResultNullable(allerrors, ${ res2 })
                  //   allerrors = partial.Result.Errors.__mergeResultNullable(allerrors, ${ res3 })
                  //   ...
                  //   if (allerrors == null) {
                  //     partial.Result.Value(${ constructor }) // using res1.asInstanceOf[partial.Result.Value[Tpe]].value, ...
                  //   } else {
                  //     allerrors
                  //   }
                  // }
                  PrependDefinitionsTo
                    .prependVar[partial.Result.Errors](
                      Expr.Null.asInstanceOfExpr[partial.Result.Errors],
                      ExprPromise.NameGenerationStrategy.FromPrefix("allerrors")
                    )
                    .use { case (allerrors, setAllErrors) =>
                      Expr.block(
                        partialsAsLazy.map { case (_, result) =>
                          import result.{Underlying, value as expr}
                          // Here, we're building:
                          // '{ allerrors = partial.Result.Errors.__mergeResultNullable(allerrors, ${ resN }) }
                          setAllErrors(ChimneyExpr.PartialResult.Errors.mergeResultNullable(allerrors, expr))
                        },
                        // Here, we're building:
                        // '{ if (allerrors == null) $ifBlock else $elseBock }
                        Expr.ifElse[partial.Result[To]](allerrors eqExpr Expr.Null) {
                          // Here, we're building:
                          // '{ partial.Result.Value(${ constructor }) } // using res1.asInstanceOf[partial.Result.Value[Tpe]].value, ...
                          ChimneyExpr.PartialResult
                            .Value[To](
                              constructor(
                                totalConstructorArguments ++ partialsAsLazy.map { case (name, result) =>
                                  import result.Underlying as Res
                                  name -> result.mapK[Expr] { _ => (expr: Expr[partial.Result[Res]]) =>
                                    expr.asInstanceOfExpr[partial.Result.Value[Res]].value
                                  }
                                }
                              )
                            )
                            .upcastExpr[partial.Result[To]]
                        } {
                          allerrors.upcastExpr[partial.Result[To]]
                        }
                      )
                    }

                ctx match {
                  case TransformationContext.ForTotal(_) =>
                    assertionFailed("Expected partial, got total")
                  case TransformationContext.ForPartial(_, failFast) =>
                    // Finally, we are combining:
                    // if (${ failFast }) {
                    //   ${ failFastBranch }
                    // } else {
                    //   ${ fullErrorBranch }
                    // }
                    Expr.ifElse[partial.Result[To]](failFast)(failFastBranch)(fullErrorBranch)
                }
              }
          )
      }
    }

    // If we derived partial.Result[$ctorParam] we are appending
    //  ${ derivedToElement }.prependErrorPath(PathElement.Accessor("fromName"))
    private def appendPath[A: Type](expr: TransformationExpr[A], path: String): TransformationExpr[A] =
      expr.fold(TransformationExpr.fromTotal)(partialE =>
        TransformationExpr.fromPartial(
          partialE.prependErrorPath(
            ChimneyExpr.PathElement
              .Accessor(Expr.String(path))
              .upcastExpr[partial.PathElement]
          )
        )
      )

    private def appendMissingTransformer[From, To, SourceField: Type, TargetField: Type](
        errors: DerivationErrors,
        toName: String
    )(implicit ctx: TransformationContext[From, To]) = {
      val newError = DerivationResult.missingTransformer[
        From,
        To,
        SourceField,
        TargetField,
        Existential[TransformationExpr]
      ](toName)
      val oldErrors = DerivationResult.fail(errors)
      newError.parTuple(oldErrors).map[Existential[TransformationExpr]](_ => ???)
    }
  }
}
