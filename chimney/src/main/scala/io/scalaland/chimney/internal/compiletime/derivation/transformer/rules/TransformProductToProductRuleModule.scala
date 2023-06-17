package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.internal.compiletime.fp.Syntax.*
import io.scalaland.chimney.internal.compiletime.fp.{Applicative, Traverse}
import io.scalaland.chimney.partial
import io.scalaland.chimney.partial.Result

private[compiletime] trait TransformProductToProductRuleModule { this: Derivation =>

  import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformProductToProductRule extends Rule("ProductToProduct") {

    private type PartialExpr[A] = Expr[partial.Result[A]]

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (ProductType(Product(fromExtractors, _)), ProductType(Product(_, toConstructors))) =>
          import ctx.config.*
          toConstructors match {
            case Product.Constructor(parameters, _) if !flags.beanSetters && parameters.exists(isUsingSetter) =>
              // TODO: provide a nice error message that there are params but setters are disabled
              DerivationResult.notYetImplemented("error about used setters")
            case Product.Constructor(parameters, constructor) =>
              lazy val fromEnabledExtractors = fromExtractors.filter { getter =>
                getter._2.value.sourceType match {
                  case Product.Getter.SourceType.ConstructorVal => true
                  case Product.Getter.SourceType.AccessorMethod => flags.methodAccessors
                  case Product.Getter.SourceType.JavaBeanGetter => flags.beanGetters
                }
              }

              Traverse[List]
                .traverse[
                  DerivationResult,
                  (String, Existential[Product.Parameter]),
                  (String, Existential[TransformationExpr])
                ](
                  parameters.toList
                ) { case (toName: String, ctorParam: Existential[Product.Parameter]) =>
                  Existential
                    .use(ctorParam) { implicit ParameterType: Type[ctorParam.Underlying] =>
                      { case Product.Parameter(_, defaultValue) =>
                        fieldOverrides
                          .get(toName) // TODO: use might use _.getName, which would be mapped to _.setName in beans!
                          .map {
                            case RuntimeFieldOverride.Const(runtimeDataIdx) =>
                              // We're constructing:
                              // '{ ${ runtimeDataStore }(idx).asInstanceOf[$ctorParam] }
                              DerivationResult.existential[TransformationExpr, ctorParam.Underlying](
                                TransformationExpr.fromTotal(
                                  ctx.runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[ctorParam.Underlying]
                                )
                              )
                            case RuntimeFieldOverride.ConstPartial(runtimeDataIdx) =>
                              // We're constructing:
                              // '{ ${ runtimeDataStore }(idx).asInstanceOf[partial.Result[$ctorParam]] }
                              DerivationResult.existential[TransformationExpr, ctorParam.Underlying](
                                TransformationExpr.fromPartial(
                                  ctx
                                    .runtimeDataStore(runtimeDataIdx)
                                    .asInstanceOfExpr[partial.Result[ctorParam.Underlying]]
                                )
                              )
                            case RuntimeFieldOverride.Computed(runtimeDataIdx) =>
                              // We're constructing:
                              // '{ ${ runtimeDataStore }(idx).asInstanceOf[$From => $ctorParam](${ src }) }
                              DerivationResult.existential[TransformationExpr, ctorParam.Underlying](
                                TransformationExpr.fromTotal(
                                  ctx
                                    .runtimeDataStore(runtimeDataIdx)
                                    .asInstanceOfExpr[From => ctorParam.Underlying]
                                    .apply(ctx.src)
                                )
                              )
                            case RuntimeFieldOverride.ComputedPartial(runtimeDataIdx) =>
                              // We're constructing:
                              // '{ ${ runtimeDataStore }(idx).asInstanceOf[$From => partial.Result[$ctorParam]](${ src }) }
                              DerivationResult.existential[TransformationExpr, ctorParam.Underlying](
                                TransformationExpr.fromPartial(
                                  ctx
                                    .runtimeDataStore(runtimeDataIdx)
                                    .asInstanceOfExpr[From => partial.Result[ctorParam.Underlying]]
                                    .apply(ctx.src)
                                )
                              )
                            case RuntimeFieldOverride.RenamedFrom(sourceName) =>
                              fromExtractors
                                .collectFirst { case (`sourceName`, getter) =>
                                  Existential.use(getter) { implicit Getter: Type[getter.Underlying] =>
                                    { case Product.Getter(_, get) =>
                                      // We're constructing:
                                      // '{ ${ derivedToElement } } // using ${ src.$name }
                                      deriveRecursiveTransformationExpr[getter.Underlying, ctorParam.Underlying](
                                        get(ctx.src)
                                      ).map(Existential[TransformationExpr, ctorParam.Underlying](_))
                                    }
                                  }
                                }
                                .getOrElse {
                                  DerivationResult.assertionError(
                                    s"Assumed that field $sourceName is a part of ${Type[From]}, but wasn't found"
                                  )
                                }
                          }
                          .orElse(fromEnabledExtractors.collectFirst {
                            case (name, getter) if areNamesMatching(name, toName) =>
                              Existential.use(getter) { implicit Getter: Type[getter.Underlying] =>
                                { case Product.Getter(_, get) =>
                                  // We're constructing:
                                  // '{ ${ derivedToElement } } // using ${ src.$name }
                                  deriveRecursiveTransformationExpr[getter.Underlying, ctorParam.Underlying](
                                    get(ctx.src)
                                  ).map(Existential[TransformationExpr, ctorParam.Underlying](_))
                                }
                              }
                          })
                          .orElse(defaultValue.map { (value: Expr[ctorParam.Underlying]) =>
                            // We're constructing:
                            // '{ ${ defaultValue } }
                            DerivationResult.existential[TransformationExpr, ctorParam.Underlying](
                              TransformationExpr.fromTotal(value)
                            )
                          })
                          .getOrElse {
                            // TODO:
                            //   - if there is Java Bean getter that couldn't be used because flag is off, inform about it
                            //   - if there is accessor that couldn't be used because flag is off, inform about it
                            //   - if default value exists that couldn't be used because flag is off, inform about it
                            DerivationResult.notYetImplemented("Proper error message")
                          }
                      }
                    }
                    .map(toName -> _)
                }
                .flatMap { (resolvedArguments: List[(String, Existential[TransformationExpr])]) =>
                  val totalConstructorArguments: Map[String, ExistentialExpr] = resolvedArguments.collect {
                    case (name, expr) if expr.value.isTotal => name -> expr.mapK(_ => _.ensureTotal)
                  }.toMap

                  resolvedArguments.collect[(String, Existential[PartialExpr])] {
                    case (name, expr) if expr.value.isPartial => name -> expr.mapK(_ => _.ensurePartial)
                  } match {
                    case Nil =>
                      // We're constructing:
                      // '{ ${ constructor } }
                      DerivationResult.expandedTotal(constructor(totalConstructorArguments))
                    case (name, res) :: Nil =>
                      // We're constructing:
                      // '{ ${ res }.map($name => ${ constructor }) }
                      Existential.use(res) {
                        implicit Res: Type[res.Underlying] => (resultExpr: Expr[partial.Result[res.Underlying]]) =>
                          DerivationResult.expandedPartial(
                            resultExpr.map(Expr.Function1.instance { (innerExpr: Expr[res.Underlying]) =>
                              constructor(totalConstructorArguments + (name -> ExistentialExpr(innerExpr)))
                            })
                          )
                      }
                    case (name1, res1) :: (name2, res2) :: Nil =>
                      // We're constructing:
                      // '{ partial.Result.map2(${ res1 }, ${ res2 }, { ($name1, $name2) =>
                      //   ${ constructor }
                      // }, ${ failFast }) }
                      Existential.use2(res1, res2) {
                        implicit Res1: Type[res1.Underlying] => implicit Res2: Type[res2.Underlying] => (
                            result1Expr: Expr[partial.Result[res1.Underlying]],
                            result2Expr: Expr[partial.Result[res2.Underlying]]
                        ) =>
                          ctx match {
                            case TransformationContext.ForTotal(_) =>
                              DerivationResult.assertionError("Expected partial while got total")
                            case TransformationContext.ForPartial(_, failFast) =>
                              DerivationResult.expandedPartial(
                                ChimneyExpr.PartialResult.map2(
                                  result1Expr,
                                  result2Expr,
                                  Expr.Function2.instance {
                                    (inner1Expr: Expr[res1.Underlying], inner2Expr: Expr[res2.Underlying]) =>
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
                      //           ${ constructor }
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
                      partialConstructorArguments
                        .traverse[DerivationResult, PrependDefinitionsTo[(String, Existential[PartialExpr])]] {
                          case (name: String, expr: Existential[PartialExpr]) =>
                            // We start by building this initial block of def resN = ${ derivedResultTo }
                            Existential.use(expr) {
                              implicit Expr: Type[expr.Underlying] =>
                                (partialExpr: Expr[partial.Result[expr.Underlying]]) =>
                                  ExprPromise
                                    .promise[partial.Result[expr.Underlying]](
                                      ExprPromise.NameGenerationStrategy.FromPrefix("res")
                                    )
                                    .map { (inner: Expr[partial.Result[expr.Underlying]]) =>
                                      name -> Existential[PartialExpr, expr.Underlying](inner)
                                    }
                                    .fulfilAsLazy(partialExpr)
                            }
                        }
                        .flatMap {
                          (partialsAsLazyPrepended: List[PrependDefinitionsTo[(String, Existential[PartialExpr])]]) =>
                            partialsAsLazyPrepended.sequence
                              .traverse { (partialsAsLazy: List[(String, Existential[PartialExpr])]) =>
                                val failFastBranch: Expr[partial.Result[To]] = {
                                  // Here, we're building:
                                  // res1.flatMap { $name1 =>
                                  //   res2.flatMap { $name2 =>
                                  //     res3.flatMap { $name3 =>
                                  //       ...
                                  //       ${ constructor }
                                  //     }
                                  //   }
                                  // }
                                  def nestFlatMaps(
                                      unusedPartials: List[(String, Existential[PartialExpr])],
                                      constructorArguments: Product.Arguments
                                  ): Expr[partial.Result[To]] = unusedPartials match {
                                    // Should never happen
                                    case Nil => ???
                                    // last result to compose in - use .map instead of .flatMap
                                    case (name, res) :: Nil =>
                                      Existential.use(res) {
                                        implicit ToMap: Type[res.Underlying] =>
                                          (resultToMap: Expr[partial.Result[res.Underlying]]) =>
                                            resultToMap.map(Expr.Function1.instance[res.Underlying, To] {
                                              (innerExpr: Expr[res.Underlying]) =>
                                                constructor(constructorArguments + (name -> ExistentialExpr(innerExpr)))
                                            })
                                      }
                                    // use .flatMap
                                    case (name, res) :: tail =>
                                      Existential.use(res) {
                                        implicit ToFlatMap: Type[res.Underlying] =>
                                          (resultToFlatMap: Expr[partial.Result[res.Underlying]]) =>
                                            resultToFlatMap.flatMap(
                                              Expr.Function1.instance[res.Underlying, partial.Result[To]] {
                                                (innerExpr: Expr[res.Underlying]) =>
                                                  nestFlatMaps(
                                                    tail,
                                                    constructorArguments + (name -> ExistentialExpr(innerExpr))
                                                  )
                                              }
                                            )
                                      }
                                  }

                                  nestFlatMaps(partialsAsLazy.toList, totalConstructorArguments)
                                }

                                val fullErrorBranchR: DerivationResult[Expr[partial.Result[To]]] = {
                                  // Here, we're building:
                                  // var allerrors: Errors = null
                                  // allerrors = partial.Result.Errors.__mergeResultNullable(allerrors, ${ res1 })
                                  // allerrors = partial.Result.Errors.__mergeResultNullable(allerrors, ${ res2 })
                                  // allerrors = partial.Result.Errors.__mergeResultNullable(allerrors, ${ res3 })
                                  // ...
                                  // if (allerrors == null) {
                                  //   ${ constructor } // using res1.asInstanceOf[partial.Result.Value[Tpe]].value, ...
                                  // } else {
                                  //   allerrors
                                  // }
                                  ExprPromise
                                    .promise[partial.Result.Errors](
                                      ExprPromise.NameGenerationStrategy.FromPrefix("allerrors")
                                    )
                                    .fulfilAsVar(Expr.Null.asInstanceOfExpr[partial.Result.Errors])
                                    .map[Expr[partial.Result[To]]] { allerrorsVar =>
                                      allerrorsVar
                                        .map {
                                          case (
                                                allerrors: Expr[partial.Result.Errors],
                                                setAllErrors: (Expr[partial.Result.Errors] => Expr[Unit])
                                              ) =>
                                            Expr.block(
                                              partialsAsLazy.map { case (_, result) =>
                                                Existential.use(result) {
                                                  implicit Result: Type[result.Underlying] =>
                                                    (expr: Expr[partial.Result[result.Underlying]]) =>
                                                      setAllErrors(
                                                        ChimneyExpr.PartialResult.Errors
                                                          .mergeResultNullable(allerrors, expr)
                                                      )
                                                }
                                              },
                                              Expr.ifElse[partial.Result[To]](allerrors eqExpr Expr.Null) {
                                                ChimneyExpr.PartialResult
                                                  .Value[To](
                                                    constructor(
                                                      totalConstructorArguments ++ partialsAsLazy.map {
                                                        case (name, result) =>
                                                          name -> result.mapK[Expr] {
                                                            implicit PartialExpr: Type[result.Underlying] =>
                                                              (expr: Expr[
                                                                partial.Result[result.Underlying]
                                                              ]) =>
                                                                expr
                                                                  .asInstanceOfExpr[
                                                                    partial.Result.Value[result.Underlying]
                                                                  ]
                                                                  .value
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
                                        .prepend[partial.Result[To]]
                                    }
                                }

                                ctx match {
                                  case TransformationContext.ForTotal(_) =>
                                    assertionFailed("Expected partial, got total")
                                  case TransformationContext.ForPartial(_, failFast) =>
                                    fullErrorBranchR.map { fullErrorBranch =>
                                      // Finally, we are combining:
                                      // if (${ failFast }) {
                                      //   ${ failFastBranch }
                                      // } else {
                                      //   ${ fullErrorBranch }
                                      // }
                                      Expr.ifElse[partial.Result[To]](failFast)(failFastBranch)(fullErrorBranch)
                                    }
                                }
                              }
                              .map(_.prepend[partial.Result[To]])
                        }
                        .flatMap(DerivationResult.expandedPartial)
                  }
                }
          }
        case _ => DerivationResult.attemptNextRule
      }

    private def areNamesMatching(fromName: String, toName: String): Boolean = ??? // TODO

    private val isUsingSetter: ((String, Existential[Product.Parameter])) => Boolean =
      _._2.value.targetType == Product.Parameter.TargetType.SetterParameter
  }
}
