package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.internal.compiletime.fp.Syntax.*
import io.scalaland.chimney.internal.compiletime.fp.{Applicative, Traverse}
import io.scalaland.chimney.partial

private[compiletime] trait TransformProductToProductRuleModule { this: Derivation =>

  import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformProductToProductRule extends Rule("ProductToProduct") {

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
                  Existential.use(ctorParam) { implicit ParameterType: Type[ctorParam.Underlying] =>
                    { case Product.Parameter(_, defaultValue) =>
                      fieldOverrides
                        .get(toName)
                        .map {
                          case RuntimeFieldOverride.Const(runtimeDataIdx) =>
                            // We're constructing:
                            // '{ ${ runtimeDataStore }(idx).asInstanceOf[$ctorParam] }
                            Existential(
                              TransformationExpr.fromTotal(
                                ctx.runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[ctorParam.Underlying]
                              )
                            )
                          case RuntimeFieldOverride.ConstPartial(runtimeDataIdx) =>
                            // We're constructing:
                            // '{ ${ runtimeDataStore }(idx).asInstanceOf[partial.Result[$ctorParam]] }
                            Existential(
                              TransformationExpr.fromPartial(
                                ctx
                                  .runtimeDataStore(runtimeDataIdx)
                                  .asInstanceOfExpr[partial.Result[ctorParam.Underlying]]
                              )
                            )
                          case RuntimeFieldOverride.Computed(runtimeDataIdx) =>
                            // We're constructing:
                            // '{ ${ runtimeDataStore }(idx).asInstanceOf[$From => $ctorParam](${ src }) }
                            Existential(
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
                            Existential(
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
                                    Existential(
                                      deriveRecursiveTransformationExpr[getter.Underlying, ctorParam.Underlying](
                                        get(ctx.src)
                                      )
                                    )
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
                                Existential(
                                  deriveRecursiveTransformationExpr[getter.Underlying, ctorParam.Underlying](
                                    get(ctx.src)
                                  )
                                )
                              }
                            }
                        })
                        .orElse(defaultValue.map { (value: Expr[ctorParam.Underlying]) =>
                          // We're constructing:
                          // '{ ${ defaultValue } }
                          DerivationResult.expandedTotal(value)
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
                }
                .flatMap { (resolvedArguments: List[(String, Existential[TransformationExpr])]) =>
                  val totals: Map[String, ExistentialExpr] = resolvedArguments.collect {
                    case (name, expr) if expr.value.isTotal => name -> expr.mapK(_ => _.ensureTotal)
                  }.toMap
                  val partials: List[(String, Existential[PartialExpr])] = resolvedArguments.collect {
                    case (name, expr) if expr.value.isPartial => name -> expr.mapK(_ => _.ensurePartial)
                  }

                  partials match {
                    case Nil =>
                      // We're constructing:
                      // '{ ${ constructor } }
                      DerivationResult.expandedTotal(constructor(totals))
                    case (name, res) :: Nil =>
                      // We're constructing:
                      // '{ ${ res }.map($name => ${ constructor }) }
                      Existential.use(res) {
                        implicit Res: Type[res.Underlying] => (resultExpr: Expr[partial.Result[res.Underlying]]) =>
                          DerivationResult.expandedPartial(
                            resultExpr.map(Expr.Function1.instance { (innerExpr: Expr[res.Underlying]) =>
                              constructor(totals + (name, ExistentialExpr(innerExpr)))
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
                            case TransformationContext.ForTotal(_, failFast) =>
                              DerivationResult.expandedPartial(
                                ChimneyExpr.PartialResult.map2(
                                  result1Expr,
                                  result2Expr,
                                  Expr.Function2.instance {
                                    (inner1Expr: Expr[res1.Underlying], inner2Expr: Expr[res2.Underlying]) =>
                                      constructor(
                                        totals + (name1, ExistentialExpr(inner1Expr)) + (name2, ExistentialExpr(
                                          inner2Expr
                                        ))
                                      )
                                  },
                                  failFast
                                )
                              )
                          }
                      }
                    case _ =>
                      // We're constructing:
                      // '{
                      //   def res1 = ...
                      //   def res2 = ...
                      //   def res3 = ...
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
                      // TODO: exprPromise.fulfilAsDef
                      // TODO: exprPromise.fulfilAsVar
                      // TODO: Expr.ifElse
                      // TODO: expr.isNullExpr
                      type PartialExpr[A] = Expr[partial.Result[A]]
                      DerivationResult.expandedPartial(
                        partials
                          // We start by building this initial block of def resN = ${ derivedResultTo }
                          .map[(String, PrependValsTo[ExistentialExpr])] {
                            case (name: String, expr: Existential[PartialExpr]) =>
                              Existential
                                .use(expr) {
                                  implicit Expr: Type[expr.Underlying] =>
                                    (partialExpr: Expr[partial.Result[expr.Underlying]]) =>
                                      name ->
                                        ExprPromise
                                          .promise[partial.Result[expr.Underlying]](
                                            ExprPromise.NameGenerationStrategy.FromPrefix("res")
                                          )
                                          .map { (inner: Expr[partial.Result[expr.Underlying]]) =>
                                            Existential[PartialExpr, expr.Underlying](inner)
                                          }
                                          .fulfilAsDef(partialExpr)
                                }
                          }
                          .foldLeft(Applicative[PrependValsTo].pure(Vector.empty[(String, Existential[PartialExpr])])) {
                            case (
                                  argsP: PrependValsTo[Vector[(String, Existential[PartialExpr])]],
                                  (name: String, argP: PrependValsTo[Existential[PartialExpr]])
                                ) =>
                              argsP.map2(argP) {
                                (args: Vector[(String, Existential[PartialExpr])], arg: Existential[PartialExpr]) =>
                                  args :+ (name, arg)
                              }
                          }
                          // Now, that we have these defs defined, we can use them
                          .map { partialsAsDefs: Vector[(String, Existential[PartialExpr])] =>
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
                                          innerExpr: Expr[res.Underlying] =>
                                            constructor(constructorArguments + (name, ExistentialExpr(innerExpr)))
                                        })
                                  }
                                // use .flatMap
                                case (name, res) :: tail =>
                                  Existential.use(res) {
                                    implicit ToFlatMap: Type[res.Underlying] =>
                                      (resultToFlatMap: Expr[partial.Result[res.Underlying]]) =>
                                        resultToFlatMap.flatMap(
                                          Expr.Function1.instance[res.Underlying, partial.Result[To]] {
                                            innerExpr: Expr[res.Underlying] =>
                                              nestFlatMaps(
                                                tail,
                                                constructorArguments + (name, ExistentialExpr(innerExpr))
                                              )
                                          }
                                        )
                                  }
                              }

                              nestFlatMaps(partialsAsDefs.toList, totals)
                            }

                            val fullErrorBranch: Expr[partial.Result[To]] = {
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
                              ??? // TODO
                            }

                            ctx match {
                              case TransformationContext.ForTotal(_) =>
                                assertionFailed("Expected partial, got total")
                              case TransformationContext.ForTotal(_, failFast) =>
                                // Finally, we are combining:
                                // if (${ failFast }) {
                                //   ${ failFastBranch }
                                // } else {
                                //   ${ fullErrorBranch }
                                // }
                                Expr.ifElse(failFast)(failFastBranch)(fullErrorBranch)
                            }
                          }
                          .prepend[partial.Result[To]]
                      )
                  }
                }
          }
        case _ => DerivationResult.attemptNextRule
      }

    private def areNamesMatching(fromName: String, toName: String): Boolean = ??? // TODO

    private val isUsingSetter: Existential[Product.Parameter] => Boolean =
      _.value.targetType == Product.Parameter.TargetType.SetterParameter
  }
}
