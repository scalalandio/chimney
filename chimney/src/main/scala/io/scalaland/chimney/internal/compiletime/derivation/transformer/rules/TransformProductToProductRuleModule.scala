package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.internal.compiletime.fp.Syntax.*
import io.scalaland.chimney.internal.compiletime.fp.Traverse
import io.scalaland.chimney.partial

import scala.language.postfixOps

private[compiletime] trait TransformProductToProductRuleModule { this: Derivation =>

  import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformProductToProductRule extends Rule("ProductToProduct") {

    private type PartialExpr[A] = Expr[partial.Result[A]]

    // TODO:
    // 1. update ProductValue.parseConstructor to allow any class with a public constructor:
    //    1. params from the constructor first
    //    2. followed by params from setters - deduplicated to avoid setting something twice
    // 2. add tuple support
    // 3. check that isValue is checked for Boolean

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (Product.Extraction(fromExtractors), Product.Constructor(parameters, constructor)) =>
          import ctx.config.*
          lazy val fromEnabledExtractors = fromExtractors.filter { getter =>
            getter._2.value.sourceType match {
              case Product.Getter.SourceType.ConstructorVal => true
              case Product.Getter.SourceType.AccessorMethod => flags.methodAccessors
              case Product.Getter.SourceType.JavaBeanGetter => flags.beanGetters
            }
          }

          DerivationResult.log {
            val gettersStr = fromExtractors
              .map { case (k, v) => s"`$k`: ${Type.prettyPrint(v.Underlying)} (${v.value.sourceType})" }
              .mkString(", ")
            val constructorStr = parameters
              .map { case (k, v) =>
                s"`$k`: ${Type.prettyPrint(v.Underlying)} (${v.value.targetType}, default = ${v.value.defaultValue
                    .map(a => Expr.prettyPrint(a))})"
              }
              .mkString(", ")
            s"Resolved ${Type.prettyPrint[From]} getters: ($gettersStr) and ${Type.prettyPrint[To]} constructor ($constructorStr)"
          } >>
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
                        // user might have used _.getName in modifier, to define target we know as _.setName
                        // so simple .get(toName) might not be enough
                        .collectFirst {
                          case (name, value) if areNamesMatching(name, toName) =>
                            value
                        }
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
                                    DerivationResult.namedScope(
                                      s"Recursive derivation for field `$sourceName`: ${Type
                                          .prettyPrint[getter.Underlying]} renamed into `${toName}`: ${Type
                                          .prettyPrint[ctorParam.Underlying]}"
                                    ) {
                                      // We're constructing:
                                      // '{ ${ derivedToElement } } // using ${ src.$name }
                                      deriveRecursiveTransformationExpr[getter.Underlying, ctorParam.Underlying](
                                        get(ctx.src)
                                      ).transformWith(
                                        DerivationResult.existential[TransformationExpr, ctorParam.Underlying](_)
                                      ) { _ =>
                                        // TODO: one day we could make this error message recursive
                                        DerivationResult.missingTransformer[
                                          From,
                                          To,
                                          getter.Underlying,
                                          ctorParam.Underlying,
                                          Existential[TransformationExpr]
                                        ](toName)
                                      }
                                    }
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
                        .orElse(fromEnabledExtractors.collectFirst {
                          case _
                              if ctorParam.value.targetType == Product.Parameter.TargetType.SetterParameter && !flags.beanSetters =>
                            // TODO: in future we might want to have some better error message here
                            DerivationResult.notSupportedTransformerDerivation(ctx)
                          case (name, getter) if areNamesMatching(name, toName) =>
                            Existential.use(getter) { implicit Getter: Type[getter.Underlying] =>
                              { case Product.Getter(_, get) =>
                                DerivationResult.namedScope(
                                  s"Recursive derivation for field `$name`: ${Type
                                      .prettyPrint[getter.Underlying]} into matched `${toName}`: ${Type.prettyPrint[ctorParam.Underlying]}"
                                ) {
                                  // We're constructing:
                                  // '{ ${ derivedToElement } } // using ${ src.$name }
                                  deriveRecursiveTransformationExpr[getter.Underlying, ctorParam.Underlying](
                                    get(ctx.src)
                                  ).transformWith(
                                    DerivationResult.existential[TransformationExpr, ctorParam.Underlying](_)
                                  ) { _ =>
                                    // TODO: one day we could make this error message recursive
                                    DerivationResult.missingTransformer[
                                      From,
                                      To,
                                      getter.Underlying,
                                      ctorParam.Underlying,
                                      Existential[TransformationExpr]
                                    ](toName)
                                  }
                                }
                              }
                            }
                        })
                        .orElse(defaultValue.filter(_ => ctx.config.flags.processDefaultValues).map {
                          (value: Expr[ctorParam.Underlying]) =>
                            // We're constructing:
                            // '{ ${ defaultValue } }
                            DerivationResult.existential[TransformationExpr, ctorParam.Underlying](
                              TransformationExpr.fromTotal(value)
                            )
                        })
                        .orElse {
                          Option(Expr.Option.None)
                            .filter(_ => Type[ctorParam.Underlying].isOption && ctx.config.flags.optionDefaultsToNone)
                            .map(value =>
                              // We're constructing:
                              // '{ None }
                              DerivationResult.existential[TransformationExpr, ctorParam.Underlying](
                                TransformationExpr.fromTotal(value.upcastExpr[ctorParam.Underlying])
                              )
                            )

                        }
                        .orElse {
                          Option(Expr.Unit).filter(_ => Type[ctorParam.Underlying] =:= Type[Unit]).map { value =>
                            // We're constructing:
                            // '{ () }
                            DerivationResult.existential[TransformationExpr, ctorParam.Underlying](
                              TransformationExpr.fromTotal(value.upcastExpr[ctorParam.Underlying])
                            )
                          }
                        }
                        .getOrElse {
                          val accessorExists = fieldOverrides
                            .get(toName)
                            .collectFirst { case RuntimeFieldOverride.RenamedFrom(sourceName) => sourceName }
                            .flatMap(fromExtractors.get)
                            .isDefined
                          ctorParam.value.targetType match {
                            case Product.Parameter.TargetType.ConstructorParameter =>
                              DerivationResult
                                .missingAccessor[From, To, ctorParam.Underlying, Existential[TransformationExpr]](
                                  toName,
                                  accessorExists
                                )
                            case Product.Parameter.TargetType.SetterParameter =>
                              DerivationResult
                                .missingJavaBeanSetterParam[From, To, ctorParam.Underlying, Existential[
                                  TransformationExpr
                                ]](
                                  toName
                                )
                          }
                        }
                        .logSuccess(expr => s"Resolved `$toName` field value to ${expr.value.prettyPrint}")
                    }
                  }
                  .map(toName -> _)
              }
              .logSuccess { args =>
                val totals = args.count(_._2.value.isTotal)
                val partials = args.count(_._2.value.isPartial)
                s"Resolved ${args.size} arguments, $totals as total and $partials as partial Expr"
              }
              .map[TransformationExpr[To]] { (resolvedArguments: List[(String, Existential[TransformationExpr])]) =>
                val totalConstructorArguments: Map[String, ExistentialExpr] = resolvedArguments.collect {
                  case (name, exprE) if exprE.value.isTotal => name -> exprE.mapK[Expr](_ => _.ensureTotal)
                }.toMap

                resolvedArguments.collect {
                  case (name, exprE) if exprE.value.isPartial =>
                    name -> exprE.mapK[PartialExpr] { implicit ExprE: Type[exprE.Underlying] =>
                      _.ensurePartial.prependErrorPath(
                        ChimneyExpr.PathElement.Accessor(Expr.String(name)).upcastExpr[partial.PathElement]
                      )
                    }
                } match {
                  case Nil =>
                    // We're constructing:
                    // '{ ${ constructor } }
                    TransformationExpr.fromTotal(constructor(totalConstructorArguments))
                  case (name, res) :: Nil =>
                    // We're constructing:
                    // '{ ${ res }.map($name => ${ constructor }) }
                    Existential.use(res) {
                      implicit Res: Type[res.Underlying] => (resultExpr: Expr[partial.Result[res.Underlying]]) =>
                        TransformationExpr.fromPartial(
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
                            assertionFailed("Expected partial while got total")
                          case TransformationContext.ForPartial(_, failFast) =>
                            TransformationExpr.fromPartial(
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
                            Existential.use(expr) {
                              implicit Expr: Type[expr.Underlying] =>
                                (partialExpr: Expr[partial.Result[expr.Underlying]]) =>
                                  ExprPromise
                                    .promise[partial.Result[expr.Underlying]](
                                      ExprPromise.NameGenerationStrategy.FromPrefix("res"),
                                      ExprPromise.UsageHint.Lazy
                                    )
                                    .map { (inner: Expr[partial.Result[expr.Underlying]]) =>
                                      name -> Existential[PartialExpr, expr.Underlying](inner)
                                    }
                                    .fulfilAsLazy(partialExpr)
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
                            ExprPromise
                              .promise[partial.Result.Errors](
                                ExprPromise.NameGenerationStrategy.FromPrefix("allerrors"),
                                ExprPromise.UsageHint.Var
                              )
                              .fulfilAsVar(Expr.Null.asInstanceOfExpr[partial.Result.Errors])
                              .use { case (allerrors, setAllErrors) =>
                                Expr.block(
                                  partialsAsLazy.map { case (_, result) =>
                                    Existential.use(result) {
                                      implicit Result: Type[result.Underlying] =>
                                        (expr: Expr[partial.Result[result.Underlying]]) =>
                                          // Here, we're building:
                                          // '{ allerrors = partial.Result.Errors.__mergeResultNullable(allerrors, ${ resN }) }
                                          setAllErrors(
                                            ChimneyExpr.PartialResult.Errors.mergeResultNullable(allerrors, expr)
                                          )
                                    }
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
                                            name -> result.mapK[Expr] {
                                              implicit PartialExpr: Type[result.Underlying] =>
                                                (expr: Expr[partial.Result[result.Underlying]]) =>
                                                  expr
                                                    .asInstanceOfExpr[partial.Result.Value[result.Underlying]]
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
              .flatMap(DerivationResult.expanded)
        case _ => DerivationResult.attemptNextRule
      }

    private def areNamesMatching(fromName: String, toName: String): Boolean = {
      import ProductType.{dropGetIs, isGetterName, dropSet, isSetterName}

      def normalizedFromName =
        if (isGetterName(fromName)) dropGetIs(fromName) else fromName
      def normalizedToName =
        if (isGetterName(toName)) dropGetIs(toName) else if (isSetterName(toName)) dropSet(toName) else toName

      fromName == toName || normalizedFromName == normalizedToName
    }

    private val isUsingSetter: ((String, Existential[Product.Parameter])) => Boolean =
      _._2.value.targetType == Product.Parameter.TargetType.SetterParameter
  }
}
