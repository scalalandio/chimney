package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.{DerivationErrors, DerivationResult}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.internal.compiletime.fp.Implicits.*
import io.scalaland.chimney.internal.compiletime.fp.Traverse
import io.scalaland.chimney.partial

private[compiletime] trait TransformProductToProductRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformProductToProductRule extends Rule("ProductToProduct") {

    private type PartialExpr[A] = Expr[partial.Result[A]]

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      // From is checked after To, because extraction always succeeds
      (Type[To], Type[From]) match {
        case (HasCustomConstructor(constructorOverride), Product.Extraction(fromExtractors)) =>
          mapOverridesAndExtractorsToConstructorArguments[From, To](fromExtractors, constructorOverride)
        case (Product.Constructor(parameters, constructor), Product.Extraction(fromExtractors)) =>
          mapOverridesAndExtractorsToConstructorArguments[From, To](fromExtractors, parameters, constructor)
        case _ =>
          DerivationResult.attemptNextRuleBecause(
            s"Type ${Type.prettyPrint[To]} does not have a public primary constructor"
          )
      }

    private object HasCustomConstructor {
      def unapply[A, From, To](
          tpe: Type[A]
      )(implicit ctx: TransformationContext[From, To]): Option[RuntimeConstructorOverride] =
        ctx.config
          .filterOverridesForConstructor { someTo =>
            import someTo.Underlying as SomeTo
            Type[To] <:< Type[SomeTo]
          }
          .collectFirst { case (_, runtimeOverride) => runtimeOverride }
    }

    private def mapOverridesAndExtractorsToConstructorArguments[From, To](
        fromExtractors: Product.Getters[From],
        constructorOverride: RuntimeConstructorOverride
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] = {
      import Product.Constructor.exprAsInstanceOfMethod as mkCtor
      constructorOverride match {
        case RuntimeConstructorOverride.Constructor(idx, args) =>
          val Product.Constructor(parameters, constructor) = mkCtor[To](args)(ctx.runtimeDataStore(idx))
          mapOverridesAndExtractorsToConstructorArguments[From, To](fromExtractors, parameters, constructor)
        case RuntimeConstructorOverride.ConstructorPartial(idx, args) =>
          val Product.Constructor(params, ctor) = mkCtor[partial.Result[To]](args)(ctx.runtimeDataStore(idx))
            .asInstanceOf[Product.Constructor[To]] // a hack to avoid weird conversions back and forth, part 1
          mapOverridesAndExtractorsToConstructorArguments[From, To](fromExtractors, params, ctor).map {
            case Rule.ExpansionResult.Expanded(transformationExpr) =>
              val flattenTransformationExpr =
                // a hack to avoid weird conversions back and forth, part 2
                (transformationExpr.asInstanceOf[TransformationExpr[partial.Result[To]]]) match {
                  case TransformationExpr.PartialExpr(expr) => TransformationExpr.PartialExpr(expr.flatten)
                  case TransformationExpr.TotalExpr(expr)   => TransformationExpr.PartialExpr(expr)
                }
              Rule.ExpansionResult.Expanded(flattenTransformationExpr)
            case Rule.ExpansionResult.AttemptNextRule(reason) => Rule.ExpansionResult.AttemptNextRule(reason)
          }
      }
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
          filterOverridesForField(usedToName =>
            !parameters.keys.exists(toName => areFieldNamesMatching(usedToName, toName))
          ).keys.toList
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

            // .withFieldRenamed(_.isField, _.isField2) has no way if figuring out if user means mapping getter
            // into Field2 or isField2 if there are multiple matching target arguments/setters
            object AmbiguousOverrides {

              def unapply(input: (String, RuntimeFieldOverride)): Option[(String, List[String])] = {
                val (toName, runtimeField) = input
                val ambiguousOverrides = parameters
                  .collect {
                    case (anotherToName, _)
                        if toName == anotherToName || areFieldNamesMatching(toName, anotherToName) =>
                      runtimeField match {
                        case RuntimeFieldOverride.Const(_) =>
                          s".withFieldConst(_.$anotherToName, ...)"
                        case RuntimeFieldOverride.ConstPartial(_) =>
                          s".withFieldConstPartial(_.$anotherToName, ...)"
                        case RuntimeFieldOverride.Computed(_) =>
                          s".withFieldComputed(_.$anotherToName, ...)"
                        case RuntimeFieldOverride.ComputedPartial(_) =>
                          s".withFieldComputedPartial(_.$anotherToName, ...)"
                        case RuntimeFieldOverride.RenamedFrom(sourcePath) =>
                          s".withFieldRenamed(${FieldPath.print(sourcePath)}, _.$anotherToName})"
                      }
                  }
                  .toList
                  .sorted
                if (ambiguousOverrides.size > 1) Some(toName -> ambiguousOverrides) else None
              }
            }

            // User might have used _.getName in modifier, to define target we know as _.setName so simple .get(toName)
            // might not be enough. However, we DO want to prioritize strict name matches.
            filterOverridesForField(_ == toName).headOption
              .orElse(filterOverridesForField(areFieldNamesMatching(_, toName)).headOption)
              .map {
                case AmbiguousOverrides(overrideName, foundOverrides) =>
                  DerivationResult.ambiguousFieldOverrides[From, To, Existential[TransformationExpr]](
                    overrideName,
                    foundOverrides,
                    flags.getFieldNameComparison.toString
                  )
                case (fromName, value) =>
                  useOverride[From, To, CtorParam](fromName, toName, value)
              }
              .orElse {
                val ambiguityOrPossibleSourceField =
                  if (usePositionBasedMatching) Right(ctorParamToGetter.get(ctorParam))
                  else
                    fromEnabledExtractors.collect {
                      case (fromName, getter) if areFieldNamesMatching(fromName, toName) => (fromName, toName, getter)
                    }.toList match {
                      case Nil                  => Right(None)
                      case fromFieldData :: Nil => Right(Some(fromFieldData))
                      case multipleFromNames    => Left(multipleFromNames.map(_._1))
                    }
                ambiguityOrPossibleSourceField match {
                  case Right(possibleSourceField) =>
                    possibleSourceField.map { case (fromName, toName, getter) =>
                      useExtractor[From, To, CtorParam](ctorParam.value.targetType, fromName, toName, getter)
                    }
                  case Left(foundFromNames) =>
                    Some(
                      DerivationResult.ambiguousFieldSources[From, To, Existential[TransformationExpr]](
                        foundFromNames,
                        toName
                      )
                    )
                }
              }
              .orElse(useFallbackValues[From, To, CtorParam](defaultValue))
              .getOrElse[DerivationResult[Existential[TransformationExpr]]] {
                if (usePositionBasedMatching)
                  DerivationResult.tupleArityMismatch(fromArity = fromEnabledExtractors.size, toArity = parameters.size)
                else {
                  lazy val availableGetters = fromExtractors.filter { case (fromName, _) =>
                    areFieldNamesMatching(fromName, toName)
                  }.toList
                  lazy val availableMethodAccessors = availableGetters.collect {
                    case (fromName, getter) if getter.value.sourceType == Product.Getter.SourceType.AccessorMethod =>
                      fromName
                  }
                  lazy val availableInheritedAccessors = availableGetters.collect {
                    case (fromName, getter) if !getter.value.isLocal => fromName
                  }
                  ctorParam.value.targetType match {
                    case Product.Parameter.TargetType.ConstructorParameter =>
                      DerivationResult
                        .missingConstructorArgument[From, To, CtorParam, Existential[TransformationExpr]](
                          toName,
                          availableMethodAccessors,
                          availableInheritedAccessors
                        )
                    case Product.Parameter.TargetType.SetterParameter if (flags.beanSettersIgnoreUnmatched) =>
                      DerivationResult.pure(unmatchedSetter)
                    case Product.Parameter.TargetType.SetterParameter =>
                      DerivationResult
                        .missingJavaBeanSetterParam[From, To, CtorParam, Existential[TransformationExpr]](
                          toName,
                          availableMethodAccessors,
                          availableInheritedAccessors
                        )
                  }
                }
              }
              .logSuccess(expr =>
                if (expr == unmatchedSetter) s"Setter `$toName` not resolved but ignoring setters is allowed"
                else s"Resolved `$toName` field value to ${expr.value.prettyPrint}"
              )
              .map(toName -> _)
          }
          .map(_.filterNot(_._2 == unmatchedSetter))
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
        import ctx.originalSrc.{Underlying as OriginalFrom, value as originalSrc}
        ctx match {
          case TransformationContext.ForTotal(_) =>
            // We're constructing:
            // '{ ${ runtimeDataStore }(idx).asInstanceOf[$OriginalFrom => $CtorParam](${ originalSrc }) }
            DerivationResult.existential[TransformationExpr, CtorParam](
              TransformationExpr.fromTotal(
                ctx.runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[OriginalFrom => CtorParam].apply(originalSrc)
              )
            )
          case TransformationContext.ForPartial(_, _) =>
            // We're constructing:
            // '{
            //   partial.Result.fromFunction(
            //     ${ runtimeDataStore }(idx).asInstanceOf[$OriginalFrom => $CtorParam]
            //   )
            //   .apply(${ originalSrc })
            //   .prependErrorPath(PathElement.Accessor("fromName"))
            // }
            DerivationResult.existential[TransformationExpr, CtorParam](
              TransformationExpr.fromPartial(
                ChimneyExpr.PartialResult
                  .fromFunction(
                    ctx.runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[OriginalFrom => CtorParam]
                  )
                  .apply(originalSrc)
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
        //     .asInstanceOf[$OriginalFrom => partial.Result[$CtorParam]](${ originalSrc })
        //     .prependErrorPath(PathElement.Accessor("fromName"))
        // }
        import ctx.originalSrc.{Underlying as OriginalFrom, value as originalSrc}
        DerivationResult.existential[TransformationExpr, CtorParam](
          TransformationExpr.fromPartial(
            ctx
              .runtimeDataStore(runtimeDataIdx)
              .asInstanceOfExpr[OriginalFrom => partial.Result[CtorParam]]
              .apply(originalSrc)
              .prependErrorPath(
                ChimneyExpr.PathElement.Accessor(Expr.String(fromName)).upcastExpr[partial.PathElement]
              )
          )
        )
      case RuntimeFieldOverride.RenamedFrom(sourcePath) =>
        def extractSource[Source: Type](
            sourceName: String,
            extractedSrcExpr: Expr[Source]
        ): Either[String, ExistentialExpr] = Type[Source] match {
          case Product.Extraction(getters) =>
            getters
              .collectFirst[Either[String, ExistentialExpr]] { case (`sourceName`, getter) =>
                import getter.Underlying as Getter, getter.value.get
                Right(get(extractedSrcExpr).as_??)
              }
              .getOrElse {
                Left(
                  s"""|Assumed that field $sourceName is a part of ${Type.prettyPrint[Source]}, but wasn't found
                      |available methods: ${getters.keys.map(n => s"`$n`").mkString(", ")}""".stripMargin
                )
              }
          case _ =>
            Left(s"""Assumed that field $sourceName is a part of ${Type.prettyPrint[Source]}, but wasn't found""")
        }

        def extractNestedSource(fieldPath: FieldPath): Either[String, ExistentialExpr] = fieldPath match {
          case FieldPath.Select(sourceName, instance) =>
            extractNestedSource(instance).flatMap { extractedSrcValue =>
              import extractedSrcValue.Underlying as ExtractedSourceValue, extractedSrcValue.value as extractedSrcExpr
              extractSource[ExtractedSourceValue](sourceName, extractedSrcExpr)
            }
          case _ => Right(ctx.originalSrc)
        }

        extractNestedSource(sourcePath).fold(
          errorMsg => DerivationResult.assertionError(errorMsg),
          extractedSrc => {
            import extractedSrc.Underlying as ExtractedSrc, extractedSrc.value as extractedSrcExpr
            DerivationResult.namedScope(
              s"Recursive derivation for field `$sourcePath`: ${Type
                  .prettyPrint[ExtractedSrc]} renamed into `$toName`: ${Type
                  .prettyPrint[CtorParam]}"
            ) {
              // We're constructing:
              // '{ ${ derivedToElement } } // using ${ src.$name }
              deriveRecursiveTransformationExpr[ExtractedSrc, CtorParam](
                extractedSrcExpr,
                new DownField(areFieldNamesMatching(_, toName))
              ).transformWith { expr =>
                // If we derived partial.Result[$ctorParam] we are appending
                //  ${ derivedToElement }.prependErrorPath(PathElement.Accessor("fromName"))
                DerivationResult.existential[TransformationExpr, CtorParam](appendPath(expr, sourcePath))
              } { errors =>
                appendMissingTransformer[From, To, ExtractedSrc, CtorParam](errors, toName)
              }
            }
          }
        )
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
          deriveRecursiveTransformationExpr[Getter, CtorParam](
            get(ctx.src),
            new DownField(areFieldNamesMatching(_, toName))
          ).transformWith { expr =>
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
              constructor(totalConstructorArguments + (name -> innerExpr.as_??))
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
                        (name1 -> inner1Expr.as_??) +
                        (name2 -> inner2Expr.as_??)
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
                        constructor(constructorArguments + (name -> innerExpr.as_??))
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
    @scala.annotation.tailrec
    private def appendPath[A: Type](expr: TransformationExpr[A], path: FieldPath): TransformationExpr[A] =
      path match {
        case FieldPath.Prepended(name, instance) => appendPath[A](appendPath[A](expr, name), instance)
        case _                                   => expr
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
      val newError = DerivationResult.missingFieldTransformer[
        From,
        To,
        SourceField,
        TargetField,
        Existential[TransformationExpr]
      ](toName)
      val oldErrors = DerivationResult.fail(errors)
      newError.parTuple(oldErrors).map[Existential[TransformationExpr]](_ => ???)
    }

    private val unmatchedSetter = Existential[TransformationExpr, Null](TransformationExpr.fromTotal(Expr.Null))
  }
}
