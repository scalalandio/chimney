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
          mapOverridesAndExtractorsToConstructorArguments[From, To, To](fromExtractors, parameters, constructor)
        case _ =>
          DerivationResult.attemptNextRuleBecause(
            s"Type ${Type.prettyPrint[To]} does not have a public primary constructor"
          )
      }

    private object HasCustomConstructor {
      def unapply[A, From, To](
          tpe: Type[A]
      )(implicit ctx: TransformationContext[From, To]): Option[TransformerOverride.ForConstructor] =
        ctx.config.currentOverrideForConstructor
    }

    private def mapOverridesAndExtractorsToConstructorArguments[From, To](
        fromExtractors: Product.Getters[From],
        constructorOverride: TransformerOverride.ForConstructor
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] = {
      import Product.Constructor.exprAsInstanceOfMethod as mkCtor
      constructorOverride match {
        case TransformerOverride.Constructor(runtimeData, args) =>
          val Product.Constructor(parameters, constructor) = mkCtor[To](args)(runtimeData)
          mapOverridesAndExtractorsToConstructorArguments[From, To, To](fromExtractors, parameters, constructor)
        case TransformerOverride.ConstructorPartial(runtimeData, args) =>
          val Product.Constructor(params, ctor) = mkCtor[partial.Result[To]](args)(runtimeData)
          mapOverridesAndExtractorsToConstructorArguments[From, To, partial.Result[To]](fromExtractors, params, ctor)
            .map {
              case Rule.ExpansionResult.Expanded(transformationExpr) =>
                val flattenTransformationExpr = // no idea why it doesn't figure that out on its own in Scala 3
                  transformationExpr.asInstanceOf[TransformationExpr[partial.Result[To]]] match {
                    case TransformationExpr.PartialExpr(expr) => TransformationExpr.PartialExpr(expr.flatten)
                    case TransformationExpr.TotalExpr(expr)   => TransformationExpr.PartialExpr(expr)
                  }
                Rule.ExpansionResult.Expanded(flattenTransformationExpr)
              case Rule.ExpansionResult.AttemptNextRule(reason) => Rule.ExpansionResult.AttemptNextRule(reason)
            }
      }
    }

    private def mapOverridesAndExtractorsToConstructorArguments[From, To, ToOrPartialTo: Type](
        fromExtractors: Product.Getters[From],
        parameters: Product.Parameters,
        constructor: Product.Arguments => Expr[ToOrPartialTo]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[ToOrPartialTo]] = {
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
          !getter._2.value.isInherited || flags.inheritedAccessors
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
          filterCurrentOverridesForField(usedToName =>
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
            s"`$k`: ${Type.prettyPrint(using v.Underlying)} (${v.value.sourceType}, ${if (!v.value.isInherited) "declared"
              else "inherited"})"
          }
          .mkString(", ")
        val constructorStr = parameters
          .map { case (k, v) =>
            s"`$k`: ${Type.prettyPrint(using v.Underlying)} (${v.value.targetType}, default = ${v.value.defaultValue
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
            if (flags.nonUnitBeanSetters) parameters.toList
            else
              parameters
                .filter(to =>
                  to._2.value.targetType match {
                    case Product.Parameter.TargetType.SetterParameter(returnedType) =>
                      returnedType.Underlying =:= Type[Unit]
                    case Product.Parameter.TargetType.ConstructorParameter => true
                  }
                )
                .toList
          ) { case (toName: String, ctorParam: Existential[Product.Parameter]) =>
            import ctorParam.Underlying as CtorParam, ctorParam.value.defaultValue

            // .withFieldRenamed(_.isField, _.isField2) has no way if figuring out if user means mapping getter
            // into Field2 or isField2 if there are multiple matching target arguments/setters
            object AmbiguousOverrides {

              def unapply(input: (String, TransformerOverride.ForField)): Option[(String, List[String])] = {
                val (toName, runtimeField) = input
                val ambiguousOverrides = parameters
                  .collect {
                    case (anotherToName, _)
                        if toName == anotherToName || areFieldNamesMatching(toName, anotherToName) =>
                      runtimeField match {
                        case TransformerOverride.Const(_) =>
                          s".withFieldConst(_.$anotherToName, ...)"
                        case TransformerOverride.ConstPartial(_) =>
                          s".withFieldConstPartial(_.$anotherToName, ...)"
                        case TransformerOverride.Computed(_) =>
                          s".withFieldComputed(_.$anotherToName, ...)"
                        case TransformerOverride.ComputedPartial(_) =>
                          s".withFieldComputedPartial(_.$anotherToName, ...)"
                        case TransformerOverride.RenamedFrom(sourcePath) =>
                          s".withFieldRenamed($sourcePath, _.$anotherToName})"
                      }
                  }
                  .toList
                  .sorted
                if (ambiguousOverrides.size > 1) Some(toName -> ambiguousOverrides) else None
              }
            }

            // User might have used _.getName in modifier, to define target we know as _.setName so simple .get(toName)
            // might not be enough. However, we DO want to prioritize strict name matches.
            filterCurrentOverridesForField(_ == toName).headOption
              .orElse(filterCurrentOverridesForField(areFieldNamesMatching(_, toName)).headOption)
              .map {
                case AmbiguousOverrides(overrideName, foundOverrides) =>
                  DerivationResult.ambiguousFieldOverrides[From, To, Existential[TransformationExpr]](
                    overrideName,
                    foundOverrides,
                    flags.getFieldNameComparison.toString
                  )
                case (_, value) =>
                  useOverride[From, To, CtorParam](toName, value).flatMap(
                    DerivationResult.existential[TransformationExpr, CtorParam](_)
                  )
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
              .orElse {
                useFallbackValues[From, To, CtorParam](
                  defaultValue.orElse(summonDefaultValue[CtorParam].map(_.provide()))
                )
              }
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
                    case (fromName, getter) if getter.value.isInherited => fromName
                  }
                  ctorParam.value.targetType match {
                    case Product.Parameter.TargetType.ConstructorParameter =>
                      DerivationResult
                        .missingConstructorArgument[From, To, CtorParam, Existential[TransformationExpr]](
                          toName,
                          availableMethodAccessors,
                          availableInheritedAccessors,
                          availableDefault = defaultValue.isDefined,
                          availableNone = OptionalValue.unapply[CtorParam].isDefined
                        )
                    case Product.Parameter.TargetType.SetterParameter(_) if flags.beanSettersIgnoreUnmatched =>
                      DerivationResult.pure(unmatchedSetter)
                    case Product.Parameter.TargetType.SetterParameter(returnedType)
                        if !flags.nonUnitBeanSetters && !(returnedType.Underlying =:= Type[Unit]) =>
                      DerivationResult.pure(nonUnitSetter)
                    case Product.Parameter.TargetType.SetterParameter(_) =>
                      DerivationResult
                        .missingJavaBeanSetterParam[From, To, CtorParam, Existential[TransformationExpr]](
                          toName,
                          availableMethodAccessors,
                          availableInheritedAccessors,
                          availableNone = OptionalValue.unapply[CtorParam].isDefined
                        )
                  }
                }
              }
              .logSuccess {
                case `unmatchedSetter` => s"Setter `$toName` not resolved but ignoring setters is allowed"
                case `nonUnitSetter` =>
                  s"Setter `$toName` not resolved it has non-Unit return type and they are ignored"
                case expr => s"Resolved `$toName` field value to ${expr.value.prettyPrint}"
              }
              .map(toName -> _)
          }
          .map(_.filterNot(_._2 == unmatchedSetter).filterNot(_._2 == nonUnitSetter))
          .logSuccess { args =>
            val totals = args.count(_._2.value.isTotal)
            val partials = args.count(_._2.value.isPartial)
            s"Resolved ${args.size} arguments, $totals as total and $partials as partial Expr"
          }
          .map[TransformationExpr[ToOrPartialTo]] {
            (resolvedArguments: List[(String, Existential[TransformationExpr])]) =>
              wireArgumentsToConstructor[From, To, ToOrPartialTo](resolvedArguments, constructor)
          }
          .flatMap(DerivationResult.expanded)
    }

    // TODO: perhaps we should NOT pass To's field name as From's field name when providing errors to overrides?
    def useOverride[From, To, CtorParam: Type](
        toName: String,
        runtimeFieldOverride: TransformerOverride.ForField
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[TransformationExpr[CtorParam]] = runtimeFieldOverride match {
      case TransformerOverride.Const(runtimeData) =>
        // We're constructing:
        // '{ ${ runtimeDataStore }(idx).asInstanceOf[$ctorParam] }
        DerivationResult.pure(
          TransformationExpr.fromTotal(
            runtimeData.asInstanceOfExpr[CtorParam]
          )
        )
      case TransformerOverride.ConstPartial(runtimeData) =>
        // We're constructing:
        // '{
        //   ${ runtimeDataStore }(idx)
        //     .asInstanceOf[partial.Result[$ctorParam]]
        //     .prependErrorPath(PathElement.Accessor("toName"))
        //  }
        DerivationResult.pure(
          TransformationExpr.fromPartial(
            runtimeData
              .asInstanceOfExpr[partial.Result[CtorParam]]
              .prependErrorPath(
                ChimneyExpr.PathElement.Accessor(Expr.String(toName)).upcastToExprOf[partial.PathElement]
              )
          )
        )
      case TransformerOverride.Computed(runtimeData) =>
        import ctx.originalSrc.{Underlying as OriginalFrom, value as originalSrc}
        ctx match {
          case TransformationContext.ForTotal(_) =>
            // We're constructing:
            // '{ ${ runtimeDataStore }(idx).asInstanceOf[$OriginalFrom => $CtorParam](${ originalSrc }) }
            DerivationResult.pure(
              TransformationExpr.fromTotal(
                runtimeData.asInstanceOfExpr[OriginalFrom => CtorParam].apply(originalSrc)
              )
            )
          case TransformationContext.ForPartial(_, _) =>
            // We're constructing:
            // '{
            //   partial.Result.fromFunction(
            //     ${ runtimeDataStore }(idx).asInstanceOf[$OriginalFrom => $CtorParam]
            //   )
            //   .apply(${ originalSrc })
            //   .prependErrorPath(PathElement.Accessor("toName"))
            // }
            DerivationResult.pure(
              TransformationExpr.fromPartial(
                ChimneyExpr.PartialResult
                  .fromFunction(runtimeData.asInstanceOfExpr[OriginalFrom => CtorParam])
                  .apply(originalSrc)
                  .prependErrorPath(
                    ChimneyExpr.PathElement
                      .Accessor(Expr.String(toName))
                      .upcastToExprOf[partial.PathElement]
                  )
              )
            )
        }
      case TransformerOverride.ComputedPartial(runtimeData) =>
        // We're constructing:
        // '{
        //   ${ runtimeDataStore }(idx)
        //     .asInstanceOf[$OriginalFrom => partial.Result[$CtorParam]](${ originalSrc })
        //     .prependErrorPath(PathElement.Accessor("toName"))
        // }
        import ctx.originalSrc.{Underlying as OriginalFrom, value as originalSrc}
        DerivationResult.pure(
          TransformationExpr.fromPartial(
            runtimeData
              .asInstanceOfExpr[OriginalFrom => partial.Result[CtorParam]]
              .apply(originalSrc)
              .prependErrorPath(
                ChimneyExpr.PathElement.Accessor(Expr.String(toName)).upcastToExprOf[partial.PathElement]
              )
          )
        )
      case TransformerOverride.RenamedFrom(sourcePath) =>
        def extractSource[Source: Type](
            sourceName: String,
            extractedSrcExpr: Expr[Source]
        ): DerivationResult[ExistentialExpr] = Type[Source] match {
          case Product.Extraction(getters) =>
            getters.filter { case (fromName, _) => areFieldNamesMatching(fromName, sourceName) }.toList match {
              case Nil =>
                DerivationResult.assertionError(
                  s"""|Assumed that field $sourceName is a part of ${Type.prettyPrint[Source]}, but wasn't found
                      |available methods: ${getters.keys.map(n => s"`$n`").mkString(", ")}""".stripMargin
                )
              case (_, getter) :: Nil =>
                import getter.Underlying as Getter, getter.value.get
                DerivationResult.pure(get(extractedSrcExpr).as_??)
              case matchingGetters =>
                DerivationResult.ambiguousFieldOverrides[From, To, ExistentialExpr](
                  sourceName,
                  matchingGetters.map(_._1).sorted,
                  ctx.config.flags.getFieldNameComparison.toString
                )
            }
          case _ =>
            DerivationResult.assertionError(
              s"""Assumed that field $sourceName is a part of ${Type.prettyPrint[Source]}, but wasn't found"""
            )
        }

        def extractNestedSource(path: Path, extractedSrcValue: ExistentialExpr): DerivationResult[ExistentialExpr] =
          path match {
            case Path.Root =>
              DerivationResult.pure(extractedSrcValue)
            case Path.AtField(sourceName, path2) =>
              import extractedSrcValue.Underlying as ExtractedSourceValue, extractedSrcValue.value as extractedSrcExpr
              extractSource[ExtractedSourceValue](sourceName, extractedSrcExpr).flatMap { extractedSrcValue2 =>
                extractNestedSource(path2, extractedSrcValue2)
              }
            case path =>
              DerivationResult.notSupportedRenameFromPath[From, To, ExistentialExpr](
                toName,
                path,
                ctx.srcJournal.last._1
              )
          }

        val extractedNestedSourceCandidates = for {
          (prefixPath, prefixExpr) <- ctx.srcJournal.reverseIterator
          newSourcePath <- sourcePath.drop(prefixPath).iterator
        } yield extractNestedSource(newSourcePath, prefixExpr)

        val extractedSrcResult = extractedNestedSourceCandidates
          .fold(extractedNestedSourceCandidates.next()) { (a, b) =>
            // We're not using orElse because we want to:
            // - find the first successful result
            // - but NOT aggregate the errors, if everything fails, keep only the first error
            a.recoverWith(errors => b.recoverWith(_ => DerivationResult.fail(errors)))
          }

        extractedSrcResult.flatMap { extractedSrc =>
          import extractedSrc.Underlying as ExtractedSrc, extractedSrc.value as extractedSrcExpr
          DerivationResult.namedScope(
            s"Recursive derivation for field `$sourcePath`: ${Type
                .prettyPrint[ExtractedSrc]} renamed into `$toName`: ${Type.prettyPrint[CtorParam]}"
          ) {
            // We're constructing:
            // '{ ${ derivedToElement } } // using ${ src.$name }
            deriveRecursiveTransformationExpr[ExtractedSrc, CtorParam](
              extractedSrcExpr,
              sourcePath,
              Path(_.select(toName))
            )
              .transformWith { expr =>
                // If we derived partial.Result[$ctorParam] we are appending:
                //  ${ derivedToElement }.prependErrorPath(...).prependErrorPath(...) // sourcePath
                DerivationResult.pure(appendPath(expr, sourcePath))
              } { errors =>
                appendMissingTransformer[From, To, ExtractedSrc, CtorParam](errors, toName)
              }
          }
        }
    }

    // Exposes logic for: OptionToOption, EitherToEither, ...
    def useOverrideIfPresentOr[From, To, CtorParam: Type](
        toName: String,
        runtimeFieldOverrides: Set[TransformerOverride.ForField]
    )(whenAbsent: => DerivationResult[TransformationExpr[CtorParam]])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[TransformationExpr[CtorParam]] = runtimeFieldOverrides.toList match {
      case Nil =>
        whenAbsent
      case runtimeFieldOverride :: Nil =>
        TransformProductToProductRule.useOverride[From, To, CtorParam](toName, runtimeFieldOverride)
      // $COVERAGE-OFF$Config parsing dedupliate values
      case runtimeFieldOverrides =>
        DerivationResult.assertionError(s"Unexpected multiple overrides: ${runtimeFieldOverrides.mkString(", ")}")
      // $COVERAGE-ON$
    }

    private def useExtractor[From, To, CtorParam: Type](
        ctorTargetType: Product.Parameter.TargetType,
        fromName: String,
        toName: String,
        getter: Existential[Product.Getter[From, *]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Existential[TransformationExpr]] = ctorTargetType match {
      case Product.Parameter.TargetType.SetterParameter(_) if !ctx.config.flags.beanSetters =>
        DerivationResult
          .notSupportedTransformerDerivation(ctx)
          .log(s"Matched $fromName to $toName but $toName is setter and they are disabled")
      case _ =>
        import getter.Underlying as Getter, getter.value.get
        DerivationResult.namedScope(
          s"Recursive derivation for field `$fromName`: ${Type
              .prettyPrint[Getter]} into matched `$toName`: ${Type.prettyPrint[CtorParam]}"
        ) {
          // We're constructing:
          // '{ ${ derivedToElement } } // using ${ src.$name }
          deriveRecursiveTransformationExpr[Getter, CtorParam](
            get(ctx.src),
            Path(_.select(fromName)),
            Path(_.select(toName))
          ).transformWith { expr =>
            // If we derived partial.Result[$ctorParam] we are appending:
            //  ${ derivedToElement }.prependErrorPath(PathElement.Accessor("fromName"))
            DerivationResult.existential[TransformationExpr, CtorParam](appendPath(expr, fromName))
          } { errors =>
            appendMissingTransformer[From, To, Getter, CtorParam](errors, toName)
          }
        }
    }

    private def useFallbackValues[From, To, CtorParam: Type](
        defaultValue: => Option[Expr[CtorParam]]
    )(implicit ctx: TransformationContext[From, To]): Option[DerivationResult[Existential[TransformationExpr]]] = {
      def useDefaultValue: Option[DerivationResult[Existential[TransformationExpr]]] =
        // Default values are provided from ProductType parsing.
        if (ctx.config.flags.isDefaultValueEnabledGloballyOrFor[CtorParam]) {
          defaultValue.map { (value: Expr[CtorParam]) =>
            // We're constructing:
            // '{ ${ defaultValue } }
            DerivationResult.existential[TransformationExpr, CtorParam](
              TransformationExpr.fromTotal(value)
            )
          }
        } else None

      def useNone: Option[DerivationResult[Existential[TransformationExpr]]] =
        // OptionalValue handles both scala.Options as well as a support provided through integrations.OptionalValue.
        OptionalValue.unapply[CtorParam].filter(_ => ctx.config.flags.optionDefaultsToNone).map { optional =>
          // We're constructing:
          // '{ None }
          DerivationResult.existential[TransformationExpr, CtorParam](
            TransformationExpr.fromTotal(optional.value.empty)
          )
        }

      def useSingletonType: Option[DerivationResult[Existential[TransformationExpr]]] =
        // Singletons are always supported as a fallback (except None as this is explicitly handled with a flag).
        SingletonType.parse[CtorParam].filterNot(_ => Type[CtorParam] =:= Type[None.type]).map { singleton =>
          // We're constructing:
          // '{ singleton } // e.g. (), null, case object, val, Java enum
          DerivationResult.existential[TransformationExpr, CtorParam](
            TransformationExpr.fromTotal(singleton.value.upcastToExprOf[CtorParam])
          )
        }

      useDefaultValue.orElse(useNone).orElse(useSingletonType)
    }

    private def wireArgumentsToConstructor[From, To, ToOrPartialTo: Type](
        resolvedArguments: List[(String, Existential[TransformationExpr])],
        constructor: Product.Arguments => Expr[ToOrPartialTo]
    )(implicit ctx: TransformationContext[From, To]): TransformationExpr[ToOrPartialTo] = {
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
            // $COVERAGE-OFF$should never happen unless we messed up
            case TransformationContext.ForTotal(_) =>
              assertionFailed("Expected partial while got total")
            // $COVERAGE-ON$
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
                val failFastBranch: Expr[partial.Result[ToOrPartialTo]] = {
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
                  ): Expr[partial.Result[ToOrPartialTo]] = unusedPartials match {
                    // Should never happen
                    case Nil => ???
                    // last result to compose in - use .map instead of .flatMap
                    case (name, res) :: Nil =>
                      import res.{Underlying as Res, value as resultToMap}
                      resultToMap.map(Expr.Function1.instance[Res, ToOrPartialTo] { (innerExpr: Expr[Res]) =>
                        constructor(constructorArguments + (name -> innerExpr.as_??))
                      })
                    // use .flatMap
                    case (name, res) :: tail =>
                      import res.{Underlying as Res, value as resultToFlatMap}
                      resultToFlatMap.flatMap(
                        Expr.Function1.instance[Res, partial.Result[ToOrPartialTo]] { (innerExpr: Expr[Res]) =>
                          nestFlatMaps(tail, constructorArguments + (name -> ExistentialExpr(innerExpr)))
                        }
                      )
                  }

                  nestFlatMaps(partialsAsLazy.toList, totalConstructorArguments)
                }

                val fullErrorBranch: Expr[partial.Result[ToOrPartialTo]] =
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
                        Expr.ifElse[partial.Result[ToOrPartialTo]](allerrors `eqExpr` Expr.Null) {
                          // Here, we're building:
                          // '{ partial.Result.Value(${ constructor }) } // using res1.asInstanceOf[partial.Result.Value[Tpe]].value, ...
                          ChimneyExpr.PartialResult
                            .Value[ToOrPartialTo](
                              constructor(
                                totalConstructorArguments ++ partialsAsLazy.map { case (name, result) =>
                                  import result.Underlying as Res
                                  name -> result.mapK[Expr] { _ => (expr: Expr[partial.Result[Res]]) =>
                                    expr.asInstanceOfExpr[partial.Result.Value[Res]].value
                                  }
                                }
                              )
                            )
                            .upcastToExprOf[partial.Result[ToOrPartialTo]]
                        } {
                          allerrors.upcastToExprOf[partial.Result[ToOrPartialTo]]
                        }
                      )
                    }

                ctx match {
                  // $COVERAGE-OFF$should never happen unless we messed up
                  case TransformationContext.ForTotal(_) =>
                    assertionFailed("Expected partial, got total")
                  // $COVERAGE-ON$
                  case TransformationContext.ForPartial(_, failFast) =>
                    // Finally, we are combining:
                    // if (${ failFast }) {
                    //   ${ failFastBranch }
                    // } else {
                    //   ${ fullErrorBranch }
                    // }
                    Expr.ifElse[partial.Result[ToOrPartialTo]](failFast)(failFastBranch)(fullErrorBranch)
                }
              }
          )
      }
    }

    // If we derived partial.Result[$ctorParam] we are appending:
    //  ${ derivedToElement }.prependErrorPath(PathElement.Accessor("fromName"))
    @scala.annotation.tailrec
    private def appendPath[A: Type](expr: TransformationExpr[A], path: Path): TransformationExpr[A] =
      path match {
        case Path.AtField(name, path2) => appendPath[A](appendPath[A](expr, name), path2)
        case _                         => expr // Path.Root - other values are not possible in from Path for renames
      }

    // If we derived partial.Result[$ctorParam] we are appending:
    //  ${ derivedToElement }.prependErrorPath(PathElement.Accessor("fromName"))
    private def appendPath[A: Type](expr: TransformationExpr[A], path: String): TransformationExpr[A] =
      expr.fold(TransformationExpr.fromTotal)(partialE =>
        TransformationExpr.fromPartial(
          partialE.prependErrorPath(
            ChimneyExpr.PathElement
              .Accessor(Expr.String(path))
              .upcastToExprOf[partial.PathElement]
          )
        )
      )

    private def appendMissingTransformer[From, To, SourceField: Type, TargetField: Type](
        errors: DerivationErrors,
        toName: String
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Nothing] = {
      val newError = DerivationResult.missingFieldTransformer[
        From,
        To,
        SourceField,
        TargetField,
        TransformationExpr[TargetField]
      ](toName)
      val oldErrors = DerivationResult.fail(errors)
      newError.parTuple(oldErrors).map[Nothing](_ => ???)
    }

    // Stub to use when the setter's return type is not Unit and nonUnitBeanSetters flag is off.
    private val nonUnitSetter = Existential[TransformationExpr, Null](TransformationExpr.fromTotal(Expr.Null))

    // Stub to use when the setter's was not matched and beanSettersIgnoreUnmatched flag is on.
    private val unmatchedSetter = Existential[TransformationExpr, Null](TransformationExpr.fromTotal(Expr.Null))
  }
}
