package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.dsl.FailOnIgnoredSourceVal
import io.scalaland.chimney.internal.compiletime.NotSupportedOperationFromPath.Operation as FromOperation
import io.scalaland.chimney.internal.compiletime.{DerivationErrors, DerivationResult}
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.internal.compiletime.fp.Implicits.*
import io.scalaland.chimney.internal.compiletime.fp.Traverse
import io.scalaland.chimney.partial

import scala.annotation.unused
import scala.collection.immutable.{ListMap, SortedSet}

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
            s"Type ${Type.prettyPrint[To]} does not have a public primary constructor NOR exactly one (non-primary) public constructor"
          )
      }

    private object HasCustomConstructor {
      def unapply[A, From, To](
          @unused tpe: Type[A]
      )(implicit ctx: TransformationContext[From, To]): Option[TransformerOverride.ForConstructor] =
        ctx.config.currentOverrideForConstructor
    }

    private def mapOverridesAndExtractorsToConstructorArguments[From, To](
        fromExtractors: Product.Getters[From],
        constructorOverride: TransformerOverride.ForConstructor
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] = {
      import Product.Constructor.exprAsInstanceOfMethod as mkCtor
      constructorOverride match {
        case TransformerOverride.Constructor(args, runtimeData) =>
          val Product.Constructor(parameters, constructor) = mkCtor[To](args)(runtimeData)
          mapOverridesAndExtractorsToConstructorArguments[From, To, To](fromExtractors, parameters, constructor)
        case TransformerOverride.ConstructorPartial(args, runtimeData) =>
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

      lazy val fromEnabledExtractors = fromExtractors.filter { case (name, getter) =>
        filterAllowedFieldsByFlags(flags.at(TargetPath(Path(_.select(name)))))(getter)
      }

      val usePositionBasedMatching =
        Type[From].isTuple || Type[To].isTuple || ctx.config.filterCurrentOverridesForFallbacks.exists {
          case TransformerOverride.Fallback(runtimeData) => runtimeData.Underlying.isTuple
        }
      lazy val ctorParamToGetterByPosition = {
        val fromGetters = fromExtractors.view.map { case (fromName, fromGetter) =>
          FromOrFallbackGetter[From](ctx.src, fromName, fromGetter)
        }
        val fallbackGetters = ctx.config.filterCurrentOverridesForFallbacks.toVector.flatMap {
          case TransformerOverride.Fallback(runtimeData) =>
            import runtimeData.{Underlying as Fallback, value as fallbackExpr}
            Type[Fallback] match {
              case Product.Extraction(fallbackExtractors) =>
                fallbackExtractors.view.map { case (fallbackName, fallbackGetter) =>
                  FromOrFallbackGetter[Fallback](fallbackExpr, fallbackName, fallbackGetter)
                }
              case _ => Vector.empty[FromOrFallbackGetter].view
            }
        }
        // tuples should use ONLY vals
        val valGetters = (fromGetters ++ fallbackGetters).filter { case fof =>
          // ConstructorBodyVals in position-based matching could be enabled by a flag
          !fof.getter.value.isInherited && fof.getter.value.sourceType == Product.Getter.SourceType.ConstructorArgVal
        }
        parameters.view
          .zip(valGetters)
          .map { case ((toName, ctorParam), fromOrFallback) =>
            ctorParam -> (toName -> fromOrFallback)
          }
          .toMap
      }

      val verifyNoOverrideUnused: DerivationResult[List[Nothing]] = Traverse[List]
        .parTraverse[DerivationResult, String, Nothing](
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

      val fromNamesUsedByExtractors = scala.collection.mutable.ListBuffer.empty[String]
      val fromNamesExplicitlyUnused = ctx.config.filterCurrentUnusedFields

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
        s"Resolved ${Type.prettyPrint[From]} getters: ($gettersStr) and ${Type
            .prettyPrint[To]} constructor ($constructorStr), using ${if (usePositionBasedMatching) "position-based matching (tuple present)"
          else "name-based matching"}"
      } >> verifyNoOverrideUnused >>
        Traverse[List]
          .parTraverse[
            DerivationResult,
            (String, Existential[Product.Parameter]),
            (String, Existential[TransformationExpr])
          ](
            parameters.filter { case (toName, param) =>
              flags.atTgt(_.select(toName)).nonUnitBeanSetters || (param.value.targetType match {
                case Product.Parameter.TargetType.SetterParameter(returnedType) =>
                  returnedType.Underlying =:= Type[Unit]
                case Product.Parameter.TargetType.ConstructorParameter => true
              })
            }.toList
          ) { case (toName: String, ctorParam: Existential[Product.Parameter]) =>
            import ctorParam.Underlying as CtorParam, ctorParam.value.defaultValue

            val fieldFlags = flags.atTgt(_.select(toName))

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
                        case TransformerOverride.Unused =>
                          ".withFieldUnused(...)"
                        case TransformerOverride.Const(_) =>
                          s".withFieldConst(_.$anotherToName, ...)"
                        case TransformerOverride.ConstPartial(_) =>
                          s".withFieldConstPartial(_.$anotherToName, ...)"
                        case TransformerOverride.Computed(_, _, _) =>
                          s".withFieldComputed(_.$anotherToName, ...)"
                        case TransformerOverride.ComputedPartial(_, _, _) =>
                          s".withFieldComputedPartial(_.$anotherToName, ...)"
                        case TransformerOverride.Renamed(sourcePath, _) =>
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
                    flags.getFieldNameComparison.toString // name comparison is defined for nested fields, not the field itself
                  )
                case (_, value) =>
                  useOverride[From, To, CtorParam](toName, value).flatMap(
                    DerivationResult.existential[TransformationExpr, CtorParam](_)
                  )
              }
              .orElse {
                if (usePositionBasedMatching) {
                  ctorParamToGetterByPosition.get(ctorParam).collect {
                    case (toName, fromOrFallback) if !fromNamesExplicitlyUnused(fromOrFallback.name) =>
                      import fromOrFallback.{FromOrFallback, src as srcOrFallback, name as fromName, getter}
                      fromNamesUsedByExtractors += fromName
                      implicit val positionBasedCtx: TransformationContext[FromOrFallback, To] =
                        ctx.updateFromTo[FromOrFallback, To](newSrc = srcOrFallback)(using FromOrFallback, ctx.To)
                      useExtractor[FromOrFallback, To, CtorParam](ctorParam.value.targetType, fromName, toName, getter)
                  }
                } else {
                  val ambiguityOrPossibleSourceField = fromEnabledExtractors.collect {
                    case (fromName, getter)
                        if areFieldNamesMatching(fromName, toName) && !fromNamesExplicitlyUnused(fromName) =>
                      (fromName, toName, getter)
                  }.toList match {
                    case Nil                  => Right(None)
                    case fromFieldData :: Nil => Right(Some(fromFieldData))
                    case multipleFromNames    => Left(multipleFromNames.map(_._1))
                  }
                  ambiguityOrPossibleSourceField match {
                    case Right(possibleSourceField) =>
                      possibleSourceField.map { case (fromName, toName, getter) =>
                        fromNamesUsedByExtractors += fromName
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
              }
              .orElse {
                useFallbackValues[From, To, CtorParam](toName) {
                  defaultValue.orElse(summonDefaultValue[CtorParam].map(_.provide()))
                }
              }
              .getOrElse[DerivationResult[Existential[TransformationExpr]]] {
                if (usePositionBasedMatching) {
                  val arities = ctorParamToGetterByPosition.view.zipWithIndex
                    .collect { case ((_, (_, sof)), idx) => sof.src -> idx }
                    .groupBy(_._1)
                    .view
                    .mapValues(_.map(_._2))
                    .map { case (_, vals) => vals.min -> vals.size }
                    .toList
                    .sortBy(_._1)
                    .map(_._2)
                  DerivationResult.tupleArityMismatch(
                    fromArity = arities.headOption.getOrElse(0),
                    toArity = parameters.size,
                    fallbackArity = arities.drop(1)
                  )
                } else {
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
                          availableNone = OptionalValue.parse[CtorParam].isDefined
                        )
                    case Product.Parameter.TargetType.SetterParameter(_) if fieldFlags.beanSettersIgnoreUnmatched =>
                      DerivationResult.pure(unmatchedSetter)
                    case Product.Parameter.TargetType.SetterParameter(returnedType)
                        if !fieldFlags.nonUnitBeanSetters && !(returnedType.Underlying =:= Type[Unit]) =>
                      DerivationResult.pure(nonUnitSetter)
                    case Product.Parameter.TargetType.SetterParameter(_) =>
                      DerivationResult
                        .missingJavaBeanSetterParam[From, To, CtorParam, Existential[TransformationExpr]](
                          toName,
                          availableMethodAccessors,
                          availableInheritedAccessors,
                          availableNone = OptionalValue.parse[CtorParam].isDefined
                        )
                  }
                }
              }
              .logSuccess {
                case `unmatchedSetter` => s"Setter `$toName` not resolved but ignoring setters is allowed"
                case `nonUnitSetter`   =>
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
          .flatTap { _ =>
            checkPolicy(
              fromExtractors.view
                .collect {
                  case (fromName, getter)
                      if getter.value.sourceType == Product.Getter.SourceType.ConstructorArgVal || getter.value.sourceType == Product.Getter.SourceType.ConstructorBodyVal =>
                    fromName
                }
                .to(SortedSet),
              fromNamesUsedByExtractors.toSet,
              fromNamesExplicitlyUnused
            )
          }
          .map[TransformationExpr[ToOrPartialTo]] {
            (resolvedArguments: List[(String, Existential[TransformationExpr])]) =>
              wireArgumentsToConstructor[From, To, ToOrPartialTo](resolvedArguments, constructor)
          }
          .flatMap(DerivationResult.expanded)
    }

    private def useOverride[From, To, CtorParam: Type](
        toName: String,
        runtimeFieldOverride: TransformerOverride.ForField
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[TransformationExpr[CtorParam]] = runtimeFieldOverride match {
      case TransformerOverride.Unused =>
        DerivationResult.assertionError("Unused field override should have been checked on source side Path")
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
        //     .prependErrorPath(PathElement.Const("_.toName"))
        //  }
        DerivationResult.pure(
          TransformationExpr.fromPartial(
            runtimeData
              .asInstanceOfExpr[partial.Result[CtorParam]]
              .prependErrorPath(
                ChimneyExpr.PathElement
                  .Const(Expr.String(s"${ctx.currentTgt}.$toName"))
                  .upcastToExprOf[partial.PathElement]
              )
          )
        )
      case TransformerOverride.Computed(sourcePath, _, runtimeData) =>
        extractSrcByPath(FromOperation.Computed, sourcePath, toName).map { extractedSrc =>
          import extractedSrc.Underlying as ExtractedSrc, extractedSrc.value as extractedSrcExpr
          ctx match {
            case TransformationContext.ForTotal(_) =>
              // We're constructing:
              // '{ ${ runtimeDataStore }(idx).asInstanceOf[$ExtractedSrc => $CtorParam](${ extractedSrcExpr }) }
              TransformationExpr.fromTotal(
                runtimeData.asInstanceOfExpr[ExtractedSrc => CtorParam].apply(extractedSrcExpr)
              )
            case TransformationContext.ForPartial(_, _) =>
              // We're constructing:
              // '{
              //   partial.Result.fromFunction(
              //     ${ runtimeDataStore }(idx).asInstanceOf[$ExtractedSrc => $CtorParam]
              //   )
              //   .apply(${ extractedSrcExpr })
              //   // prepend sourcePath
              //   .prependErrorPath(PathElement.Computed("_.toName"))
              // }
              TransformationExpr.fromPartial(
                prependWholeErrorPath(
                  ChimneyExpr.PartialResult
                    .fromFunction(runtimeData.asInstanceOfExpr[ExtractedSrc => CtorParam])
                    .apply(extractedSrcExpr),
                  sourcePath
                )
                  .prependErrorPath(
                    ChimneyExpr.PathElement
                      .Computed(Expr.String(s"${ctx.currentTgt}.$toName"))
                      .upcastToExprOf[partial.PathElement]
                  )
              )
          }
        }
      case TransformerOverride.ComputedPartial(sourcePath, _, runtimeData) =>
        extractSrcByPath(FromOperation.ComputedPartial, sourcePath, toName).map { extractedSrc =>
          import extractedSrc.Underlying as ExtractedSrc, extractedSrc.value as extractedSrcExpr
          // We're constructing:
          // '{
          //   ${ runtimeDataStore }(idx)
          //     .asInstanceOf[$ExtractedSrc => partial.Result[$CtorParam]](${ extractedSrcExpr })
          //     // prepend sourcePath
          //     .prependErrorPath(PathElement.Computed("_.toName"))
          // }
          TransformationExpr.fromPartial(
            prependWholeErrorPath(
              runtimeData
                .asInstanceOfExpr[ExtractedSrc => partial.Result[CtorParam]]
                .apply(extractedSrcExpr),
              sourcePath
            )
              .prependErrorPath(
                ChimneyExpr.PathElement
                  .Computed(Expr.String(s"${ctx.currentTgt}.$toName"))
                  .upcastToExprOf[partial.PathElement]
              )
          )
        }
      case TransformerOverride.Renamed(sourcePath, _) =>
        extractSrcByPath(FromOperation.Renamed, sourcePath, toName).flatMap { extractedSrc =>
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
              Path(_.select(toName)),
              findMatchingUpdateCandidates(toName)
            )
              .transformWith { expr =>
                // If we derived partial.Result[$ctorParam] we are appending:
                //  ${ derivedToElement }.prependErrorPath(...).prependErrorPath(...) // sourcePath
                DerivationResult.pure(expr.fold(TransformationExpr.fromTotal) { partialExpr =>
                  TransformationExpr.fromPartial(prependWholeErrorPath(partialExpr, sourcePath))
                })
              } { errors =>
                appendMissingTransformer[From, To, ExtractedSrc, CtorParam](errors, toName)
              }
          }
        }
    }

    private def extractSrcByPath[From, To](operation: FromOperation, sourcePath: Path, toName: String)(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[ExistentialExpr] = {
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
                ctx.config.flags.getFieldNameComparison.toString // name comparison is defined for nested fields, not the field itself
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
            DerivationResult.notSupportedOperationFromPath[From, To, ExistentialExpr](
              operation,
              toName,
              path,
              ctx.srcJournal.last._1
            )
        }

      val extractedNestedSourceCandidates = for {
        (prefixPath, prefixExpr) <- ctx.srcJournal.reverseIterator
        newSourcePath <- sourcePath.drop(prefixPath).iterator
      } yield extractNestedSource(newSourcePath, prefixExpr)

      extractedNestedSourceCandidates.fold(extractedNestedSourceCandidates.next()) { (a, b) =>
        // We're not using orElse because we want to:
        // - find the first successful result
        // - but NOT aggregate the errors, if everything fails, keep only the first error
        a.recoverWith(errors => b.recoverWith(_ => DerivationResult.fail(errors)))
      }
    }

    // Exposes logic for: OptionToOption, EitherToEither, IterableToIterable, MapToMap...
    def useOverrideIfPresentOr[From, To, CtorParam: Type](
        toName: String,
        runtimeFieldOverrides: Set[TransformerOverride.ForField]
    )(whenAbsent: => DerivationResult[TransformationExpr[CtorParam]])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[TransformationExpr[CtorParam]] = runtimeFieldOverrides.toList match {
      case Nil =>
        whenAbsent
      case runtimeFieldOverride :: Nil =>
        import io.scalaland.chimney.internal.compiletime.DerivationError.TransformerError as TError
        import io.scalaland.chimney.internal.compiletime.NotSupportedOperationFromPath as NotSupportedFrom
        useOverride[From, To, CtorParam](toName, runtimeFieldOverride).recoverWith {
          case DerivationErrors(TError(NotSupportedFrom(_, `toName`, _, _)), Vector()) =>
            // If we cannot extract value in .withFieldComputedFrom/.withFieldComputedPartialFrom, it might be because
            // path is matching on TargetSide, but SourceSide requires recursion, TransformationContext update,
            // and then matching on some other rule.
            whenAbsent
          case errors => DerivationResult.fail(errors)
        }
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
      case Product.Parameter.TargetType.SetterParameter(_) if !ctx.config.flags.atTgt(_.select(toName)).beanSetters =>
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
            Path(_.select(toName)),
            findMatchingUpdateCandidates(toName)
          ).transformWith { expr =>
            // If we derived partial.Result[$ctorParam] we are appending:
            //  ${ derivedToElement }.prependErrorPath(PathElement.Accessor("fromName"))
            DerivationResult.existential[TransformationExpr, CtorParam](expr.fold(TransformationExpr.fromTotal) {
              partialExpr =>
                TransformationExpr.fromPartial(prependWholeErrorPath(partialExpr, Path(_.select(fromName))))
            })
          } { errors =>
            appendMissingTransformer[From, To, Getter, CtorParam](errors, toName)
          }
        }
    }

    private def useFallbackValues[From, To, CtorParam: Type](toName: String)(
        defaultValue: => Option[Expr[CtorParam]]
    )(implicit ctx: TransformationContext[From, To]): Option[DerivationResult[Existential[TransformationExpr]]] = {
      lazy val fieldFlags = ctx.config.flags.atTgt(_.select(toName))

      def useFallbackValue: Option[DerivationResult[Existential[TransformationExpr]]] =
        findMatchingFallbackFieldAndUpdateCandidates(toName).collectFirst {
          case (fromName, fromFallbackField, updateCandidates) =>
            import fromFallbackField.{Underlying as FromFallbackField, value as fallbackExpr}
            DerivationResult.namedScope(
              s"Recursive derivation for fallback field `$fromName`: ${Type
                  .prettyPrint[FromFallbackField]} into matched `$toName`: ${Type.prettyPrint[CtorParam]}"
            ) {
              deriveRecursiveTransformationExpr[FromFallbackField, CtorParam](
                fallbackExpr,
                Path(_.select(fromName)),
                Path(_.select(toName)),
                updateCandidates
              ).flatMap(expr => DerivationResult.existential[TransformationExpr, CtorParam](expr))
            }
        }

      def useDefaultValue: Option[DerivationResult[Existential[TransformationExpr]]] =
        // Default values are provided from ProductType parsing.
        if (fieldFlags.isDefaultValueEnabledGloballyOrFor[CtorParam]) {
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
        OptionalValue.parse[CtorParam].filter(_ => fieldFlags.optionDefaultsToNone).map { optional =>
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

      useFallbackValue.orElse(useDefaultValue).orElse(useNone).orElse(useSingletonType)
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
          //     allerrors = io.scalaland.chimney.internal.runtime.ResultUtils.mergeNullable(allerrors, ${ res1 })
          //     allerrors = io.scalaland.chimney.internal.runtime.ResultUtils.mergeNullable(allerrors, ${ res2 })
          //     allerrors = io.scalaland.chimney.internal.runtime.ResultUtils.mergeNullable(allerrors, ${ res3 })
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
                  //   allerrors = io.scalaland.chimney.internal.runtime.ResultUtils.mergeNullable(allerrors, ${ res1 })
                  //   allerrors = io.scalaland.chimney.internal.runtime.ResultUtils.mergeNullable(allerrors, ${ res2 })
                  //   allerrors = io.scalaland.chimney.internal.runtime.ResultUtils.mergeNullable(allerrors, ${ res3 })
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
                          // '{ allerrors = io.scalaland.chimney.internal.runtime.ResultUtils.mergeNullable(allerrors, ${ resN }) }
                          setAllErrors(ChimneyExpr.PartialResult.Errors.mergeResultNullable(allerrors, expr))
                        },
                        // Here, we're building:
                        // '{ if (allerrors == null) $ifBlock else $elseBock }
                        Expr.ifElse[partial.Result[ToOrPartialTo]](allerrors eqExpr Expr.Null) {
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

    private def filterAllowedFieldsByFlags[A](
        fieldFlags: TransformerFlags
    ): Existential[Product.Getter[A, *]] => Boolean = getter => {
      val allowedSourceType = getter.value.sourceType match {
        case Product.Getter.SourceType.ConstructorArgVal  => true
        case Product.Getter.SourceType.ConstructorBodyVal => true
        case Product.Getter.SourceType.AccessorMethod     => fieldFlags.methodAccessors
        case Product.Getter.SourceType.JavaBeanGetter     => fieldFlags.beanGetters
      }
      val allowedInheritance = !getter.value.isInherited || fieldFlags.inheritedAccessors
      allowedSourceType && allowedInheritance
    }

    // Fallback utilities

    private trait FromOrFallbackGetter {
      type FromOrFallback
      implicit val FromOrFallback: Type[FromOrFallback]

      val src: Expr[FromOrFallback]
      val name: String
      val getter: Existential[Product.Getter[FromOrFallback, *]]

      override def toString: String =
        s"$name: ${Type.prettyPrint(using getter.Underlying)} = ${Expr.prettyPrint(getter.value.get(src))} (sourceType = ${getter.value.sourceType}, isInherited = ${getter.value.isInherited})"
    }
    private object FromOrFallbackGetter {
      def apply[FromOrFallback0: Type](
          src_ : Expr[FromOrFallback0],
          name_ : String,
          getter_ : Existential[Product.Getter[FromOrFallback0, *]]
      ): FromOrFallbackGetter = new FromOrFallbackGetter {
        type FromOrFallback = FromOrFallback0
        val FromOrFallback = Type[FromOrFallback0]

        val src: Expr[FromOrFallback0] = src_
        val name: String = name_
        val getter: Existential[Product.Getter[FromOrFallback0, *]] = getter_
      }
    }

    @scala.annotation.nowarn("msg=never used") // on Scala 2.13 fromName/exact/possible are marked as unused 0_0
    private def findMatchingFallbackFields[From, To](toName: String)(implicit
        ctx: TransformationContext[From, To]
    ) = ctx.config.filterCurrentOverridesForFallbacks.view.flatMap { case to @ TransformerOverride.Fallback(fallback) =>
      val fieldFlags = ctx.config.flags.atTgt(_.select(toName))
      import fallback.{Underlying as Fallback, value as fallbackExpr}
      for {
        Product.Extraction(getters) <- ProductType.parseExtraction[Fallback].view
        // make sure that exact name match (==) takes priority before other matches
        (exact, possible) = getters.view.partition(_._1 == toName)
        (fromName, getter) <- (exact ++ possible)
        if filterAllowedFieldsByFlags(fieldFlags)(getter)
        if areFieldNamesMatching(fromName, toName)
      } yield {
        import getter.{Underlying as FromFallback, value as fromField}
        (to, fromName, fromField.get(fallbackExpr).as_??)
      }
    } // keep lazy!!!

    private def findMatchingUpdateCandidates[From, To](toName: String)(implicit
        ctx: TransformationContext[From, To]
    ): Map[TransformerOverride.ForFallback, Vector[TransformerOverride.ForFallback]] = ListMap
      .from(
        findMatchingFallbackFields(toName)
          .groupBy[TransformerOverride.ForFallback](_._1)
          .view
          .mapValues(_.map(t => TransformerOverride.Fallback(t._3): TransformerOverride.ForFallback).toVector)
      )
      .withDefaultValue(Vector.empty)

    private def findMatchingFallbackFieldAndUpdateCandidates[From, To](toName: String)(implicit
        ctx: TransformationContext[From, To]
    ): Option[
      (String, ExistentialExpr, Map[TransformerOverride.ForFallback, Vector[TransformerOverride.ForFallback]])
    ] = {
      val fromFallbackCandidates = findMatchingFallbackFields(toName)
      fromFallbackCandidates.collectFirst { case (to, fromName, fromFallbackField) =>
        val updateCandidates = ListMap
          .from(
            fromFallbackCandidates.tail
              .groupBy[TransformerOverride.ForFallback](_._1)
              .view
              .filterKeys(to != _) // we don't need fallback for value that became the new main src
              .mapValues(_.map(t => TransformerOverride.Fallback(t._3): TransformerOverride.ForFallback).toVector)
          )
          .withDefaultValue(Vector.empty)
        (fromName, fromFallbackField, updateCandidates)
      }
    }

    // UnnamedFieldPolicy utilities

    private def checkPolicy[From, To](
        requiredFromNames: SortedSet[String],
        fromNamesUsedByExtractors: Set[String],
        fromNamesExplicitlyUnmatched: Set[String]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Unit] =
      ctx.config.flags.unusedFieldPolicy match {
        case None                         => DerivationResult.unit
        case Some(FailOnIgnoredSourceVal) =>
          val fromNamesUsedInOverrides = ctx.sourceFieldsUsedByOverrides
          val unusedFromNames =
            requiredFromNames -- fromNamesUsedByExtractors -- fromNamesUsedInOverrides -- fromNamesExplicitlyUnmatched
          if (unusedFromNames.isEmpty) {
            DerivationResult.unit
              .logSuccess(_ => s"Run UnusedFieldPolicy=$FailOnIgnoredSourceVal, all source vals used")
          } else
            DerivationResult
              .failedPolicyCheck(FailOnIgnoredSourceVal, ctx.currentSrc, unusedFromNames.toList)
              .logFailure(_ =>
                s"Run UnusedFieldPolicy=$FailOnIgnoredSourceVal, unused source vals: ${unusedFromNames.mkString(", ")}"
              )
      }

    // Error-related utilities

    @scala.annotation.tailrec
    private def prependWholeErrorPath[A: Type](expr: Expr[partial.Result[A]], path: Path): Expr[partial.Result[A]] =
      path match {
        // If we derived partial.Result[$ctorParam] we are appending:
        //  ${ derivedToElement }.prependErrorPath(PathElement.Accessor("fromName"))
        case Path.AtField(name, path2) =>
          prependWholeErrorPath(
            expr.prependErrorPath(
              ChimneyExpr.PathElement
                .Accessor(Expr.String(name))
                .upcastToExprOf[partial.PathElement]
            ),
            path2
          )
        // We are not appending anything on pattern-match, so we can just drop Path on it
        case Path.AtSubtype(_, path2) =>
          prependWholeErrorPath(expr, path2)
        // To append values in .everyItem/.everyMapKey/.everyMapValue we simply have to unsealPath in their results
        case _ => expr // Path.Root
      }

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

    // Constants compared by refs to handle some special cases without an explosion in complexity.

    // Stub to use when the setter's return type is not Unit and nonUnitBeanSetters flag is off.
    private val nonUnitSetter = Existential[TransformationExpr, Null](TransformationExpr.fromTotal(Expr.Null))

    // Stub to use when the setter's was not matched and beanSettersIgnoreUnmatched flag is on.
    private val unmatchedSetter = Existential[TransformationExpr, Null](TransformationExpr.fromTotal(Expr.Null))
  }
}
