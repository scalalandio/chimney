package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.data.NonEmptyVector
import hearth.fp.effect.{Log, MErrors, MIO}
import hearth.fp.instances.*
import hearth.fp.syntax.*
import io.scalaland.chimney.dsl.FailOnIgnoredSourceVal
import io.scalaland.chimney.internal.compiletime.NotSupportedOperationFromPath.Operation as FromOperation
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.annotation.unused
import scala.collection.immutable.{ListMap, SortedSet}

/** NB: the mutable `fromNamesUsedByExtractors` buffer is filled during the (lazy) MIO run - the `flatTap(checkPolicy)`
  * read happens strictly after all writes.
  */
private[compiletime] trait TransformProductToProductRuleModule { this: Derivation & hearth.MacroCommons =>

  import ChimneyType.Implicits.*

  protected object TransformProductToProductRule extends Rule("ProductToProduct") {

    private type PartialExpr[A] = Expr[partial.Result[A]]

    // Not implicit, provided locally/explicitly where needed (the hearth#316 sibling-implicit-lazy-Type deadlock
    // this guarded against is fixed since 0.4.1 - kept explicit to avoid ambient-implicit ambiguity).
    private lazy val UnitType: Type[Unit] = Type.of[Unit]
    private lazy val NullType: Type[Null] = Type.of[Null]
    private lazy val NoneType: Type[None.type] = Type.of[None.type]
    // def, NOT a lazy val: a trait-level materialized Expr would be created under whichever splice touches it first
    // and leak into later splices (cross-quotes usage contract violation, ScopeException under -Xcheck-macros).
    private def nullExpr: Expr[Null] = Expr.NullExprCodec.toExpr(null)

    // Cross-quotes helpers in methods with regular type parameters (the cross-quotes helper-def pattern).

    private def fn1TypeCompat[A: Type, B: Type]: Type[A => B] = Type.of[A => B]

    private def fnFromBooleanTypeCompat[A: Type]: Type[Boolean => A] = Type.of[Boolean => A]

    private def applyFnCompat[A: Type, B: Type](fn: Expr[A => B], a: Expr[A]): Expr[B] = Expr.quote {
      Expr.splice(fn).apply(Expr.splice(a))
    }

    private def applyFailFastCompat[A: Type](fn: Expr[Boolean => A], failFast: Expr[Boolean]): Expr[A] = Expr.quote {
      Expr.splice(fn).apply(Expr.splice(failFast))
    }

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      // From is checked after To, because extraction always succeeds
      (Type[To], Type[From]) match {
        case (HasCustomConstructor(constructorOverride), Product.Extraction(fromExtractors)) =>
          mapOverridesAndExtractorsToConstructorArguments[From, To](fromExtractors, constructorOverride)
        case (Product.Constructor(parameters, constructor), Product.Extraction(fromExtractors)) =>
          mapOverridesAndExtractorsToConstructorArguments[From, To, To](fromExtractors, parameters, constructor)
        case _ =>
          attemptNextRuleBecause(
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
    )(implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] = {
      import Product.Constructor.exprAsInstanceOfMethod as mkCtor
      constructorOverride match {
        case TransformerOverride.Constructor(args, runtimeData) =>
          val Product.Constructor(parameters, constructor) = mkCtor[To](args)(runtimeData)
          mapOverridesAndExtractorsToConstructorArguments[From, To, To](fromExtractors, parameters, constructor)
        case TransformerOverride.ConstructorPartial(args, runtimeData, failFastAware) if !failFastAware =>
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
        case TransformerOverride.ConstructorPartial(args, runtimeData, _ /* failFastAware = true */ ) =>
          implicit val FnBoolPartialTo: Type[Boolean => partial.Result[To]] =
            fnFromBooleanTypeCompat[partial.Result[To]]
          val Product.Constructor(params, ctor) =
            mkCtor[Boolean => partial.Result[To]](args)(runtimeData)
          mapOverridesAndExtractorsToConstructorArguments[From, To, Boolean => partial.Result[To]](
            fromExtractors,
            params,
            ctor
          ).map {
            case Rule.ExpansionResult.Expanded(transformationExpr) =>
              val failFastExpr = ctx match {
                case TransformationContext.ForPartial(_, failFast) => failFast
                case _                                             => Expr(false)
              }
              // no idea why it doesn't figure that out on its own in Scala 2 (GADT skolem)
              val appliedExpr =
                transformationExpr.asInstanceOf[TransformationExpr[Boolean => partial.Result[To]]] match {
                  case TransformationExpr.TotalExpr(expr) =>
                    TransformationExpr.PartialExpr(applyFailFastCompat(expr, failFastExpr))
                  case TransformationExpr.PartialExpr(expr) =>
                    TransformationExpr.PartialExpr(
                      expr
                        .map(LambdaBuilder.of1[Boolean => partial.Result[To]]().buildWith {
                          (fn: Expr[Boolean => partial.Result[To]]) =>
                            applyFailFastCompat(fn, failFastExpr)
                        })
                        .flatten
                    )
                }
              Rule.ExpansionResult.Expanded(appliedExpr)
            case Rule.ExpansionResult.AttemptNextRule(reason) => Rule.ExpansionResult.AttemptNextRule(reason)
          }
      }
    }

    private def mapOverridesAndExtractorsToConstructorArguments[From, To, ToOrPartialTo: Type](
        fromExtractors: Product.Getters[From],
        parameters: Product.Parameters,
        constructor: Product.Arguments => Expr[ToOrPartialTo]
    )(implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[ToOrPartialTo]] = {
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

      val verifyNoOverrideUnused: MIO[List[Nothing]] =
        filterCurrentOverridesForField(usedToName =>
          !parameters.keys.exists(toName => areFieldNamesMatching(usedToName, toName))
        ).keys.toList
          .parTraverse[MIO, Nothing] { fromName =>
            val tpeStr = Type.prettyPrint[To]
            val params = parameters.keys.map(n => s"`$n`").mkString(", ")
            MIO.fail(
              new AssertionError(
                s"""|Assumed that parameter/setter $fromName is a part of $tpeStr, but wasn't found
                  |available methods: $params""".stripMargin
              )
            )
          }

      val fromNamesUsedByExtractors = scala.collection.mutable.ListBuffer.empty[String]
      val fromNamesExplicitlyUnused = ctx.config.filterCurrentUnusedFields

      Log.info {
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
        parameters
          .filter { case (toName, param) =>
            flags.atTgt(_.select(toName)).nonUnitBeanSetters || (param.value.targetType match {
              case Product.Parameter.TargetType.SetterParameter(returnedType) =>
                returnedType.Underlying =:= UnitType
              case Product.Parameter.TargetType.ConstructorParameter => true
            })
          }
          .toList
          .parTraverse[MIO, (String, Existential[TransformationExpr])] {
            case (toName: String, ctorParam: Existential[Product.Parameter]) =>
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
                          case TransformerOverride.ComputedPartial(_, _, _, _) =>
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
                    ambiguousFieldOverrides[From, To, Existential[TransformationExpr]](
                      overrideName,
                      foundOverrides,
                      flags.getFieldNameComparison.toString // name comparison is defined for nested fields, not the field itself
                    )
                  case (_, value) =>
                    useOverride[From, To, CtorParam](toName, value).flatMap(
                      existential[TransformationExpr, CtorParam](_)
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
                        useExtractor[FromOrFallback, To, CtorParam](
                          ctorParam.value.targetType,
                          fromName,
                          toName,
                          getter
                        )
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
                          ambiguousFieldSources[From, To, Existential[TransformationExpr]](
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
                .getOrElse[MIO[Existential[TransformationExpr]]] {
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
                    tupleArityMismatch(
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
                        missingConstructorArgument[From, To, CtorParam, Existential[TransformationExpr]](
                          toName,
                          availableMethodAccessors,
                          availableInheritedAccessors,
                          availableDefault = defaultValue.isDefined,
                          availableNone = OptionalValue.parse[CtorParam].isDefined
                        )
                      case Product.Parameter.TargetType.SetterParameter(_) if fieldFlags.beanSettersIgnoreUnmatched =>
                        MIO.pure(unmatchedSetter)
                      case Product.Parameter.TargetType.SetterParameter(returnedType)
                          if !fieldFlags.nonUnitBeanSetters && !(returnedType.Underlying =:= UnitType) =>
                        MIO.pure(nonUnitSetter)
                      case Product.Parameter.TargetType.SetterParameter(_) =>
                        missingJavaBeanSetterParam[From, To, CtorParam, Existential[TransformationExpr]](
                          toName,
                          availableMethodAccessors,
                          availableInheritedAccessors,
                          availableNone = OptionalValue.parse[CtorParam].isDefined
                        )
                    }
                  }
                }
                .log
                .valueAsInfo {
                  case `unmatchedSetter` => s"Setter `$toName` not resolved but ignoring setters is allowed"
                  case `nonUnitSetter`   =>
                    s"Setter `$toName` not resolved it has non-Unit return type and they are ignored"
                  case expr => s"Resolved `$toName` field value to ${expr.value.prettyPrint}"
                }
                .map(toName -> _)
          }
          .map(_.filterNot(_._2 == unmatchedSetter).filterNot(_._2 == nonUnitSetter))
          .log
          .valueAsInfo { args =>
            val totals = args.count(_._2.value.isTotal)
            val partials = args.count(_._2.value.isPartial)
            s"Resolved ${args.size} arguments, $totals as total and $partials as partial Expr"
          }
          .flatTap { (_: List[(String, Existential[TransformationExpr])]) =>
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
          .flatMap(expanded)
    }

    private def useOverride[From, To, CtorParam: Type](
        toName: String,
        runtimeFieldOverride: TransformerOverride.ForField
    )(implicit
        ctx: TransformationContext[From, To]
    ): MIO[TransformationExpr[CtorParam]] = runtimeFieldOverride match {
      case TransformerOverride.Unused =>
        MIO.fail(new AssertionError("Unused field override should have been checked on source side Path"))
      case TransformerOverride.Const(runtimeData) =>
        // We're constructing:
        // '{ ${ runtimeDataStore }(idx).asInstanceOf[$ctorParam] }
        MIO.pure(
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
        MIO.pure(
          TransformationExpr.fromPartial(
            runtimeData
              .asInstanceOfExpr[partial.Result[CtorParam]]
              .prependErrorPath(
                ChimneyExpr.PathElement
                  .Const(Expr(s"${ctx.currentTgt}.$toName"))
                  .upcast[partial.PathElement]
              )
          )
        )
      case TransformerOverride.Computed(sourcePath, _, runtimeData) =>
        extractSrcByPath(FromOperation.Computed, sourcePath, toName).map { extractedSrc =>
          import extractedSrc.Underlying as ExtractedSrc, extractedSrc.value as extractedSrcExpr
          implicit val FnSrcCtorParam: Type[ExtractedSrc => CtorParam] = fn1TypeCompat[ExtractedSrc, CtorParam]
          ctx match {
            case TransformationContext.ForTotal(_) =>
              // We're constructing:
              // '{ ${ runtimeDataStore }(idx).asInstanceOf[$ExtractedSrc => $CtorParam](${ extractedSrcExpr }) }
              TransformationExpr.fromTotal(
                applyFnCompat(runtimeData.asInstanceOfExpr[ExtractedSrc => CtorParam], extractedSrcExpr)
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
                  applyFnCompat(
                    ChimneyExpr.PartialResult
                      .fromFunction(runtimeData.asInstanceOfExpr[ExtractedSrc => CtorParam]),
                    extractedSrcExpr
                  ),
                  sourcePath
                )
                  .prependErrorPath(
                    ChimneyExpr.PathElement
                      .Computed(Expr(s"${ctx.currentTgt}.$toName"))
                      .upcast[partial.PathElement]
                  )
              )
          }
        }
      case TransformerOverride.ComputedPartial(sourcePath, _, runtimeData, failFastAware) =>
        extractSrcByPath(FromOperation.ComputedPartial, sourcePath, toName).map { extractedSrc =>
          import extractedSrc.Underlying as ExtractedSrc, extractedSrc.value as extractedSrcExpr
          implicit val FnBoolPartialCtorParam: Type[Boolean => partial.Result[CtorParam]] =
            fnFromBooleanTypeCompat[partial.Result[CtorParam]]
          implicit val FnSrcPartialCtorParam: Type[ExtractedSrc => partial.Result[CtorParam]] =
            fn1TypeCompat[ExtractedSrc, partial.Result[CtorParam]]
          implicit val FnSrcBoolPartialCtorParam: Type[ExtractedSrc => Boolean => partial.Result[CtorParam]] =
            fn1TypeCompat[ExtractedSrc, Boolean => partial.Result[CtorParam]]
          // We're constructing:
          // '{
          //   ${ runtimeDataStore }(idx)
          //     .asInstanceOf[$ExtractedSrc => partial.Result[$CtorParam]](${ extractedSrcExpr })
          //     // prepend sourcePath
          //     .prependErrorPath(PathElement.Computed("_.toName"))
          // }
          // (or the failFastAware variant with an extra Boolean => ... curried call)
          val partialResult = if (failFastAware) {
            val failFastExpr = ctx match {
              case TransformationContext.ForPartial(_, failFast) => failFast
              case _                                             => Expr(false)
            }
            applyFailFastCompat(
              applyFnCompat(
                runtimeData.asInstanceOfExpr[ExtractedSrc => Boolean => partial.Result[CtorParam]],
                extractedSrcExpr
              ),
              failFastExpr
            )
          } else {
            applyFnCompat(
              runtimeData.asInstanceOfExpr[ExtractedSrc => partial.Result[CtorParam]],
              extractedSrcExpr
            )
          }
          TransformationExpr.fromPartial(
            prependWholeErrorPath(partialResult, sourcePath)
              .prependErrorPath(
                ChimneyExpr.PathElement
                  .Computed(Expr(s"${ctx.currentTgt}.$toName"))
                  .upcast[partial.PathElement]
              )
          )
        }
      case TransformerOverride.Renamed(sourcePath, _) =>
        extractSrcByPath(FromOperation.Renamed, sourcePath, toName).flatMap { extractedSrc =>
          import extractedSrc.Underlying as ExtractedSrc, extractedSrc.value as extractedSrcExpr
          Log.namedScope(
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
              .redeemWith { expr =>
                // If we derived partial.Result[$ctorParam] we are appending:
                //  ${ derivedToElement }.prependErrorPath(...).prependErrorPath(...) // sourcePath
                MIO.pure(expr.fold(TransformationExpr.fromTotal) { partialExpr =>
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
    ): MIO[ExistentialExpr] = {
      def extractSource[Source: Type](
          sourceName: String,
          extractedSrcExpr: Expr[Source]
      ): MIO[ExistentialExpr] = Type[Source] match {
        case Product.Extraction(getters) =>
          getters.filter { case (fromName, _) => areFieldNamesMatching(fromName, sourceName) }.toList match {
            case Nil =>
              MIO.fail(
                new AssertionError(
                  s"""|Assumed that field $sourceName is a part of ${Type.prettyPrint[Source]}, but wasn't found
                    |available methods: ${getters.keys.map(n => s"`$n`").mkString(", ")}""".stripMargin
                )
              )
            case (_, getter) :: Nil =>
              import getter.Underlying as Getter, getter.value.get
              MIO.pure(get(extractedSrcExpr).as_??)
            case matchingGetters =>
              ambiguousFieldOverrides[From, To, ExistentialExpr](
                sourceName,
                matchingGetters.map(_._1).sorted,
                ctx.config.flags.getFieldNameComparison.toString // name comparison is defined for nested fields, not the field itself
              )
          }
        case _ =>
          MIO.fail(
            new AssertionError(
              s"""Assumed that field $sourceName is a part of ${Type.prettyPrint[Source]}, but wasn't found"""
            )
          )
      }

      def extractNestedSource(path: Path, extractedSrcValue: ExistentialExpr): MIO[ExistentialExpr] =
        path match {
          case Path.Root =>
            MIO.pure(extractedSrcValue)
          case Path.AtField(sourceName, path2) =>
            import extractedSrcValue.Underlying as ExtractedSourceValue, extractedSrcValue.value as extractedSrcExpr
            extractSource[ExtractedSourceValue](sourceName, extractedSrcExpr).flatMap { extractedSrcValue2 =>
              extractNestedSource(path2, extractedSrcValue2)
            }
          case path =>
            notSupportedOperationFromPath[From, To, ExistentialExpr](
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
        a.recoverWith(errors => b.recoverWith(_ => MIO.fail(errors)))
      }
    }

    // Exposes logic for: OptionToOption, EitherToEither, IterableToIterable, MapToMap...
    def useOverrideIfPresentOr[From, To, CtorParam: Type](
        toName: String,
        runtimeFieldOverrides: Set[TransformerOverride.ForField]
    )(whenAbsent: => MIO[TransformationExpr[CtorParam]])(implicit
        ctx: TransformationContext[From, To]
    ): MIO[TransformationExpr[CtorParam]] = runtimeFieldOverrides.toList match {
      case Nil =>
        whenAbsent
      case runtimeFieldOverride :: Nil =>
        import io.scalaland.chimney.internal.compiletime.DerivationError.TransformerError as TError
        import io.scalaland.chimney.internal.compiletime.NotSupportedOperationFromPath as NotSupportedFrom
        useOverride[From, To, CtorParam](toName, runtimeFieldOverride).recoverWith {
          case NonEmptyVector(TError(NotSupportedFrom(_, `toName`, _, _)), Vector()) =>
            // If we cannot extract value in .withFieldComputedFrom/.withFieldComputedPartialFrom, it might be because
            // path is matching on TargetSide, but SourceSide requires recursion, TransformationContext update,
            // and then matching on some other rule.
            whenAbsent
          case errors => MIO.fail(errors)
        }
      // $COVERAGE-OFF$Config parsing dedupliate values
      case runtimeFieldOverrides =>
        MIO.fail(new AssertionError(s"Unexpected multiple overrides: ${runtimeFieldOverrides.mkString(", ")}"))
      // $COVERAGE-ON$
    }

    private def useExtractor[From, To, CtorParam: Type](
        ctorTargetType: Product.Parameter.TargetType,
        fromName: String,
        toName: String,
        getter: Existential[Product.Getter[From, *]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): MIO[Existential[TransformationExpr]] = ctorTargetType match {
      case Product.Parameter.TargetType.SetterParameter(_) if !ctx.config.flags.atTgt(_.select(toName)).beanSetters =>
        notSupportedTransformerDerivation(ctx)
          .logInfo(s"Matched $fromName to $toName but $toName is setter and they are disabled")
      case _ =>
        import getter.Underlying as Getter, getter.value.get
        Log.namedScope(
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
          ).redeemWith { expr =>
            // If we derived partial.Result[$ctorParam] we are appending:
            //  ${ derivedToElement }.prependErrorPath(PathElement.Accessor("fromName"))
            existential[TransformationExpr, CtorParam](expr.fold(TransformationExpr.fromTotal) { partialExpr =>
              TransformationExpr.fromPartial(prependWholeErrorPath(partialExpr, Path(_.select(fromName))))
            })
          } { errors =>
            appendMissingTransformer[From, To, Getter, CtorParam](errors, toName)
          }
        }
    }

    private def useFallbackValues[From, To, CtorParam: Type](toName: String)(
        defaultValue: => Option[Expr[CtorParam]]
    )(implicit ctx: TransformationContext[From, To]): Option[MIO[Existential[TransformationExpr]]] = {
      lazy val fieldFlags = ctx.config.flags.atTgt(_.select(toName))

      def useFallbackValue: Option[MIO[Existential[TransformationExpr]]] =
        findMatchingFallbackFieldAndUpdateCandidates(toName).collectFirst {
          case (fromName, fromFallbackField, updateCandidates) =>
            import fromFallbackField.{Underlying as FromFallbackField, value as fallbackExpr}
            Log.namedScope(
              s"Recursive derivation for fallback field `$fromName`: ${Type
                  .prettyPrint[FromFallbackField]} into matched `$toName`: ${Type.prettyPrint[CtorParam]}"
            ) {
              deriveRecursiveTransformationExpr[FromFallbackField, CtorParam](
                fallbackExpr,
                Path(_.select(fromName)),
                Path(_.select(toName)),
                updateCandidates
              ).flatMap(expr => existential[TransformationExpr, CtorParam](expr))
            }
        }

      def useDefaultValue: Option[MIO[Existential[TransformationExpr]]] =
        // Default values are provided from ProductType parsing.
        if (fieldFlags.isDefaultValueEnabledGloballyOrFor[CtorParam]) {
          defaultValue.map { (value: Expr[CtorParam]) =>
            // We're constructing:
            // '{ ${ defaultValue } }
            existential[TransformationExpr, CtorParam](
              TransformationExpr.fromTotal(value)
            )
          }
        } else None

      def useNone: Option[MIO[Existential[TransformationExpr]]] =
        // OptionalValue handles both scala.Options as well as a support provided through integrations.OptionalValue.
        OptionalValue.parse[CtorParam].filter(_ => fieldFlags.optionDefaultsToNone).map { optional =>
          // We're constructing:
          // '{ None }
          existential[TransformationExpr, CtorParam](
            TransformationExpr.fromTotal(optional.value.empty)
          )
        }

      def useSingletonType: Option[MIO[Existential[TransformationExpr]]] =
        // Singletons are always supported as a fallback (except None as this is explicitly handled with a flag).
        SingletonType.parse[CtorParam].filterNot(_ => Type[CtorParam] =:= NoneType).map { singleton =>
          // We're constructing:
          // '{ singleton } // e.g. (), null, case object, val, Java enum
          existential[TransformationExpr, CtorParam](
            TransformationExpr.fromTotal(singleton.value.upcast[CtorParam])
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
            resultExpr.map(LambdaBuilder.of1[Res]().buildWith { (innerExpr: Expr[Res]) =>
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
                  LambdaBuilder.of2[Res1, Res2]().buildWith { case (inner1Expr, inner2Expr) =>
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
              .traverse[ValDefs, (String, Existential[PartialExpr])] {
                case (name: String, expr: Existential[PartialExpr]) =>
                  // We start by building this initial block of '{ lazy val resN = ${ derivedResultTo } }
                  import expr.{Underlying as Res, value as partialResultExpr}
                  ValDefs
                    .createLazy(partialResultExpr, FreshName.FromPrefix("res"))
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
                      resultToMap.map(LambdaBuilder.of1[Res]().buildWith { (innerExpr: Expr[Res]) =>
                        constructor(constructorArguments + (name -> innerExpr.as_??))
                      })
                    // use .flatMap
                    case (name, res) :: tail =>
                      import res.{Underlying as Res, value as resultToFlatMap}
                      resultToFlatMap.flatMap(
                        LambdaBuilder.of1[Res]().buildWith { (innerExpr: Expr[Res]) =>
                          nestFlatMaps(tail, constructorArguments + (name -> innerExpr.as_??))
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
                  ValDefs
                    .createVar[partial.Result.Errors](
                      nullExpr.asInstanceOfExpr[partial.Result.Errors](using NullType, implicitly),
                      FreshName.FromPrefix("allerrors")
                    )
                    .use { case (allerrors, setAllErrors) =>
                      val mergeErrorsStatements = partialsAsLazy.map { case (_, result) =>
                        import result.{Underlying, value as expr}
                        // Here, we're building:
                        // '{ allerrors = io.scalaland.chimney.internal.runtime.ResultUtils.mergeNullable(allerrors, ${ resN }) }
                        setAllErrors(ChimneyExpr.PartialResult.Errors.mergeResultNullable(allerrors, expr))
                      }
                      // Here, we're building:
                      // '{ partial.Result.Value(${ constructor }) } // using res1.asInstanceOf[partial.Result.Value[Tpe]].value, ...
                      // (hoisted before the quote - Scala 2 cross-quotes cannot reify splices referencing cake members)
                      val constructedExpr: Expr[partial.Result[ToOrPartialTo]] = ChimneyExpr.PartialResult
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
                        .upcast[partial.Result[ToOrPartialTo]]
                      val allErrorsExpr: Expr[partial.Result[ToOrPartialTo]] =
                        allerrors.upcast[partial.Result[ToOrPartialTo]]
                      // Here, we're building:
                      // '{ if (allerrors == null) $ifBlock else $elseBock }
                      val checkErrorsExpr: Expr[partial.Result[ToOrPartialTo]] = Expr.quote {
                        if (Expr.splice(allerrors) == null) Expr.splice(constructedExpr)
                        else Expr.splice(allErrorsExpr)
                      }
                      mergeErrorsStatements.foldRight(checkErrorsExpr) { (statement, acc) =>
                        Expr.quote {
                          Expr.splice(statement)
                          Expr.splice(acc)
                        }
                      }
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
                    Expr.quote {
                      if (Expr.splice(failFast)) Expr.splice(failFastBranch) else Expr.splice(fullErrorBranch)
                    }
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
    ): MIO[Unit] =
      ctx.config.flags.unusedFieldPolicy match {
        case None                         => MIO.void
        case Some(FailOnIgnoredSourceVal) =>
          val fromNamesUsedInOverrides = ctx.sourceFieldsUsedByOverrides
          val unusedFromNames =
            requiredFromNames -- fromNamesUsedByExtractors -- fromNamesUsedInOverrides -- fromNamesExplicitlyUnmatched
          if (unusedFromNames.isEmpty) {
            MIO.void.log.valueAsInfo(_ => s"Run UnusedFieldPolicy=$FailOnIgnoredSourceVal, all source vals used")
          } else
            failedPolicyCheck(FailOnIgnoredSourceVal, ctx.currentSrc, unusedFromNames.toList).log.errorsAsInfo(_ =>
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
                .Accessor(Expr(name))
                .upcast[partial.PathElement]
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
        errors: MErrors,
        toName: String
    )(implicit ctx: TransformationContext[From, To]): MIO[Nothing] = {
      val newError = missingFieldTransformer[
        From,
        To,
        SourceField,
        TargetField,
        TransformationExpr[TargetField]
      ](toName)
      val oldErrors = MIO.fail(errors)
      newError.parTuple(oldErrors).map[Nothing](_ => ???)
    }

    // Constants compared by refs to handle some special cases without an explosion in complexity.

    // Stub to use when the setter's return type is not Unit and nonUnitBeanSetters flag is off.
    private lazy val nonUnitSetter = {
      implicit val NullT: Type[Null] = NullType
      Existential[TransformationExpr, Null](TransformationExpr.fromTotal(nullExpr))
    }

    // Stub to use when the setter's was not matched and beanSettersIgnoreUnmatched flag is on.
    private lazy val unmatchedSetter = {
      implicit val NullT: Type[Null] = NullType
      Existential[TransformationExpr, Null](TransformationExpr.fromTotal(nullExpr))
    }
  }
}
