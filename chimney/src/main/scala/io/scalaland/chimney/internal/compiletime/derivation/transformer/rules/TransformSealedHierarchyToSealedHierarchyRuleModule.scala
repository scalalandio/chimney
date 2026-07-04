package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.data.NonEmptyVector
import hearth.fp.effect.{Log, MIO}
import hearth.fp.instances.*
import hearth.fp.syntax.*
import io.scalaland.chimney.dsl.FailOnUnmatchedTargetSubtype
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformSealedHierarchyToSealedHierarchyRuleModule {
  this: Derivation & hearth.MacroCommons =>

  import ChimneyType.Implicits.*

  protected object TransformSealedHierarchyToSealedHierarchyRule extends Rule("SealedHierarchyToSealedHierarchy") {

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
      (Type[From], Type[To]) match {
        case (SealedHierarchy(SealedEnum(fromElements)), SealedHierarchy(SealedEnum(toElements))) =>
          mapEachSealedElementToAnotherSealedElement(fromElements, toElements)
        case (SealedHierarchy(_), _) =>
          attemptNextRuleBecause(
            s"Type ${Type.prettyPrint[From]} is a sealed/enum type but ${Type.prettyPrint[To]} is not"
          )
        case (_, SealedHierarchy(_)) =>
          attemptNextRuleBecause(
            s"Type ${Type.prettyPrint[To]} is a sealed/enum type but ${Type.prettyPrint[From]} is not"
          )
        case _ => attemptNextRule
      }

    private def mapEachSealedElementToAnotherSealedElement[From, To](
        fromElements: SealedEnum.Elements[From],
        toElements: SealedEnum.Elements[To]
    )(implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] = {
      val toSubtypesMatched = scala.collection.mutable.ListBuffer.empty[ExistentialType]
      val toSubtypesExplicitlyUnmatched = ctx.config.filterCurrentUnusedSubtypes

      Log.info {
        val fromSubs = fromElements.map(tpe => Type.prettyPrint(using tpe.Underlying)).mkString(", ")
        val toSubs = toElements.map(tpe => Type.prettyPrint(using tpe.Underlying)).mkString(", ")
        s"Resolved ${Type.prettyPrint[From]} subtypes: ($fromSubs) and ${Type.prettyPrint[To]} subtypes ($toSubs)"
      } >> mapOverriddenElements[From, To].flatMap { overrideMappings =>
        fromElements
          .filterNot { fromSubtype =>
            // A single coproduct override might be a sealed which in nested sealed hierarchy which would remove
            // the need for several non-abstract subtypes - keeping them would result in unreachable code errors.
            overrideMappings.exists { case (usedFromSubtype, _) =>
              fromSubtype.Underlying <:< usedFromSubtype.Underlying
            }
          }
          .parTraverse[MIO, MatchCase[TransformationExpr[To]]] {
            (fromSubtype: Existential.UpperBounded[From, SealedEnum.Element[From, *]]) =>
              mapElementsMatchedByName[From, To](
                fromSubtype,
                toElements,
                toSubtypesMatched,
                toSubtypesExplicitlyUnmatched
              )
                .orElse(mapWholeTo[From, To](fromSubtype))
          }
          .flatTap(_ => checkPolicy(toElements, toSubtypesMatched.toSet, toSubtypesExplicitlyUnmatched))
          .flatMap { (nameMatchedMappings: List[MatchCase[TransformationExpr[To]]]) =>
            combineElementsMappings[From, To](overrideMappings.map(_._2) ++ nameMatchedMappings)
          }
      }
    }

    private def mapOverriddenElements[From, To](implicit
        ctx: TransformationContext[From, To]
    ): MIO[List[(ExistentialType, MatchCase[TransformationExpr[To]])]] = {
      val overrides = ctx.config
        .filterCurrentOverridesForSubtype { (someFrom: ??) =>
          import someFrom.Underlying as SomeFrom
          Type[SomeFrom] <:< Type[From]
        }
        .filter {
          case (_, TransformerOverride.Unused)                               => true
          case (_, TransformerOverride.Computed(_, targetPath, _))           => targetPath == ctx.currentTgt
          case (_, TransformerOverride.ComputedPartial(_, targetPath, _, _)) => targetPath == ctx.currentTgt
          case (_, TransformerOverride.Renamed(_, targetPath))               =>
            targetPath.drop(ctx.currentTgt) match {
              case Some(Path.AtSubtype(someTo, Path.Root)) => someTo.Underlying <:< Type[To]
              case _                                       => false
            }
        }
        .toList

      implicit val FnFromTo: Type[From => To] = fn1TypeCompat[From, To]
      implicit val FnFromPartialTo: Type[From => partial.Result[To]] = fn1TypeCompat[From, partial.Result[To]]
      implicit val FnBoolPartialTo: Type[Boolean => partial.Result[To]] = fnFromBooleanTypeCompat[partial.Result[To]]
      implicit val FnFromBoolPartialTo: Type[From => Boolean => partial.Result[To]] =
        fn1TypeCompat[From, Boolean => partial.Result[To]]

      overrides.parTraverse[MIO, (ExistentialType, MatchCase[TransformationExpr[To]])] {
        case (someFrom, runtimeSubtype) =>
          import someFrom.Underlying as SomeFrom
          MatchCase
            .typeMatch[SomeFrom](FreshName.FromType)
            .traverse[MIO, TransformationExpr[To]] { (someFromExpr: Expr[SomeFrom]) =>
              // Ideally we would use here (someFrom => ...) types and pass down someFromExpr,
              // unfortunately on Scala 2 we end up with situations like:
              //   case javaEnum: JavaEnum.Value =>
              //      val _ = javaEnum
              //     runtimeDataStore(x).asInstanceOf[JavaEnum.Value => To](javaEnum)
              // complaining that javaEnum.type is not equal to expected JavaEnum.Value.type.
              lazy val fromExpr: Expr[From] = someFromExpr.upcast[From]

              // targetPath verified by filter in overrides
              runtimeSubtype match {
                case TransformerOverride.Unused =>
                  MIO.fail(
                    new AssertionError("Unmatched subtype override should have been checked on target side Path")
                  )
                case TransformerOverride.Computed(_, _, runtimeData) =>
                  // We're constructing:
                  // case someFromExpr: $someFrom => runtimeDataStore(${ idx }).asInstanceOf[$someFrom => $To](someFromExpr)
                  MIO.pure(
                    TransformationExpr.fromTotal(
                      applyFnCompat(runtimeData.asInstanceOfExpr[From => To], fromExpr)
                    )
                  )
                case TransformerOverride.ComputedPartial(_, _, runtimeData, failFastAware) =>
                  // We're constructing:
                  // case someFromExpr: $someFrom => runtimeDataStore(${ idx }).asInstanceOf[$someFrom => partial.Result[$To]](someFromExpr)
                  val partialResult = if (failFastAware) {
                    val failFastExpr = ctx match {
                      case TransformationContext.ForPartial(_, failFast) => failFast
                      case _                                             => Expr(false)
                    }
                    applyFailFastCompat(
                      applyFnCompat(runtimeData.asInstanceOfExpr[From => Boolean => partial.Result[To]], fromExpr),
                      failFastExpr
                    )
                  } else {
                    applyFnCompat(runtimeData.asInstanceOfExpr[From => partial.Result[To]], fromExpr)
                  }
                  MIO.pure(TransformationExpr.fromPartial(partialResult))
                case TransformerOverride.Renamed(_, targetPath) =>
                  val Some(Path.AtSubtype(someTo, _)) = targetPath.drop(ctx.currentTgt): @unchecked
                  // We're constructing:
                  // case someFromExpr: $someFrom => $derivedToSubtype.asInstance
                  import someTo.Underlying as SomeTo
                  deriveRecursiveTransformationExpr[SomeFrom, SomeTo](
                    someFromExpr,
                    Path(_.matching[SomeFrom]),
                    Path(_.matching[SomeTo])
                  ).map(
                    _.fold(totalExpr => TransformationExpr.fromTotal(totalExpr.asInstanceOfExpr[To])) { partialExpr =>
                      TransformationExpr.fromPartial(partialExpr.asInstanceOfExpr[partial.Result[To]])
                    }
                  )
              }
            }
            .map(matchCase => (someFrom, matchCase))
      }
    }

    private def mapElementsMatchedByName[From, To](
        fromSubtype: Existential.UpperBounded[From, SealedEnum.Element[From, *]],
        toElements: SealedEnum.Elements[To],
        toSubtypesMatched: scala.collection.mutable.ListBuffer[??],
        toSubtypesExplicitlyUnmatched: Set[??]
    )(implicit
        ctx: TransformationContext[From, To]
    ): MIO[MatchCase[TransformationExpr[To]]] = {
      import fromSubtype.Underlying as FromSubtype, fromSubtype.value.name as fromName
      toElements.filter { toSubtype =>
        areSubtypeNamesMatching(fromName, toSubtype.value.name) &&
        !toSubtypesExplicitlyUnmatched.exists(um => toSubtype.Underlying <:< um.Underlying)
      } match {
        // 0 matches - no coproduct with the same name
        case Nil =>
          missingSubtypeTransformer[From, To, FromSubtype, MatchCase[TransformationExpr[To]]]
        // 1 match - unambiguous finding
        case toSubtype :: Nil =>
          import toSubtype.Underlying as ToSubtype, toSubtype.value.upcast as toUpcast
          MatchCase
            // Scala 2/3 compatibility: each Java enum value on Scala 3 would have distinct type,
            // while on Scala 2 they all have the same type, so FreshName.FromType behaves differently
            .typeMatch[FromSubtype](FreshName.FromPrefix(fromSubtype.value.name.toLowerCase))
            .traverse[MIO, TransformationExpr[To]] { (fromSubtypeExpr: Expr[FromSubtype]) =>
              // We're constructing:
              // case fromSubtypeExpr: $fromSubtype => ${ derivedToSubtype } } // or ${ derivedResultToSubtype
              lazy val fromSubtypeIntoToSubtype =
                deriveRecursiveTransformationExpr[FromSubtype, ToSubtype](
                  fromSubtypeExpr,
                  Path(_.matching[FromSubtype]),
                  Path(_.matching[ToSubtype])
                ).map(_.map(toUpcast))
              // We're constructing:
              // case fromSubtypeExpr: $fromSubtype => ${ derivedToSubtype } } // or ${ derivedResultToSubtype }; using fromSubtypeExpr.value
              lazy val fromSubtypeUnwrappedIntoToSubtype =
                FromSubtype match {
                  case WrapperClassType(fromSubtypeInner) =>
                    import fromSubtypeInner.{Underlying as FromSubtypeInner, value as wrapper}
                    Some(
                      Log.info(
                        s"Falling back on ${Type.prettyPrint[FromSubtypeInner]} to ${Type.prettyPrint[ToSubtype]} (source subtype unwrapped)"
                      ) >>
                        deriveRecursiveTransformationExpr[FromSubtypeInner, ToSubtype](
                          wrapper.unwrap(fromSubtypeExpr),
                          Path(_.matching[FromSubtype].select(wrapper.fieldName)),
                          Path(_.matching[ToSubtype])
                        ).map(_.map(toUpcast))
                    )
                  case _ => None
                }
              // We're constructing:
              // case fromSubtypeExpr: $fromSubtype => Subtype(${ derivedToSubtypeInner } // or ${ derivedResultToSubtypeInner }.map(Subtype)
              lazy val fromSubtypeIntoToSubtypeUnwrapped = toSubtype.Underlying match {
                case WrapperClassType(toSubtypeInner) =>
                  import toSubtypeInner.{Underlying as ToSubtypeInner, value as wrapper}
                  Some(
                    Log.info(
                      s"Falling back on ${Type.prettyPrint[FromSubtype]} to ${Type.prettyPrint[ToSubtypeInner]} (target subtype unwrapped)"
                    ) >>
                      deriveRecursiveTransformationExpr[FromSubtype, ToSubtypeInner](
                        fromSubtypeExpr,
                        Path(_.matching[FromSubtype]),
                        Path(_.select(wrapper.fieldName))
                      ).map(_.map(wrapper.wrap)).map(_.map(toUpcast))
                  )
                case _ => None
              }

              toSubtypesMatched += toSubtype.Underlying.as_??
              fromSubtypeIntoToSubtype
                .orElseOpt(fromSubtypeUnwrappedIntoToSubtype)
                .orElseOpt(fromSubtypeIntoToSubtypeUnwrapped)
            }
        // 2 or more matches - ambiguous coproduct instances
        case toSubtypes =>
          ambiguousSubtypeTargets[From, To, MatchCase[TransformationExpr[To]]](
            FromSubtype.as_??,
            toSubtypes.map(toSubtype => toSubtype.Underlying.as_??)
          )
      }
    }

    private def mapWholeTo[From, To](
        fromSubtype: Existential.UpperBounded[From, SealedEnum.Element[From, *]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): MIO[MatchCase[TransformationExpr[To]]] = {
      import fromSubtype.Underlying as FromSubtype
      MatchCase
        // Scala 2/3 compatibility: each Java enum value on Scala 3 would have distinct type,
        // while on Scala 2 they all have the same type, so FreshName.FromType behaves differently
        .typeMatch[FromSubtype](FreshName.FromPrefix(fromSubtype.value.name.toLowerCase))
        .traverse[MIO, TransformationExpr[To]] { (fromSubtypeExpr: Expr[FromSubtype]) =>
          // We're constructing:
          // case fromSubtypeExpr: $fromSubtype => ${ derivedTo } // or ${ derivedResultTo }
          Log.info(
            s"Falling back on ${Type.prettyPrint[FromSubtype]} to ${Type.prettyPrint[To]} (target upcasted)"
          ) >>
            deriveRecursiveTransformationExpr[FromSubtype, To](
              fromSubtypeExpr,
              followFrom = Path(_.matching[FromSubtype]),
              updateRules = _.filter(_.name == "Implicit")
            )
        }
    }

    private def combineElementsMappings[From, To](
        subtypeMappings: List[MatchCase[TransformationExpr[To]]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): MIO[Rule.ExpansionResult[To]] =
      if (subtypeMappings.exists(_.isPartial))
        // if any result is partial, all results must be lifted to partial
        Log.info(
          s"Found cases ${subtypeMappings.count(_.isPartial)} with Partial target, lifting all cases to Partial"
        ) >>
          expandedPartial(
            ctx.src.matchOn[partial.Result[To]](toNonEmptyVector(subtypeMappings.map(_.ensurePartial)))
          )
      else
        // if all are total, we might treat them as such
        expandedTotal(
          ctx.src.matchOn[To](toNonEmptyVector(subtypeMappings.map(_.ensureTotal)))
        )

    private def toNonEmptyVector[A](cases: List[MatchCase[A]]): NonEmptyVector[MatchCase[A]] =
      NonEmptyVector.fromVector(cases.toVector).getOrElse {
        // $COVERAGE-OFF$should never happen unless we messed up
        assertionFailed("Expected at least one subtype pattern-match case")
        // $COVERAGE-ON$
      }

    private def checkPolicy[From, To](
        requiredToSubtypes: SealedEnum.Elements[To],
        toSubtypesUsedInMatch: Set[ExistentialType],
        toSubtypesExplicitlyUnmatched: Set[ExistentialType]
    )(implicit ctx: TransformationContext[From, To]): MIO[Unit] =
      ctx.config.flags.unmatchedSubtypePolicy match {
        case None                               => MIO.void
        case Some(FailOnUnmatchedTargetSubtype) =>
          val toSubtypesUsedInOverrides = ctx.targetSubtypesUsedByOverrides
          val unmatchedToSubtypes = requiredToSubtypes.view
            .filterNot(tpe => toSubtypesUsedInMatch.exists(tpe2 => tpe.Underlying =:= tpe2.Underlying))
            .filterNot(tpe => toSubtypesUsedInOverrides.exists(tpe2 => tpe.Underlying =:= tpe2.Underlying))
            .filterNot(tpe => toSubtypesExplicitlyUnmatched.exists(tpe2 => tpe.Underlying =:= tpe2.Underlying))
            .map(tpe => Type.prettyPrint(using tpe.Underlying))
            .toList
          if (unmatchedToSubtypes.isEmpty) {
            MIO.void.log.valueAsInfo(_ =>
              s"Run UnmatchedSubtypePolicy=$FailOnUnmatchedTargetSubtype, all source vals used"
            )
          } else
            failedPolicyCheck(FailOnUnmatchedTargetSubtype, ctx.currentSrc, unmatchedToSubtypes).log.errorsAsInfo(_ =>
              s"Run UnmatchedSubtypePolicy=$FailOnUnmatchedTargetSubtype, unused source vals: ${unmatchedToSubtypes.mkString(", ")}"
            )
      }
  }
}
