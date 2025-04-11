package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.dsl.FailOnUnmatchedTargetSubtype
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.internal.compiletime.fp.Traverse
import io.scalaland.chimney.internal.compiletime.fp.Implicits.*
import io.scalaland.chimney.partial

private[compiletime] trait TransformSealedHierarchyToSealedHierarchyRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformSealedHierarchyToSealedHierarchyRule extends Rule("SealedHierarchyToSealedHierarchy") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (SealedHierarchy(Enum(fromElements)), SealedHierarchy(Enum(toElements))) =>
          mapEachSealedElementToAnotherSealedElement(fromElements, toElements)
        case (SealedHierarchy(_), _) =>
          DerivationResult.attemptNextRuleBecause(
            s"Type ${Type.prettyPrint[From]} is a sealed/enum type but ${Type.prettyPrint[To]} is not"
          )
        case (_, SealedHierarchy(_)) =>
          DerivationResult.attemptNextRuleBecause(
            s"Type ${Type.prettyPrint[To]} is a sealed/enum type but ${Type.prettyPrint[From]} is not"
          )
        case _ => DerivationResult.attemptNextRule
      }

    private def mapEachSealedElementToAnotherSealedElement[From, To](
        fromElements: Enum.Elements[From],
        toElements: Enum.Elements[To]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] = {
      val toSubtypesMatched = scala.collection.mutable.ListBuffer.empty[ExistentialType]
      val toSubtypesExplicitlyUnmatched = ctx.config.filterCurrentUnusedSubtypes

      DerivationResult.log {
        val fromSubs = fromElements.map(tpe => Type.prettyPrint(using tpe.Underlying)).mkString(", ")
        val toSubs = toElements.map(tpe => Type.prettyPrint(using tpe.Underlying)).mkString(", ")
        s"Resolved ${Type.prettyPrint[From]} subtypes: ($fromSubs) and ${Type.prettyPrint[To]} subtypes ($toSubs)"
      } >> mapOverriddenElements[From, To].flatMap { overrideMappings =>
        Traverse[List]
          .parTraverse[
            DerivationResult,
            Existential.UpperBounded[From, Enum.Element[From, *]],
            Existential[ExprPromise[*, TransformationExpr[To]]]
          ](fromElements.filterNot { fromSubtype =>
            // A single coproduct override might be a sealed which in nested sealed hierarchy which would remove
            // the need for several non-abstract subtypes - keeping them would result in unreachable code errors.
            overrideMappings.exists(usedFromSubtype => fromSubtype.Underlying <:< usedFromSubtype.Underlying)
          }) { (fromSubtype: Existential.UpperBounded[From, Enum.Element[From, *]]) =>
            mapElementsMatchedByName[From, To](
              fromSubtype,
              toElements,
              toSubtypesMatched,
              toSubtypesExplicitlyUnmatched
            )
              .orElse(mapWholeTo[From, To](fromSubtype))
          }
          .flatTap(_ => checkPolicy(toElements, toSubtypesMatched.toSet, toSubtypesExplicitlyUnmatched))
          .flatMap { (nameMatchedMappings: List[Existential[ExprPromise[*, TransformationExpr[To]]]]) =>
            combineElementsMappings[From, To](overrideMappings ++ nameMatchedMappings)
          }
      }
    }

    private def mapOverriddenElements[From, To](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[List[Existential[ExprPromise[*, TransformationExpr[To]]]]] = {
      val overrides = ctx.config
        .filterCurrentOverridesForSubtype { (someFrom: ??) =>
          import someFrom.Underlying as SomeFrom
          Type[SomeFrom] <:< Type[From]
        }
        .filter {
          case (_, TransformerOverride.Unused)                            => true
          case (_, TransformerOverride.Computed(_, targetPath, _))        => targetPath == ctx.currentTgt
          case (_, TransformerOverride.ComputedPartial(_, targetPath, _)) => targetPath == ctx.currentTgt
          case (_, TransformerOverride.Renamed(_, targetPath)) =>
            targetPath.drop(ctx.currentTgt) match {
              case Some(Path.AtSubtype(someTo, Path.Root)) => someTo.Underlying <:< Type[To]
              case _                                       => false
            }
        }
        .toList

      Traverse[List].parTraverse[
        DerivationResult,
        (ExistentialType, TransformerOverride.ForSubtype),
        (Existential[ExprPromise[*, TransformationExpr[To]]])
      ](overrides) { case (someFrom, runtimeSubtype) =>
        import someFrom.Underlying as SomeFrom
        DerivationResult.direct[TransformationExpr[To], Existential[ExprPromise[*, TransformationExpr[To]]]] { await =>
          val promise = ExprPromise
            .promise[SomeFrom](ExprPromise.NameGenerationStrategy.FromType)
            .map { (someFromExpr: Expr[SomeFrom]) =>
              // Ideally we would use here (someFrom => ...) types and pass down someFromExpr,
              // unfortunately on Scala 2 we end up with situations like:
              //   case javaEnum: JavaEnum.Value =>
              //      val _ = javaEnum
              //     runtimeDataStore(x).asInstanceOf[JavaEnum.Value => To](javaEnum)
              // complaining that javaEnum.type is not equal to expected JavaEnum.Value.type.
              lazy val fromExpr = someFromExpr.upcastToExprOf[From]

              // targetPath verified by filter in overrides
              runtimeSubtype match {
                case TransformerOverride.Unused =>
                  await(
                    DerivationResult
                      .assertionError("Unmatched subtype override should have been checked on target side Path")
                  )
                case TransformerOverride.Computed(_, _, runtimeData) =>
                  // We're constructing:
                  // case someFromExpr: $someFrom => runtimeDataStore(${ idx }).asInstanceOf[$someFrom => $To](someFromExpr)
                  TransformationExpr.fromTotal(
                    runtimeData.asInstanceOfExpr[From => To].apply(fromExpr)
                  )
                case TransformerOverride.ComputedPartial(_, _, runtimeData) =>
                  // We're constructing:
                  // case someFromExpr: $someFrom => runtimeDataStore(${ idx }).asInstanceOf[$someFrom => partial.Result[$To]](someFromExpr)
                  TransformationExpr.fromPartial(
                    runtimeData.asInstanceOfExpr[From => partial.Result[To]].apply(fromExpr)
                  )
                case TransformerOverride.Renamed(_, targetPath) =>
                  val Some(Path.AtSubtype(someTo, _)) = targetPath.drop(ctx.currentTgt): @unchecked
                  // We're constructing:
                  // case someFromExpr: $someFrom => $derivedToSubtype.asInstance
                  import someTo.Underlying as SomeTo
                  await(
                    deriveRecursiveTransformationExpr[SomeFrom, SomeTo](
                      someFromExpr,
                      Path(_.matching[SomeFrom]),
                      Path(_.matching[SomeTo])
                    ).map(
                      _.fold(totalExpr => TransformationExpr.fromTotal(totalExpr.asInstanceOfExpr[To])) { partialExpr =>
                        TransformationExpr.fromPartial(partialExpr.asInstanceOfExpr[partial.Result[To]])
                      }
                    )
                  )
              }
            }
          Existential[ExprPromise[*, TransformationExpr[To]], SomeFrom](promise)
        }
      }
    }

    private def mapElementsMatchedByName[From, To](
        fromSubtype: Existential.UpperBounded[From, Enum.Element[From, *]],
        toElements: Enum.Elements[To],
        toSubtypesMatched: scala.collection.mutable.ListBuffer[??],
        toSubtypesExplicitlyUnmatched: Set[??]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Existential[ExprPromise[*, TransformationExpr[To]]]] = {
      import fromSubtype.Underlying as FromSubtype, fromSubtype.value.name as fromName
      toElements.filter { toSubtype =>
        areSubtypeNamesMatching(fromName, toSubtype.value.name) &&
        !toSubtypesExplicitlyUnmatched.exists(um => toSubtype.Underlying <:< um.Underlying)
      } match {
        // 0 matches - no coproduct with the same name
        case Nil =>
          DerivationResult
            .missingSubtypeTransformer[From, To, FromSubtype, Existential[ExprPromise[*, TransformationExpr[To]]]]
        // 1 match - unambiguous finding
        case toSubtype :: Nil =>
          import toSubtype.Underlying as ToSubtype, toSubtype.value.upcast as toUpcast
          ExprPromise
            // Scala 2/3 compatibility: each Java enum value on Scala 3 would have distinct type,
            // while on Scala 2 they all have the same type, so NameGenerationStrategy.FromType behaves differently
            .promise[FromSubtype](
              ExprPromise.NameGenerationStrategy.FromPrefix(fromSubtype.value.name.toLowerCase)
            )
            .traverse { (fromSubtypeExpr: Expr[FromSubtype]) =>
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
                      DerivationResult.log(
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
                    DerivationResult.log(
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
            .map(Existential[ExprPromise[*, TransformationExpr[To]], FromSubtype](_))
        // 2 or more matches - ambiguous coproduct instances
        case toSubtypes =>
          DerivationResult.ambiguousSubtypeTargets[From, To, Existential[ExprPromise[*, TransformationExpr[To]]]](
            FromSubtype.as_??,
            toSubtypes.map(toSubtype => toSubtype.Underlying.as_??)
          )
      }
    }

    private def mapWholeTo[From, To](fromSubtype: Existential.UpperBounded[From, Enum.Element[From, *]])(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Existential[ExprPromise[*, TransformationExpr[To]]]] = {
      import fromSubtype.Underlying as FromSubtype
      ExprPromise
        // Scala 2/3 compatibility: each Java enum value on Scala 3 would have distinct type,
        // while on Scala 2 they all have the same type, so NameGenerationStrategy.FromType behaves differently
        .promise[FromSubtype](ExprPromise.NameGenerationStrategy.FromPrefix(fromSubtype.value.name.toLowerCase))
        .traverse { (fromSubtypeExpr: Expr[FromSubtype]) =>
          // We're constructing:
          // case fromSubtypeExpr: $fromSubtype => ${ derivedTo } // or ${ derivedResultTo }
          DerivationResult.log(
            s"Falling back on ${Type.prettyPrint[FromSubtype]} to ${Type.prettyPrint[To]} (target upcasted)"
          ) >>
            deriveRecursiveTransformationExpr[FromSubtype, To](
              fromSubtypeExpr,
              followFrom = Path(_.matching[FromSubtype]),
              updateRules = _.filter(_.name == "Implicit")
            )
        }
        .map(Existential[ExprPromise[*, TransformationExpr[To]], FromSubtype](_))
    }

    private def combineElementsMappings[From, To](
        subtypeMappings: List[Existential[ExprPromise[*, TransformationExpr[To]]]]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      if (subtypeMappings.exists(_.value.isPartial))
        // if any result is partial, all results must be lifted to partial
        DerivationResult.log(
          s"Found cases ${subtypeMappings.count(_.value.isPartial)} with Partial target, lifting all cases to Partial"
        ) >>
          DerivationResult
            .expandedPartial(
              subtypeMappings
                .map { subtype =>
                  subtype.value.ensurePartial.fulfillAsPatternMatchCase[partial.Result[To]]
                }
                .matchOn(ctx.src)
            )
      else
        // if all are total, we might treat them as such
        DerivationResult.expandedTotal(
          subtypeMappings
            .map { subtype =>
              subtype.value.ensureTotal.fulfillAsPatternMatchCase[To]
            }
            .matchOn(ctx.src)
        )

    private def checkPolicy[From, To](
        requiredToSubtypes: Enum.Elements[To],
        toSubtypesUsedInMatch: Set[ExistentialType],
        toSubtypesExplicitlyUnmatched: Set[ExistentialType]
    )(implicit ctx: TransformationContext[From, To]): DerivationResult[Unit] =
      ctx.config.flags.unmatchedSubtypePolicy match {
        case None => DerivationResult.unit
        case Some(FailOnUnmatchedTargetSubtype) =>
          val toSubtypesUsedInOverrides = ctx.targetSubtypesUsedByOverrides
          val unmatchedToSubtypes = requiredToSubtypes.view
            .filterNot(tpe => toSubtypesUsedInMatch.exists(tpe2 => tpe.Underlying =:= tpe2.Underlying))
            .filterNot(tpe => toSubtypesUsedInOverrides.exists(tpe2 => tpe.Underlying =:= tpe2.Underlying))
            .filterNot(tpe => toSubtypesExplicitlyUnmatched.exists(tpe2 => tpe.Underlying =:= tpe2.Underlying))
            .map(tpe => Type.prettyPrint(using tpe.Underlying))
            .toList
          if (unmatchedToSubtypes.isEmpty) {
            DerivationResult.unit
              .logSuccess(_ => s"Run UnmatchedSubtypePolicy=$FailOnUnmatchedTargetSubtype, all source vals used")
          } else
            DerivationResult
              .failedPolicyCheck(FailOnUnmatchedTargetSubtype, ctx.currentSrc, unmatchedToSubtypes)
              .logFailure(_ =>
                s"Run UnmatchedSubtypePolicy=$FailOnUnmatchedTargetSubtype, unused source vals: ${unmatchedToSubtypes.mkString(", ")}"
              )
      }
  }
}
