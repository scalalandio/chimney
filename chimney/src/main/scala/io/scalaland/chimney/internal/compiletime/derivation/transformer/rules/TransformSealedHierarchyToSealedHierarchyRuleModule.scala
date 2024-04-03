package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

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
      lazy val overrideMappings = mapOverriddenElements[From, To]
      DerivationResult.log {
        val fromSubs = fromElements.map(tpe => Type.prettyPrint(tpe.Underlying)).mkString(", ")
        val toSubs = toElements.map(tpe => Type.prettyPrint(tpe.Underlying)).mkString(", ")
        s"Resolved ${Type.prettyPrint[From]} subtypes: ($fromSubs) and ${Type.prettyPrint[To]} subtypes ($toSubs)"
      } >>
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
            mapElementsMatchedByName[From, To](fromSubtype, toElements).orElse(mapWholeTo[From, To](fromSubtype))
          }
          .flatMap { (nameMatchedMappings: List[Existential[ExprPromise[*, TransformationExpr[To]]]]) =>
            combineElementsMappings[From, To](overrideMappings ++ nameMatchedMappings)
          }
    }

    private def mapOverriddenElements[From, To](implicit
        ctx: TransformationContext[From, To]
    ): List[Existential[ExprPromise[*, TransformationExpr[To]]]] = ctx.config
      .filterOverridesForCoproduct { someFrom =>
        import someFrom.Underlying as SomeFrom
        Type[SomeFrom] <:< Type[From]
      }
      .toList
      .collect { case (someFrom, runtimeCoproductOverride) =>
        import someFrom.Underlying as SomeFrom
        someFrom.mapK[ExprPromise[*, TransformationExpr[To]]] { _ => _ =>
          ExprPromise
            .promise[SomeFrom](ExprPromise.NameGenerationStrategy.FromType)
            .map { (someFromExpr: Expr[SomeFrom]) =>
              // Ideally we would use here (someFrom => ...) types and pass down someFromExpr,
              // unfortunately on Scala 2 we end up with situations like:
              //   case javaEnum: JavaEnum.Value =>
              //      val _ = javaEnum
              //     runtimeDataStore(x).asInstanceOf[JavaEnum.Value => To](javaEnum)
              // complaining that javaEnum.type is not equal to expected JavaEnum.Value.type.
              val fromExpr = someFromExpr.upcastExpr[From]

              runtimeCoproductOverride match {
                case RuntimeOverride.ComputedNested(idx) =>
                  // We're constructing:
                  // case someFromExpr: $someFrom => runtimeDataStore(${ idx }).asInstanceOf[$someFrom => $To](someFromExpr)
                  TransformationExpr.fromTotal(
                    ctx
                      .runtimeDataStore(idx)
                      .asInstanceOfExpr[From => To]
                      .apply(fromExpr)
                  )
                case RuntimeOverride.ComputedNestedPartial(idx) =>
                  // We're constructing:
                  // case someFromExpr: $someFrom => runtimeDataStore(${ idx }).asInstanceOf[$someFrom => partial.Result[$To]](someFromExpr)
                  TransformationExpr.fromPartial(
                    ctx
                      .runtimeDataStore(idx)
                      .asInstanceOfExpr[From => partial.Result[To]]
                      .apply(fromExpr)
                  )
              }
            }
        }
      }

    private def mapElementsMatchedByName[From, To](
        fromSubtype: Existential.UpperBounded[From, Enum.Element[From, *]],
        toElements: Enum.Elements[To]
    )(implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Existential[ExprPromise[*, TransformationExpr[To]]]] = {
      import fromSubtype.Underlying as FromSubtype, fromSubtype.value.name as fromName
      toElements.filter(toSubtype => areSubtypeNamesMatching(fromName, toSubtype.value.name)) match {
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
            .promise[fromSubtype.Underlying](
              ExprPromise.NameGenerationStrategy.FromPrefix(fromSubtype.value.name.toLowerCase)
            )
            .traverse { (fromSubtypeExpr: Expr[fromSubtype.Underlying]) =>
              // We're constructing:
              // case fromSubtypeExpr: $fromSubtype => ${ derivedToSubtype } } // or ${ derivedResultToSubtype
              lazy val fromSubtypeIntoToSubtype =
                deriveRecursiveTransformationExpr[fromSubtype.Underlying, toSubtype.Underlying](
                  fromSubtypeExpr
                ).map(_.map(toUpcast))
              // We're constructing:
              // case fromSubtypeExpr: $fromSubtype => ${ derivedToSubtype } } // or ${ derivedResultToSubtype }; using fromSubtypeExpr.value
              lazy val fromSubtypeUnwrappedIntoToSubtype =
                fromSubtype.Underlying match {
                  case WrapperClassType(fromSubtypeInner) =>
                    import fromSubtypeInner.{Underlying as FromSubtypeInner, value as wrapper}
                    Some(
                      DerivationResult.log(
                        s"Falling back on ${Type.prettyPrint[fromSubtypeInner.Underlying]} to ${Type
                            .prettyPrint[toSubtype.Underlying]} (source subtype unwrapped)"
                      ) >>
                        deriveRecursiveTransformationExpr[
                          fromSubtypeInner.Underlying,
                          toSubtype.Underlying
                        ](
                          wrapper.unwrap(fromSubtypeExpr)
                        ).map(_.map(toUpcast))
                    )
                  case _ => None
                }
              // We're constructing:
              // case fromSubtypeExpr: $fromSubtype => Subtype(${ derivedToSubtypeInner } // or ${ derivedResultToSubtypeInner }.map(Subtype)
              lazy val fromSubtypeIntoToSubtypeUnwrapped = toSubtype.Underlying match {
                case WrapperClassType(toSubtypeInner) =>
                  import toSubtypeInner.{Underlying as FromSubtypeInner, value as wrapper}
                  Some(
                    DerivationResult.log(
                      s"Falling back on ${Type.prettyPrint[fromSubtype.Underlying]} to ${Type.prettyPrint[toSubtypeInner.Underlying]} (target subtype unwrapped)"
                    ) >>
                      deriveRecursiveTransformationExpr[fromSubtype.Underlying, toSubtypeInner.Underlying](
                        fromSubtypeExpr
                      ).map(_.map(wrapper.wrap)).map(_.map(toUpcast))
                  )
                case _ => None
              }

              fromSubtypeIntoToSubtype
                .orElseOpt(fromSubtypeUnwrappedIntoToSubtype)
                .orElseOpt(fromSubtypeIntoToSubtypeUnwrapped)
            }
            .map(Existential[ExprPromise[*, TransformationExpr[To]], FromSubtype](_))
        // 2 or more matches - ambiguous coproduct instances
        case toSubtypes =>
          DerivationResult.ambiguousSubtypeTargets[From, To, Existential[ExprPromise[*, TransformationExpr[To]]]](
            ExistentialType(fromSubtype.Underlying),
            toSubtypes.map(to => ExistentialType(to.Underlying))
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
        .promise[fromSubtype.Underlying](
          ExprPromise.NameGenerationStrategy.FromPrefix(fromSubtype.value.name.toLowerCase)
        )
        .traverse { (fromSubtypeExpr: Expr[FromSubtype]) =>
          // We're constructing:
          // case fromSubtypeExpr: $fromSubtype => ${ derivedTo } // or ${ derivedResultTo }
          DerivationResult.log(
            s"Falling back on ${Type.prettyPrint[FromSubtype]} to ${Type.prettyPrint[To]} (target upcasted)"
          ) >>
            deriveRecursiveTransformationExprUpdatingRules[fromSubtype.Underlying, To](fromSubtypeExpr)(rules =>
              rules.filter(rule => rule.name == "Implicit")
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
                  subtype.value.ensurePartial
                    .fulfillAsPatternMatchCase[partial.Result[To]](isCaseObject = subtype.Underlying.isCaseObject)
                }
                .matchOn(ctx.src)
            )
      else
        // if all are total, we might treat them as such
        DerivationResult.expandedTotal(
          subtypeMappings
            .map { subtype =>
              subtype.value.ensureTotal
                .fulfillAsPatternMatchCase[To](isCaseObject = subtype.Underlying.isCaseObject)
            }
            .matchOn(ctx.src)
        )
  }
}
