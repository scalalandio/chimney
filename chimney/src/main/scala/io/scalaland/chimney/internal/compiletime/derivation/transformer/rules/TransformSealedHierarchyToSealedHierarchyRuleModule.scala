package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.internal.compiletime.fp.Traverse
import io.scalaland.chimney.internal.compiletime.fp.Implicits.*
import io.scalaland.chimney.partial
import scala.collection.compat.* // for sizeIs on 2.12

private[compiletime] trait TransformSealedHierarchyToSealedHierarchyRuleModule { this: Derivation =>

  import Type.Implicits.*, ChimneyType.Implicits.*

  protected object TransformSealedHierarchyToSealedHierarchyRule extends Rule("SealedHierarchyToSealedHierarchy") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (SealedHierarchy(Enum(fromElements)), SealedHierarchy(Enum(toElements))) =>
          val verifyEnumNameUniqueness = {
            val checkFrom = fromElements.groupBy(_.value.name).toList.parTraverse { case (name, values) =>
              if (values.sizeIs == 1) DerivationResult.unit
              else DerivationResult.ambiguousCoproductInstance[From, To, Unit](name)
            }
            val checkTo = toElements.groupBy(_.value.name).toList.parTraverse { case (name, values) =>
              if (values.sizeIs == 1) DerivationResult.unit
              else DerivationResult.ambiguousCoproductInstance[From, To, Unit](name)
            }
            checkFrom.parTuple(checkTo).as(())
          }
          lazy val overrideMappings: List[Existential[ExprPromise[*, TransformationExpr[To]]]] =
            ctx.config.coproductOverrides.toList.collect {
              case ((someFrom, someTo), runtimeCoproductOverride)
                  if someFrom.Underlying <:< Type[From] && Type[To] =:= someTo.Underlying =>
                someFrom.mapK[ExprPromise[*, TransformationExpr[To]]] {
                  implicit SomeFrom: Type[someFrom.Underlying] => _ =>
                    ExprPromise
                      .promise[someFrom.Underlying](ExprPromise.NameGenerationStrategy.FromType)
                      .map { (someFromExpr: Expr[someFrom.Underlying]) =>
                        runtimeCoproductOverride match {
                          case RuntimeCoproductOverride.CoproductInstance(idx) =>
                            // We're constructing:
                            // case someFromExpr: $someFrom => runtimeDataStore(${ idx }).asInstanceOf[$someFrom => $To](someFromExpr)
                            TransformationExpr.fromTotal(
                              ctx
                                .runtimeDataStore(idx)
                                .asInstanceOfExpr[someFrom.Underlying => To]
                                .apply(someFromExpr)
                            )
                          case RuntimeCoproductOverride.CoproductInstancePartial(idx) =>
                            // We're constructing:
                            // case someFromExpr: $someFrom => runtimeDataStore(${ idx }).asInstanceOf[$someFrom => partial.Result[$To]](someFromExpr)
                            TransformationExpr.fromPartial(
                              ctx
                                .runtimeDataStore(idx)
                                .asInstanceOfExpr[someFrom.Underlying => partial.Result[To]]
                                .apply(someFromExpr)
                            )
                        }
                      }
                }
            }
          DerivationResult.log {
            val fromSubs = fromElements.map(tpe => Type.prettyPrint(tpe.Underlying)).mkString(", ")
            val toSubs = toElements.map(tpe => Type.prettyPrint(tpe.Underlying)).mkString(", ")
            s"Resolved ${Type.prettyPrint[From]} subtypes: ($fromSubs) and ${Type.prettyPrint[To]} subtypes ($toSubs)"
          } >> verifyEnumNameUniqueness >>
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
                Existential.use(fromSubtype) { implicit FromSubtype: Type[fromSubtype.Underlying] =>
                  { case Enum.Element(fromName, _) =>
                    toElements
                      .collectFirst {
                        case toSubtype if enumNamesMatch(fromName, toSubtype.value.name) =>
                          Existential.use(toSubtype) { implicit ToSubtype: Type[toSubtype.Underlying] =>
                            { case Enum.Element(_, toUpcast) =>
                              ExprPromise
                                .promise[fromSubtype.Underlying](ExprPromise.NameGenerationStrategy.FromType)
                                .traverse { (fromSubtypeExpr: Expr[fromSubtype.Underlying]) =>
                                  // We're constructing:
                                  // case fromSubtypeExpr: $fromSubtype => ${ derivedTo } // or ${ derivedResultTo }
                                  deriveRecursiveTransformationExpr[fromSubtype.Underlying, toSubtype.Underlying](
                                    fromSubtypeExpr
                                  ).map(_.map(toUpcast))
                                    .orElse(
                                      deriveRecursiveTransformationExpr[fromSubtype.Underlying, To](
                                        fromSubtypeExpr
                                      )
                                    )
                                }
                                .map(
                                  Existential[ExprPromise[*, TransformationExpr[To]], fromSubtype.Underlying](_)
                                )
                            }
                          }
                      }
                      .getOrElse {
                        DerivationResult
                          .cantFindCoproductInstanceTransformer[From, To, fromSubtype.Underlying, Existential[
                            ExprPromise[*, TransformationExpr[To]]
                          ]]
                      }
                  }
                }
              }
              .flatMap { (nameMatchedMappings: List[Existential[ExprPromise[*, TransformationExpr[To]]]]) =>
                val subtypeMappings = overrideMappings ++ nameMatchedMappings
                if (subtypeMappings.exists(_.value.isPartial))
                  // if any result is partial, all results must be lifted to partial
                  DerivationResult.log(
                    s"Found cases ${subtypeMappings.count(_.value.isPartial)} with Partial target, lifting all cases to Partial"
                  ) >>
                    DerivationResult
                      .expandedPartial(
                        subtypeMappings
                          .map { subtype =>
                            subtype.value.ensurePartial.fulfillAsPatternMatchCase[partial.Result[To]](isCaseObject =
                              subtype.Underlying.isCaseObject
                            )
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
        case _ => DerivationResult.attemptNextRule
      }

    private def enumNamesMatch(fromName: String, toName: String): Boolean = fromName == toName
  }
}
