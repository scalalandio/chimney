package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.data.NonEmptyVector
import hearth.fp.effect.{Log, MIO}
import hearth.fp.instances.*
import hearth.fp.syntax.*
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformToOptionRuleModule {
  this: Derivation & TransformOptionToOptionRuleModule & hearth.MacroCommons =>

  import ChimneyType.Implicits.*

  // Cross-quotes helpers in methods with regular type parameters (the cross-quotes helper-def pattern).

  private def optionTypeCompat[A: Type]: Type[Option[A]] = Type.of[Option[A]]

  private def optionExprCompat[A: Type](value: Expr[A]): Expr[Option[A]] = Expr.quote {
    scala.Option[A](Expr.splice(value))
  }

  private def wrapFallbackOptionCompat[A: Type](value: Expr[A]): ExistentialExpr = {
    implicit val OptionAType: Type[Option[A]] = optionTypeCompat[A]
    optionExprCompat(value).as_??
  }

  private def fn1TypeCompat[A: Type, B: Type]: Type[A => B] = Type.of[A => B]

  private def applyFnCompat[A: Type, B: Type](fn: Expr[A => B], a: Expr[A]): Expr[B] = Expr.quote {
    Expr.splice(fn).apply(Expr.splice(a))
  }

  private def fnFromBooleanTypeCompat[A: Type]: Type[Boolean => A] = Type.of[Boolean => A]

  private def applyFailFastCompat[A: Type](fn: Expr[Boolean => A], failFast: Expr[Boolean]): Expr[A] = Expr.quote {
    Expr.splice(fn).apply(Expr.splice(failFast))
  }

  protected object TransformToOptionRule extends Rule("ToOption") {

    private lazy val NoneType: Type[None.type] = Type.of[None.type]

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      Type[To] match {
        case _ if Type[To] <:< NoneType =>
          notSupportedTransformerDerivation(ctx)
            .logInfo(s"Discovered that target type is ${Type.prettyPrint(using NoneType)} which we explicitly reject")
        case OptionalValue(_) =>
          Log.namedScope("Lifting transformation into Option") {
            // $COVERAGE-OFF$scope detail is only built when Info logging is rendered (off by default, incl. in tests)
            Log.info(
              s"Lifting ${Type.prettyPrint[From]} -> ${Type
                  .prettyPrint[To]} transformation into ${Type.prettyPrint(using optionTypeCompat[From])} -> ${Type.prettyPrint[To]}"
            ) >>
              // $COVERAGE-ON$
              wrapInOptionAndTransform[From, To]
          }
        case _ =>
          attemptNextRule
      }
  }

  private def wrapInOptionAndTransform[From, To](implicit
      ctx: TransformationContext[From, To]
  ): MIO[Rule.ExpansionResult[To]] = {
    implicit val OptionFromType: Type[Option[From]] = optionTypeCompat[From]

    // Source-side subtype overrides (from withEnumCaseHandled / withSealedSubtypeHandled) return the
    // full target type (e.g. Option[Bar]).  The standard path wraps From into Option[From] and
    // delegates to OptionToOption, which recurses into From → InnerTo — but the override's path
    // gets dropped during that recursion (its SourcePath doesn't start with matching[Some[From]].value)
    // and its result type (Option[Bar]) doesn't match the inner target (Bar).
    // Fix: when such overrides exist and From is a sealed hierarchy, generate the match directly
    // at the From → Option[Bar] level, consuming the overrides here.
    val subtypeOverrides = ctx.config
      .filterCurrentOverridesForSubtype { (someFrom: ??) =>
        import someFrom.Underlying as SomeFrom
        Type[SomeFrom] <:< Type[From]
      }
      .filter {
        case (_, TransformerOverride.Computed(_, targetPath, _))           => targetPath == ctx.currentTgt
        case (_, TransformerOverride.ComputedPartial(_, targetPath, _, _)) => targetPath == ctx.currentTgt
        case _                                                             => false
      }

    if (subtypeOverrides.nonEmpty) {
      Type[From] match {
        case SealedHierarchy(SealedEnum(fromElements)) =>
          // $COVERAGE-OFF$
          Log.info(
            s"Found ${subtypeOverrides.size} subtype override(s) for sealed ${Type.prettyPrint[From]}; generating match before Option wrapping"
          ) >>
            // $COVERAGE-ON$
            mapSealedSubtypesToOption[From, To](fromElements, subtypeOverrides)
        case _ =>
          // From is not a sealed hierarchy — fall back to default wrapping (overrides will be lost,
          // but withEnumCaseHandled on a non-sealed type is not meaningful)
          wrapAndDelegateToOptionToOption[From, To]
      }
    } else {
      wrapAndDelegateToOptionToOption[From, To]
    }
  }

  private def wrapAndDelegateToOptionToOption[From, To](implicit
      ctx: TransformationContext[From, To]
  ): MIO[Rule.ExpansionResult[To]] = {
    implicit val OptionFromType: Type[Option[From]] = optionTypeCompat[From]
    // We're constructing:
    // '{ ${ derivedTo2 } /* created from Option(src) */  }
    TransformOptionToOptionRule.expand(
      ctx.updateFromTo[Option[From], To](optionExprCompat(ctx.src), updateFallbacks = wrapFallbacks)
    )
  }

  private def mapSealedSubtypesToOption[From, To](
      fromElements: SealedEnum.Elements[From],
      subtypeOverrides: Map[??, TransformerOverride.ForSubtype]
  )(implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
    Type[To] match {
      case OptionalValue(to2) =>
        import to2.{Underlying as InnerTo, value as optionalTo}
        implicit val SomeInnerToType: Type[Some[InnerTo]] = Type.of[Some[InnerTo]]
        implicit val FnFromTo: Type[From => To] = fn1TypeCompat[From, To]
        implicit val FnFromPartialTo: Type[From => partial.Result[To]] = fn1TypeCompat[From, partial.Result[To]]
        implicit val FnBoolPartialTo: Type[Boolean => partial.Result[To]] = fnFromBooleanTypeCompat[partial.Result[To]]
        implicit val FnFromBoolPartialTo: Type[From => Boolean => partial.Result[To]] =
          fn1TypeCompat[From, Boolean => partial.Result[To]]

        // For non-overridden subtypes, match by name against InnerTo's sealed subtypes
        val maybeToElements: Option[SealedEnum.Elements[InnerTo]] = Type[InnerTo] match {
          case SealedHierarchy(SealedEnum(elems)) => Some(elems)
          case _                                  => None
        }

        val elements: List[Existential.UpperBounded[From, SealedEnum.Element[From, *]]] = fromElements
        elements
          .parTraverse[MIO, MatchCase[TransformationExpr[To]]] {
            (fromSubtype: Existential.UpperBounded[From, SealedEnum.Element[From, *]]) =>
              import fromSubtype.Underlying as FromSubtype

              subtypeOverrides.find { case (tpe, _) =>
                fromSubtype.Underlying <:< tpe.Underlying || tpe.Underlying <:< fromSubtype.Underlying
              } match {
                case Some((_, runtimeOverride)) =>
                  // Overridden subtype: apply the override function (returns To = Option[Bar])
                  MatchCase
                    .typeMatch[FromSubtype](FreshName.FromType)
                    .traverse[MIO, TransformationExpr[To]] { (fromExpr: Expr[FromSubtype]) =>
                      lazy val fromUpcast: Expr[From] = fromExpr.upcast[From]
                      runtimeOverride match {
                        case TransformerOverride.Computed(_, _, runtimeData) =>
                          MIO.pure(
                            TransformationExpr.fromTotal(
                              applyFnCompat(runtimeData.asInstanceOfExpr[From => To], fromUpcast)
                            )
                          )
                        case TransformerOverride.ComputedPartial(_, _, runtimeData, failFastAware) =>
                          val partialResult = if (failFastAware) {
                            val failFastExpr = ctx match {
                              case TransformationContext.ForPartial(_, failFast) => failFast
                              case _                                             => Expr(false)
                            }
                            applyFailFastCompat(
                              applyFnCompat(
                                runtimeData.asInstanceOfExpr[From => Boolean => partial.Result[To]],
                                fromUpcast
                              ),
                              failFastExpr
                            )
                          } else {
                            applyFnCompat(runtimeData.asInstanceOfExpr[From => partial.Result[To]], fromUpcast)
                          }
                          MIO.pure(TransformationExpr.fromPartial(partialResult))
                        case _ =>
                          MIO.fail(
                            new AssertionError("Unexpected override type in ToOption subtype handling")
                          )
                      }
                    }

                case None =>
                  // Non-overridden subtype: match by name against InnerTo's subtypes, derive, wrap in Some
                  val nameMatch = maybeToElements.flatMap { toElems =>
                    toElems.filter(toSub => areSubtypeNamesMatching(fromSubtype.value.name, toSub.value.name)) match {
                      case toSub :: Nil => Some(toSub)
                      case _            => None
                    }
                  }
                  nameMatch match {
                    case Some(toSubtype) =>
                      import toSubtype.Underlying as ToSubtype, toSubtype.value.upcast as toUpcast
                      MatchCase
                        .typeMatch[FromSubtype](FreshName.FromPrefix(fromSubtype.value.name.toLowerCase))
                        .traverse[MIO, TransformationExpr[To]] { (fromExpr: Expr[FromSubtype]) =>
                          deriveRecursiveTransformationExpr[FromSubtype, ToSubtype](
                            fromExpr,
                            followFrom = Path(_.matching[FromSubtype]),
                            followTo = Path(_.matching[Some[InnerTo]].select("value").matching[ToSubtype])
                          ).map(_.map(toUpcast)).map(_.map(optionalTo.of))
                        }
                    case None =>
                      // No unique name match — try deriving FromSubtype → InnerTo directly
                      MatchCase
                        .typeMatch[FromSubtype](FreshName.FromPrefix(fromSubtype.value.name.toLowerCase))
                        .traverse[MIO, TransformationExpr[To]] { (fromExpr: Expr[FromSubtype]) =>
                          deriveRecursiveTransformationExpr[FromSubtype, InnerTo](
                            fromExpr,
                            followFrom = Path(_.matching[FromSubtype]),
                            followTo = Path(_.matching[Some[InnerTo]].select("value"))
                          ).map(_.map(optionalTo.of))
                        }
                  }
              }
          }
          .flatMap { (matchCases: List[MatchCase[TransformationExpr[To]]]) =>
            val cases = NonEmptyVector.fromVector(matchCases.toVector).getOrElse {
              // $COVERAGE-OFF$should never happen unless we messed up
              assertionFailed("Expected at least one subtype pattern-match case")
              // $COVERAGE-ON$
            }
            if (matchCases.exists(_.isPartial))
              expandedPartial(ctx.src.matchOn[partial.Result[To]](cases.map(_.ensurePartial)))
            else
              expandedTotal(ctx.src.matchOn[To](cases.map(_.ensureTotal)))
          }
      case _ =>
        // $COVERAGE-OFF$should never happen: we're in ToOption rule, To must be Optional
        assertionFailed(s"ToOption rule matched but ${Type.prettyPrint[To]} is not an OptionalValue")
      // $COVERAGE-ON$
    }

  private val wrapFallbacks: TransformerOverride.ForFallback => Vector[TransformerOverride.ForFallback] = {
    case fb @ TransformerOverride.Fallback(fallback) =>
      import fallback.{Underlying as Fallback, value as fallbackExpr}
      Vector(Type[Fallback] match {
        case OptionalValue(_) => fb
        case _                => TransformerOverride.Fallback(wrapFallbackOptionCompat(fallbackExpr))
      })
  }
}
