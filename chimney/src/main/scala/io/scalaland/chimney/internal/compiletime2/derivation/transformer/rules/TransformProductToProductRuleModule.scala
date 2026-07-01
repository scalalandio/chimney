package io.scalaland.chimney.internal.compiletime2.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime2.NotSupportedOperationFromPath.Operation as FromOperation
import io.scalaland.chimney.internal.compiletime2.{DerivationErrors, DerivationResult}
import io.scalaland.chimney.internal.compiletime2.derivation.transformer.Derivation
import io.scalaland.chimney.partial

import scala.collection.immutable.ListMap

/** PARTIAL Hearth-based port of `...compiletime.derivation.transformer.rules.TransformProductToProductRuleModule`.
  *
  * TODO(hearth-migration): this chunk ports ONLY the helpers shared with the simpler rules (OptionToOption,
  * EitherToEither, ImplicitOuterTransformer, ValueClassToType, TypeToValueClass, and later IterableToIterable and
  * MapToMap): `useOverrideIfPresentOr` and its transitive closure (`useOverride`, `extractSrcByPath`,
  * `prependWholeErrorPath`, `findMatchingFallbackFields`, `findMatchingUpdateCandidates`, `filterAllowedFieldsByFlags`,
  * `appendMissingTransformer`) plus a `Rule.expand` STUB failing with `NotYetImplemented`. The heavy
  * product-to-product expansion itself is ported in a later chunk and MUST replace the stub.
  *
  * Differences vs the old version (in the ported helpers):
  *   - `Expr.String(value)` becomes Hearth's `Expr(value)` (via `ExprCodec`),
  *   - `.upcastToExprOf[B]` becomes Hearth's `.upcast[B]`,
  *   - `asInstanceOfExpr` on `ExistentialExpr` goes through [[MacroCommonsCompat.CompatExistentialExprOps]],
  *   - `Type[A => B]` instances come from `ScalaType.Implicits.Function1Type` (old code used `Type.Implicits`),
  *   - the single-error pattern in `useOverrideIfPresentOr` matches `MErrors` through the compat
  *     `DerivationErrors.unapply` (`NonEmptyVector`'s `(head, tail)`).
  */
private[compiletime2] trait TransformProductToProductRuleModule { this: Derivation & hearth.MacroCommons =>

  import ChimneyType.Implicits.*, ScalaType.Implicits.*

  protected object TransformProductToProductRule extends Rule("ProductToProduct") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      // TODO(hearth-migration): heavy rule not ported yet - the real expansion replaces this stub in a later chunk.
      DerivationResult.notYetImplemented("TransformProductToProductRule (ported with the heavy-rules chunk)")

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
        import io.scalaland.chimney.internal.compiletime2.DerivationError.TransformerError as TError
        import io.scalaland.chimney.internal.compiletime2.NotSupportedOperationFromPath as NotSupportedFrom
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
                  .Const(Expr(s"${ctx.currentTgt}.$toName"))
                  .upcast[partial.PathElement]
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
                      .Computed(Expr(s"${ctx.currentTgt}.$toName"))
                      .upcast[partial.PathElement]
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
                  .Computed(Expr(s"${ctx.currentTgt}.$toName"))
                  .upcast[partial.PathElement]
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

    // Fallback utilities (subset used by findMatchingUpdateCandidates - the rest comes with the heavy-rule port)

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
  }
}
