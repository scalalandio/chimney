package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.instances.*
import hearth.fp.syntax.*
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

/** Hearth-based port of `...compiletime.derivation.transformer.rules.TransformEitherToEitherRuleModule`.
  *
  * Differences vs the old version:
  *   - `Type.Either(.Left/.Right)` matching and `Expr.Either` construction go through the `ScalaType`/`ScalaExpr`
  *     compat modules; `isEither/isLeft/isRight` come from the `ScalaStdTypeOps` compat ops,
  *   - `ExprPromise.promise(...).traverse(...).fulfilAsLambda` becomes `LambdaBuilder.of1(...).traverse(...)....build`
  *     (the lambdas are passed to the runtime `Either.fold`/`partial.Result.map/map2` iteration helpers - legitimate
  *     `LambdaBuilder` uses); `Expr.Function1.instance`/`Expr.Function2.instance` become `LambdaBuilder....buildWith`,
  *   - `upcastToExprOf` becomes `upcast`, `.sequence` comes from `hearth.fp` (instances + syntax).
  */
private[compiletime] trait TransformEitherToEitherRuleModule {
  this: Derivation & TransformProductToProductRuleModule & hearth.MacroCommons =>

  import ChimneyType.Implicits.*, ScalaType.Implicits.*, TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformEitherToEitherRule extends Rule("EitherToEither") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case ScalaType.Either(toL, toR) if Type[From].isEither =>
          import toL.Underlying as ToL, toR.Underlying as ToR
          mapEithers[From, To, ToL, ToR] match {
            case Some(srcToResult) =>
              def fallbackToResult = mapFallbackEithers[From, To, ToL, ToR]

              val merge = ctx match {
                case TransformationContext.ForTotal(_)             => mergeTotal[To, ToL, ToR]
                case TransformationContext.ForPartial(_, failFast) => mergePartial[To, ToL, ToR](failFast)
              }

              (ctx.config.flags.eitherFallbackMerge match {
                case None =>
                  srcToResult
                case Some(dsls.SourceOrElseFallback) =>
                  srcToResult.parMap2(fallbackToResult)((srcTo, fallbackTo) => fallbackTo.foldLeft(srcTo)(merge))
                case Some(dsls.FallbackOrElseSource) =>
                  srcToResult.parMap2(fallbackToResult)((srcTo, fallbackTo) =>
                    fallbackTo.reverseIterator.foldRight(srcTo)(merge)
                  )
              }).flatMap(either => DerivationResult.expanded(either.map(_.upcast[To])))
            case _ => DerivationResult.attemptNextRule
          }
        case _ => DerivationResult.attemptNextRule
      }

    private def mapEithers[From, To, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): Option[DerivationResult[TransformationExpr[To]]] = Type[From] match {
      case ScalaType.Either.Left(fromL, fromR) if !Type[To].isRight =>
        import fromL.Underlying as FromL, fromR.Underlying as FromR
        Some(mapLeft[From, To, FromL, FromR, ToL, ToR])
      case ScalaType.Either.Right(fromL, fromR) if !Type[To].isLeft =>
        import fromL.Underlying as FromL, fromR.Underlying as FromR
        Some(mapRight[From, To, FromL, FromR, ToL, ToR])
      case ScalaType.Either(fromL, fromR) =>
        import fromL.Underlying as FromL, fromR.Underlying as FromR
        Some(mapEither[From, To, FromL, FromR, ToL, ToR])
      case _ => None
    }

    private def mapFallbackEithers[From, To, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Vector[TransformationExpr[To]]] =
      ctx.config.filterCurrentOverridesForFallbacks.view
        .map { case TransformerOverride.Fallback(fallback) =>
          import fallback.{Underlying as Fallback, value as fallbackExpr}
          implicit val fallbackCtx: TransformationContext[Fallback, To] =
            ctx.updateFromTo[Fallback, To](fallbackExpr, updateFallbacks = _ => Vector.empty)(using Fallback, ctx.To)
          mapEithers[Fallback, To, ToL, ToR]
        }
        .collect { case Some(result) => result }
        .toVector
        .sequence

    private def mapLeft[From, To, FromL: Type, FromR: Type, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[TransformationExpr[To]] =
      useOverrideIfPresentOr("matchingLeft", ctx.config.filterCurrentOverridesForLeft) {
        deriveRecursiveTransformationExpr[FromL, ToL](
          ctx.src.upcast[Left[FromL, FromR]].value,
          Path(_.matching[Left[FromL, FromR]].select("value")),
          Path(_.matching[Left[ToL, ToR]].select("value"))
        )
      }
        .map { (derivedToL: TransformationExpr[ToL]) =>
          // We're constructing:
          // '{ Left( ${ derivedToL } ) /* from ${ src }.value */ }
          derivedToL.map(ScalaExpr.Either.Left[ToL, ToR](_).upcast[To])
        }

    private def mapRight[From, To, FromL: Type, FromR: Type, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[TransformationExpr[To]] =
      useOverrideIfPresentOr("matchingRight", ctx.config.filterCurrentOverridesForRight) {
        deriveRecursiveTransformationExpr[FromR, ToR](
          ctx.src.upcast[Right[FromL, FromR]].value,
          Path(_.matching[Right[FromL, FromR]].select("value")),
          Path(_.matching[Right[ToL, ToR]].select("value"))
        )
      }
        .map { (derivedToR: TransformationExpr[ToR]) =>
          // We're constructing:
          // '{ Right( ${ derivedToR } ) /* from ${ src }.value */ }
          derivedToR.map(ScalaExpr.Either.Right[ToL, ToR](_).upcast[To])
        }

    private def mapEither[From, To, FromL: Type, FromR: Type, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[TransformationExpr[To]] = {
      val toLeftResult = LambdaBuilder
        .of1[FromL](FreshName.FromPrefix("left"))
        .traverse { (leftExpr: Expr[FromL]) =>
          useOverrideIfPresentOr("matchingLeft", ctx.config.filterCurrentOverridesForLeft) {
            deriveRecursiveTransformationExpr[FromL, ToL](
              leftExpr,
              Path(_.matching[Left[FromL, FromR]].select("value")),
              Path(_.matching[Left[ToL, ToR]].select("value"))
            )
          }
        }

      val toRightResult = LambdaBuilder
        .of1[FromR](FreshName.FromPrefix("right"))
        .traverse { (rightExpr: Expr[FromR]) =>
          useOverrideIfPresentOr("matchingRight", ctx.config.filterCurrentOverridesForRight) {
            deriveRecursiveTransformationExpr[FromR, ToR](
              rightExpr,
              Path(_.matching[Right[FromL, FromR]].select("value")),
              Path(_.matching[Right[ToL, ToR]].select("value"))
            )
          }
        }

      val inLeft =
        (expr: Expr[ToL]) => ScalaExpr.Either.Left[ToL, ToR](expr).upcast[To]
      val inRight =
        (expr: Expr[ToR]) => ScalaExpr.Either.Right[ToL, ToR](expr).upcast[To]

      toLeftResult
        .map2(toRightResult) {
          (
              toLeft: LambdaBuilder[FromL => *, TransformationExpr[ToL]],
              toRight: LambdaBuilder[FromR => *, TransformationExpr[ToR]]
          ) =>
            (toLeft.exprPartition, toRight.exprPartition) match {
              case (Left(totalToLeft), Left(totalToRight)) =>
                // We're constructing:
                // '{ ${ src }.fold {
                //    left: $FromL => Left(${ derivedToL })
                // } {
                //    right: $FromR => Right(${ derivedToR })
                // }
                TransformationExpr.fromTotal(
                  ctx.src
                    .upcast[Either[FromL, FromR]]
                    .fold[To](
                      totalToLeft.map(inLeft).build[To]
                    )(
                      totalToRight.map(inRight).build[To]
                    )
                )
              case _ =>
                // We're constructing:
                // '{ ${ src }.fold {
                //    left: $FromL => ${ derivedToL }.map(Left(_))
                // } {
                //    right: $FromR => ${ derivedToR }.map(Right(_))
                // }
                TransformationExpr.fromPartial(
                  ctx.src
                    .upcast[Either[FromL, FromR]]
                    .fold[partial.Result[To]](
                      toLeft
                        .map(_.ensurePartial.map[To](LambdaBuilder.of1[ToL]().map(inLeft).build[To]))
                        .build[partial.Result[To]]
                    )(
                      toRight
                        .map(_.ensurePartial.map[To](LambdaBuilder.of1[ToR]().map(inRight).build[To]))
                        .build[partial.Result[To]]
                    )
                )
            }
        }
    }

    private def mergeTotal[To: Type, ToL: Type, ToR: Type]
        : (TransformationExpr[To], TransformationExpr[To]) => TransformationExpr[To] =
      (texpr1, texpt2) =>
        TransformationExpr.fromTotal(concatAndCast[To, ToL, ToR](texpr1.ensureTotal, texpt2.ensureTotal))

    private def mergePartial[To: Type, ToL: Type, ToR: Type](
        failFast: Expr[Boolean]
    ): (TransformationExpr[To], TransformationExpr[To]) => TransformationExpr[To] = {
      case (TransformationExpr.TotalExpr(expr1), TransformationExpr.TotalExpr(expr2)) =>
        TransformationExpr.fromTotal(concatAndCast[To, ToL, ToR](expr1, expr2))
      case (texpr1, texpr2) =>
        TransformationExpr.fromPartial(
          ChimneyExpr.PartialResult.map2(
            texpr1.ensurePartial,
            texpr2.ensurePartial,
            LambdaBuilder.of2[To, To]().buildWith { case (e1, e2) => concatAndCast[To, ToL, ToR](e1, e2) },
            failFast
          )
        )
    }

    private def concatAndCast[To: Type, ToL: Type, ToR: Type](either1: Expr[To], either2: Expr[To]): Expr[To] =
      either1.upcast[Either[ToL, ToR]].orElse(either2.upcast[Either[ToL, ToR]]).upcast[To]
  }
}
