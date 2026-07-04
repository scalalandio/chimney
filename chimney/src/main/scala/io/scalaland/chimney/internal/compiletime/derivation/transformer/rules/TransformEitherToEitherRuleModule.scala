package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import hearth.fp.effect.MIO
import hearth.fp.instances.*
import hearth.fp.syntax.*
import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.partial

private[compiletime] trait TransformEitherToEitherRuleModule {
  this: Derivation & TransformProductToProductRuleModule & hearth.MacroCommons =>

  import ChimneyType.Implicits.*, TransformProductToProductRule.useOverrideIfPresentOr

  protected object TransformEitherToEitherRule extends Rule("EitherToEither") {

    private lazy val EitherCtor: Type.Ctor2[Either] = Type.Ctor2.of[Either]
    private lazy val LeftCtor: Type.Ctor2[Left] = Type.Ctor2.of[Left]
    private lazy val RightCtor: Type.Ctor2[Right] = Type.Ctor2.of[Right]
    private lazy val EitherOfAnyType: Type[Either[Any, Any]] = Type.of[Either[Any, Any]]
    private lazy val LeftOfAnyType: Type[Left[Any, Any]] = Type.of[Left[Any, Any]]
    private lazy val RightOfAnyType: Type[Right[Any, Any]] = Type.of[Right[Any, Any]]

    // Cross-quotes helpers in methods with regular type parameters (the cross-quotes helper-def pattern).

    private def leftExprCompat[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] = Expr.quote {
      scala.Left[L, R](Expr.splice(value))
    }

    private def rightExprCompat[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] = Expr.quote {
      scala.Right[L, R](Expr.splice(value))
    }

    private def leftValueCompat[L: Type, R: Type](left: Expr[Left[L, R]]): Expr[L] = Expr.quote {
      Expr.splice(left).value
    }

    private def rightValueCompat[L: Type, R: Type](right: Expr[Right[L, R]]): Expr[R] = Expr.quote {
      Expr.splice(right).value
    }

    private def eitherFoldCompat[L: Type, R: Type, A: Type](either: Expr[Either[L, R]])(left: Expr[L => A])(
        right: Expr[R => A]
    ): Expr[A] = Expr.quote {
      Expr.splice(either).fold[A](Expr.splice(left), Expr.splice(right))
    }

    private def eitherOrElseCompat[L: Type, R: Type](
        either1: Expr[Either[L, R]],
        either2: Expr[Either[L, R]]
    ): Expr[Either[L, R]] = Expr.quote {
      Expr.splice(either1).orElse(Expr.splice(either2))
    }

    def expand[From, To](implicit ctx: TransformationContext[From, To]): MIO[Rule.ExpansionResult[To]] =
      Type[To] match {
        case EitherCtor(toL, toR) if Type[From] <:< EitherOfAnyType =>
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
              }).flatMap(either => expanded(either.map(_.upcast[To])))
            case _ => attemptNextRule
          }
        case _ => attemptNextRule
      }

    private def mapEithers[From, To, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): Option[MIO[TransformationExpr[To]]] = Type[From] match {
      case LeftCtor(fromL, fromR) if !(Type[To] <:< RightOfAnyType) =>
        import fromL.Underlying as FromL, fromR.Underlying as FromR
        Some(mapLeft[From, To, FromL, FromR, ToL, ToR])
      case RightCtor(fromL, fromR) if !(Type[To] <:< LeftOfAnyType) =>
        import fromL.Underlying as FromL, fromR.Underlying as FromR
        Some(mapRight[From, To, FromL, FromR, ToL, ToR])
      case EitherCtor(fromL, fromR) =>
        import fromL.Underlying as FromL, fromR.Underlying as FromR
        Some(mapEither[From, To, FromL, FromR, ToL, ToR])
      case _ => None
    }

    private def mapFallbackEithers[From, To, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): MIO[Vector[TransformationExpr[To]]] =
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
    ): MIO[TransformationExpr[To]] = {
      implicit val LeftFromType: Type[Left[FromL, FromR]] = Type.of[Left[FromL, FromR]]
      implicit val LeftToType: Type[Left[ToL, ToR]] = Type.of[Left[ToL, ToR]]
      useOverrideIfPresentOr("matchingLeft", ctx.config.filterCurrentOverridesForLeft) {
        deriveRecursiveTransformationExpr[FromL, ToL](
          leftValueCompat(ctx.src.upcast[Left[FromL, FromR]]),
          Path(_.matching[Left[FromL, FromR]].select("value")),
          Path(_.matching[Left[ToL, ToR]].select("value"))
        )
      }
        .map { (derivedToL: TransformationExpr[ToL]) =>
          // We're constructing:
          // '{ Left( ${ derivedToL } ) /* from ${ src }.value */ }
          derivedToL.map(leftExprCompat[ToL, ToR](_).upcast[To])
        }
    }

    private def mapRight[From, To, FromL: Type, FromR: Type, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): MIO[TransformationExpr[To]] = {
      implicit val RightFromType: Type[Right[FromL, FromR]] = Type.of[Right[FromL, FromR]]
      implicit val RightToType: Type[Right[ToL, ToR]] = Type.of[Right[ToL, ToR]]
      useOverrideIfPresentOr("matchingRight", ctx.config.filterCurrentOverridesForRight) {
        deriveRecursiveTransformationExpr[FromR, ToR](
          rightValueCompat(ctx.src.upcast[Right[FromL, FromR]]),
          Path(_.matching[Right[FromL, FromR]].select("value")),
          Path(_.matching[Right[ToL, ToR]].select("value"))
        )
      }
        .map { (derivedToR: TransformationExpr[ToR]) =>
          // We're constructing:
          // '{ Right( ${ derivedToR } ) /* from ${ src }.value */ }
          derivedToR.map(rightExprCompat[ToL, ToR](_).upcast[To])
        }
    }

    private def mapEither[From, To, FromL: Type, FromR: Type, ToL: Type, ToR: Type](implicit
        ctx: TransformationContext[From, To]
    ): MIO[TransformationExpr[To]] = {
      implicit val EitherFromType: Type[Either[FromL, FromR]] = Type.of[Either[FromL, FromR]]
      implicit val LeftFromType: Type[Left[FromL, FromR]] = Type.of[Left[FromL, FromR]]
      implicit val RightFromType: Type[Right[FromL, FromR]] = Type.of[Right[FromL, FromR]]
      implicit val LeftToType: Type[Left[ToL, ToR]] = Type.of[Left[ToL, ToR]]
      implicit val RightToType: Type[Right[ToL, ToR]] = Type.of[Right[ToL, ToR]]

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
        (expr: Expr[ToL]) => leftExprCompat[ToL, ToR](expr).upcast[To]
      val inRight =
        (expr: Expr[ToR]) => rightExprCompat[ToL, ToR](expr).upcast[To]

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
                  eitherFoldCompat[FromL, FromR, To](ctx.src.upcast[Either[FromL, FromR]])(
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
                  eitherFoldCompat[FromL, FromR, partial.Result[To]](ctx.src.upcast[Either[FromL, FromR]])(
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

    private def concatAndCast[To: Type, ToL: Type, ToR: Type](either1: Expr[To], either2: Expr[To]): Expr[To] = {
      implicit val EitherToType: Type[Either[ToL, ToR]] = Type.of[Either[ToL, ToR]]
      eitherOrElseCompat(either1.upcast[Either[ToL, ToR]], either2.upcast[Either[ToL, ToR]]).upcast[To]
    }
  }
}
