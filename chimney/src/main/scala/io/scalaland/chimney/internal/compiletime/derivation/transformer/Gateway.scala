package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}
import io.scalaland.chimney.{internal, partial, PartialTransformer, Transformer}

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Gateway { this: Definitions & Derivation =>

  /** Intended for: being called from platform-specific code which returns Expr directly to splicing site */
  final protected def deriveTotalTransformerUnsafe[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ]: Expr[Transformer[From, To]] =
    instantiateTotalTransformer[From, To] { (src: Expr[From]) =>
      deriveTransformerTargetExpr(
        TransformerContext.ForTotal.create[From, To](
          src,
          configurationsImpl.readTransformerConfig[From, To, Cfg, InstanceFlags, SharedFlags]
        )
      ).unsafeGet._2
    }

  /** Intended for: being called from platform-specific code which returns Expr directly to splicing site */
  final protected def derivePartialTransformerUnsafe[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ]: Expr[PartialTransformer[From, To]] =
    instantiatePartialTransformer[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
      deriveTransformerTargetExpr(
        TransformerContext.ForPartial.create[From, To](
          src,
          failFast,
          configurationsImpl.readTransformerConfig[From, To, Cfg, InstanceFlags, SharedFlags]
        )
      ).unsafeGet._2
    }

  /** Adapts DerivedExpr[To] to expected type of transformation */
  private def deriveTransformerTargetExpr[From, To](implicit
      ctx: TransformerContext[From, To]
  ): DerivationResult[Expr[ctx.Target]] =
    // pattern match on DerivedExpr and convert to whatever is needed
    deriveTransformationResultExpr[From, To]
      .flatMap {
        case DerivedExpr.TotalExpr(expr) =>
          ctx match {
            case TransformerContext.ForTotal(_, _, _, _) => DerivationResult.pure(expr)
            case TransformerContext.ForPartial(_, _, _, _, _) =>
              DerivationResult.pure(ChimneyExpr.PartialResult.Value(expr))
          }
        case DerivedExpr.PartialExpr(expr) =>
          ctx match {
            case TransformerContext.ForTotal(_, _, _, _) =>
              DerivationResult.fromException(
                new AssertionError("Derived partial.Result expression where total Transformer excepts direct value")
              )
            case TransformerContext.ForPartial(_, _, _, _, _) =>
              DerivationResult.pure(expr)
          }
      }
      .asInstanceOf[DerivationResult[Expr[ctx.Target]]]

  protected def instantiateTotalTransformer[From: Type, To: Type](
      toExpr: Expr[From] => Expr[To]
  ): Expr[Transformer[From, To]]

  protected def instantiatePartialTransformer[From: Type, To: Type](
      toExpr: (Expr[From], Expr[Boolean]) => Expr[partial.Result[To]]
  ): Expr[PartialTransformer[From, To]]
}
