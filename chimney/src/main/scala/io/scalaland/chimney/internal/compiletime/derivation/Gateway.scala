package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal
import io.scalaland.chimney.internal.compiletime.Definitions

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Gateway { this: Definitions & Derivation & Legacy =>

  /** Intended for: being called from *Unsafe method to return final Expr */
  final protected def deriveTotalTransformer[From: Type, To: Type](
      config: TransformerConfig[From, To]
  ): DerivationResult[Expr[Transformer[From, To]]] =
    instantiateTotalTransformer[From, To] { (src: Expr[From]) =>
      deriveTransformerTargetExpr(TransformerContext.ForTotal.create[From, To](src = src, config))
    }

  /** Intended for: being called from platform-specific code which returns Expr directly to splicing site */
  final protected def deriveTotalTransformerUnsafe[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ]: Expr[Transformer[From, To]] =
    deriveTotalTransformer[From, To](
      configurationsImpl.readTransformerConfig[From, To, Cfg, InstanceFlags, SharedFlags]
    ).unsafeGet._2 // TODO: consider where diagnostics from State (_1) should be printed if requested to

  /** Intended for: being called from *Unsafe method to return final Expr */
  final protected def derivePartialTransformer[From: Type, To: Type](
      config: TransformerConfig[From, To]
  ): DerivationResult[Expr[PartialTransformer[From, To]]] =
    instantiatePartialTransformer[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
      deriveTransformerTargetExpr(
        TransformerContext.ForPartial.create[From, To](src = src, failFast = failFast, config)
      )
    }

  /** Intended for: being called from platform-specific code which returns Expr directly to splicing site */
  final protected def derivePartialTransformerUnsafe[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ]: Expr[PartialTransformer[From, To]] =
    derivePartialTransformer[From, To](
      configurationsImpl.readTransformerConfig[From, To, Cfg, InstanceFlags, SharedFlags]
    ).unsafeGet._2 // TODO: consider where diagnostics from State (_1) should be printed if requested to

  /** Adapts DerivedExpr[To] to expected type of transformation */
  private def deriveTransformerTargetExpr[From, To](implicit
      ctx: TransformerContext[From, To]
  ): DerivationResult[Expr[ctx.Target]] = {
    // pattern match on DerivedExpr and convert to whatever is needed
    deriveTransformationResultExpr[From, To].flatMap {
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
          case TransformerContext.ForPartial(_, _, _, _, _) => DerivationResult.pure(expr)
        }
    }
    DerivationResult.notYetImplemented("Actual derivation")
  }
}
