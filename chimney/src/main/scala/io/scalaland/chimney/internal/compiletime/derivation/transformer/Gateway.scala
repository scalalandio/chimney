package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.dsl.TransformerDefinitionCommons
import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}
import io.scalaland.chimney.{internal, partial, PartialTransformer, Transformer}

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait Gateway { this: Definitions & Derivation =>

  // Intended for: being called from platform-specific code which returns Expr directly to splicing site

  final def deriveTotalTransformationResult[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ](src: Expr[From], runtimeDataStore: Option[Expr[TransformerDefinitionCommons.RuntimeDataStore]]): Expr[To] =
    deriveTransformationResult(
      TransformerContext.ForTotal.create[From, To](
        src,
        configurationsImpl.readTransformerConfig[Cfg, InstanceFlags, SharedFlags],
        runtimeDataStore
      )
    ).toEither.fold(
      derivationErrors => {
        val lines = derivationErrors.prettyPrint

        val richLines =
          s"""Chimney can't derive transformation from ${Type[From]} to ${Type[To]}
           |
           |$lines
           |Consult $chimneyDocUrl for usage examples.
           |
           |""".stripMargin

        reportError(richLines)
      },
      identity
    )

  final def deriveTotalTransformer[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ](runtimeDataStore: Option[Expr[TransformerDefinitionCommons.RuntimeDataStore]]): Expr[Transformer[From, To]] =
    instantiateTotalTransformer[From, To] { (src: Expr[From]) =>
      deriveTotalTransformationResult[From, To, Cfg, InstanceFlags, SharedFlags](src, runtimeDataStore)
    }

  final def derivePartialTransformationResult[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ](
      src: Expr[From],
      failFast: Expr[Boolean],
      runtimeDataStore: Option[Expr[TransformerDefinitionCommons.RuntimeDataStore]]
  ): Expr[partial.Result[To]] =
    deriveTransformationResult(
      TransformerContext.ForPartial.create[From, To](
        src,
        failFast,
        configurationsImpl.readTransformerConfig[Cfg, InstanceFlags, SharedFlags],
        runtimeDataStore
      )
    ).toEither.fold(
      derivationErrors => {
        val lines = derivationErrors.prettyPrint

        val richLines =
          s"""Chimney can't derive transformation from ${Type[From]} to ${Type[To]}
             |
             |$lines
             |Consult $chimneyDocUrl for usage examples.
             |
             |""".stripMargin

        reportError(richLines)
      },
      identity
    )

  final def derivePartialTransformer[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ](runtimeDataStore: Option[Expr[TransformerDefinitionCommons.RuntimeDataStore]]): Expr[PartialTransformer[From, To]] =
    instantiatePartialTransformer[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
      derivePartialTransformationResult[From, To, Cfg, InstanceFlags, SharedFlags](src, failFast, runtimeDataStore)
    }

  /** Adapts DerivedExpr[To] to expected type of transformation */
  private def deriveTransformationResult[From, To](implicit
      ctx: TransformerContext[From, To]
  ): DerivationResult[Expr[ctx.Target]] =
    // pattern match on DerivedExpr and convert to whatever is needed
    deriveTransformationResultExpr[From, To]
      .flatMap {
        case DerivedExpr.TotalExpr(expr) =>
          ctx match {
            case TransformerContext.ForTotal(_, _, _, _, _) =>
              DerivationResult.pure(expr)
            case TransformerContext.ForPartial(_, _, _, _, _, _) =>
              DerivationResult.pure(ChimneyExpr.PartialResult.Value(expr))
          }
        case DerivedExpr.PartialExpr(expr) =>
          ctx match {
            case TransformerContext.ForTotal(_, _, _, _, _) =>
              DerivationResult.fromException(
                new AssertionError("Derived partial.Result expression where total Transformer excepts direct value")
              )
            case TransformerContext.ForPartial(_, _, _, _, _, _) =>
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

  private val chimneyDocUrl = "https://scalalandio.github.io/chimney"
}
