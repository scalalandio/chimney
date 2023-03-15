package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal
import io.scalaland.chimney.internal.compiletime.Definitions
import io.scalaland.chimney.partial

// TODO
private[compiletime] trait DerivationGateway { this: Definitions & DerivationDefinitions =>

  /** Intended for: being called from *Unsafe method to return final Expr; recursive derivation */
  final protected def deriveTotalTransformer[From: Type, To: Type]: DerivationResult[Expr[Transformer[From, To]]] =
    instantiateTotalTransformer[From, To] { (src: Expr[From]) =>
      deriveTransformerBody[From, To](DerivedExpr.emptyTotal(src))
        .map(_.toEither)
        .flatMap {
          case Left(total) => DerivationResult.pure(total)
          case Right(_)    => DerivationResult.fromException(new AssertionError("Expected total transformer expr"))
        }
    }

  /** Intended for: being called from platform-specific code which returns Expr directly to splicing site */
  final protected def deriveTotalTransformerUnsafe[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ]: Expr[Transformer[From, To]] = DerivationResult.unsafeRunExpr(
    Context(config = readConfig[Cfg, InstanceFlags, SharedFlags]),
    deriveTotalTransformer[From, To]
  )

  /** Intended for: being called from *Unsafe method to return final Expr; recursive derivation */
  final protected def derivePartialTransformer[From: Type, To: Type]
      : DerivationResult[Expr[PartialTransformer[From, To]]] =
    instantiatePartialTransformer[From, To] { (src: Expr[From], failFast: Expr[Boolean]) =>
      deriveTransformerBody[From, To](DerivedExpr.emptyPartial(src, failFast))
        .map(_.toEither)
        .flatMap {
          case Left(_)        => DerivationResult.fromException(new AssertionError("Expected partial transformer expr"))
          case Right(partial) => DerivationResult.pure(partial)
        }
    }

  /** Intended for: being called from platform-specific code which returns Expr directly to splicing site */
  final protected def derivePartialTransformerUnsafe[
      From: Type,
      To: Type,
      Cfg <: internal.TransformerCfg: Type,
      InstanceFlags <: internal.TransformerFlags: Type,
      SharedFlags <: internal.TransformerFlags: Type
  ]: Expr[PartialTransformer[From, To]] = DerivationResult.unsafeRunExpr(
    Context(config = readConfig[Cfg, InstanceFlags, SharedFlags]),
    derivePartialTransformer[From, To]
  )

  private def deriveTransformerBody[From: Type, To: Type](
      inputs: DerivedExpr[Unit, Unit]
  ): DerivationResult[DerivedExpr[To, partial.Result[To]]] =
    DerivationResult.notYetImplemented("Actual derivation")
}
