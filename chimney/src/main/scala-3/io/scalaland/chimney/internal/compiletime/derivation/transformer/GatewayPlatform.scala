package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.{internal, partial, PartialTransformer, Transformer}

private[derivation] trait GatewayPlatform extends Gateway { this: DerivationPlatform =>

  override protected def instantiateTotalTransformer[From: Type, To: Type](
      toExpr: Expr[From] => Expr[To]
  ): Expr[Transformer[From, To]] =
    '{
      new Transformer[From, To] {
        def transform(src: From): To = ${ toExpr('{ src }) }
      }
    }

  override protected def instantiatePartialTransformer[From: Type, To: Type](
      toExpr: (Expr[From], Expr[Boolean]) => Expr[partial.Result[To]]
  ): Expr[PartialTransformer[From, To]] =
    '{
      new PartialTransformer[From, To] {
        def transform(src: From, failFast: Boolean): partial.Result[To] = ${ toExpr('{ src }, '{ failFast }) }
      }
    }
}
