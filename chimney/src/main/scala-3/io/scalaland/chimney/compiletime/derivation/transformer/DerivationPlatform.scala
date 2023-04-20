package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{DefinitionsPlatform, DerivationResult}
import io.scalaland.chimney.{partial, PartialTransformer, Transformer}

private[derivation] trait DerivationPlatform extends Derivation { this: DefinitionsPlatform =>

  override protected def instantiateTotalTransformer[From: Type, To: Type](
      f: Expr[From] => DerivationResult[Expr[To]]
  ): DerivationResult[Expr[Transformer[From, To]]] = DerivationResult.direct { scoped =>
    '{
      new Transformer[From, To] {
        def transform(src: From): To = ${ scoped.returns(f('{ src })) }
      }
    }
  }

  override protected def instantiatePartialTransformer[From: Type, To: Type](
      f: (Expr[From], Expr[Boolean]) => DerivationResult[Expr[partial.Result[To]]]
  ): DerivationResult[Expr[PartialTransformer[From, To]]] = DerivationResult.direct { scoped =>
    '{
      new PartialTransformer[From, To] {
        def transform(src: From, failFast: Boolean): partial.Result[To] = ${
          scoped.returns(f('{ src }, '{ failFast }))
        }
      }
    }
  }

  override protected val rulesAvailableForPlatform: Seq[Rule] = Seq()
}
