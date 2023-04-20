package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{DefinitionsPlatform, DerivationErrors, DerivationResult}
import io.scalaland.chimney.{partial, PartialTransformer, Transformer}

private[derivation] trait DerivationPlatform extends Derivation with Legacy { this: DefinitionsPlatform =>

  import c.universe.{Transformer as _, *}

  private case class Wrapper(errors: DerivationErrors)

  override protected def instantiateTotalTransformer[From: Type, To: Type](
      f: Expr[From] => DerivationResult[Expr[To]]
  ): DerivationResult[Expr[Transformer[From, To]]] = DerivationResult.direct { scoped =>
    val src: String = c.freshName("src") // TODO: copy-paste solution/utility from old macros
    val srcExpr: Expr[From] = c.Expr[From](q"${TermName(src)}")
    c.Expr[Transformer[From, To]](
      q"""new _root_.io.scalaland.chimney.Transformer[${Type[From]}, ${Type[To]}] {
        def transform($src: ${Type[From]}): ${Type[To]} = {
          ${scoped.returns(f(srcExpr))}
        }
      }"""
    )
  }

  override protected def instantiatePartialTransformer[From: Type, To: Type](
      f: (Expr[From], Expr[Boolean]) => DerivationResult[Expr[partial.Result[To]]]
  ): DerivationResult[Expr[PartialTransformer[From, To]]] = DerivationResult.direct { scoped =>
    val src: String = c.freshName("src") // TODO: copy-paste solution/utility from old macros
    val srcExpr: Expr[From] = c.Expr[From](q"${TermName(src)}")
    val failFast: String = c.freshName("failFast") // TODO: copy-paste solution/utility from old macros
    val failFastExpr: Expr[Boolean] = c.Expr[Boolean](q"${TermName(failFast)}")
    c.Expr[PartialTransformer[From, To]](
      q"""new _root_.io.scalaland.chimney.PartialTransformer[${Type[From]}, ${Type[To]}] {
          def transform($src: ${Type[From]}, $failFast): _root_.io.scalaland.chimney.partial.Result[${Type[To]}] = {
            ${scoped.returns(f(srcExpr, failFastExpr))}
          }
        }"""
    )
  }

  override protected val rulesAvailableForPlatform: Seq[Rule] = Seq()
}
