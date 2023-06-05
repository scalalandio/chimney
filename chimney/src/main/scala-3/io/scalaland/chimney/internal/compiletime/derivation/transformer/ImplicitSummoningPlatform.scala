package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait ImplicitSummoningPlatform { this: DerivationPlatform =>

  import quotes.*, quotes.reflect.*

  // TODO: consult with Janek Chyb more buller proof way of verifying that we aren't calling
  //       Transformer.derive nor PartialTransformer.derive

  final protected def isAutoderivedFromTransformerDerive[From: Type, To: Type](
      expr: Expr[io.scalaland.chimney.Transformer[From, To]]
  ): Boolean = expr.asTerm match {
    case Inlined(Some(TypeApply(Ident("derive"), _)), _, _) => true //
    case _                                                  => false
  }

  final protected def isAutoderivedFromPartialTransformerDerive[From: Type, To: Type](
      expr: Expr[io.scalaland.chimney.PartialTransformer[From, To]]
  ): Boolean = expr.asTerm match {
    case Inlined(Some(TypeApply(Ident("derive"), _)), _, _) => true
    case _                                                  => false
  }
}
