package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait ImplicitSummoningPlatform { this: DefinitionsPlatform & Configurations & Contexts =>

  import quotes.*, quotes.reflect.*

  // TODO: consult with Janek Chyb more buller proof way of verifying that we aren't calling
  //       Transformer.derive nor PartialTransformer.derive

  // TODO: this throws :(

  private val transformerDerive =
    TypeRepr.of[io.scalaland.chimney.Transformer.type].typeSymbol.methodMember("derive").head
  private val partialTransformerDerive =
    TypeRepr.of[io.scalaland.chimney.PartialTransformer.type].typeSymbol.methodMember("derive").head

  final protected def isAutoderivedFromTransformerDerive[From: Type, To: Type](
      expr: Expr[io.scalaland.chimney.Transformer[From, To]]
  ): Boolean = expr.asTerm match {
    case Inlined(Some(TypeApply(ident, _)), _, _) => ident.symbol == transformerDerive
    case _                                        => false
  }

  final protected def isAutoderivedFromPartialTransformerDerive[From: Type, To: Type](
      expr: Expr[io.scalaland.chimney.PartialTransformer[From, To]]
  ): Boolean = expr.asTerm match {
    case Inlined(Some(TypeApply(ident, _)), _, _) => ident.symbol == partialTransformerDerive
    case _                                        => false
  }
}
