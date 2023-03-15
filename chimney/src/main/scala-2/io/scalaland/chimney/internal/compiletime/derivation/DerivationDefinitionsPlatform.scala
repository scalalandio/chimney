package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.{partial, PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[derivation] trait DerivationDefinitionsPlatform extends DerivationDefinitions { this: DefinitionsPlatform =>

  final override protected def instantiateTotalTransformer[From: Type, To: Type](
      f: Expr[From] => DerivationResult[Expr[To]]
  ): DerivationResult[Expr[Transformer[From, To]]] =
    DerivationResult.notYetImplemented("Turning (From => To) into Transformer[From, To]")

  final override protected def instantiatePartialTransformer[From: Type, To: Type](
      f: (Expr[From], Expr[Boolean]) => DerivationResult[Expr[partial.Result[To]]]
  ): DerivationResult[Expr[PartialTransformer[From, To]]] =
    DerivationResult.notYetImplemented("Turning (From => To) into PartialTransformer[From, To]")
}
