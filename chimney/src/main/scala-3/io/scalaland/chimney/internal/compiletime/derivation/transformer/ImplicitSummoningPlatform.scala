package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[derivation] trait ImplicitSummoningPlatform { this: DerivationPlatform =>

  import quotes.*, quotes.reflect.*

  protected def summonTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] =
    scala.quoted.Expr.summon(using ChimneyType.Transformer[From, To])

  protected def summonPartialTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    scala.quoted.Expr.summon(using ChimneyType.PartialTransformer[From, To])
}
