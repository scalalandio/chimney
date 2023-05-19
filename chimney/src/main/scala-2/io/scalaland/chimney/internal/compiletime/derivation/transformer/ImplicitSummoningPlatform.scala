package io.scalaland.chimney.internal.compiletime.derivation.transformer

private[derivation] trait ImplicitSummoningPlatform { this: DerivationPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  final override protected def summonTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.Transformer[From, To]]] = scala.util
    .Try(c.inferImplicitValue(ChimneyType.Transformer[From, To], silent = true, withMacrosDisabled = false))
    .toOption
    .filterNot(_ == EmptyTree)
    .map(c.Expr[io.scalaland.chimney.Transformer[From, To]](_))

  final override protected def summonPartialTransformerUnchecked[From: Type, To: Type]
      : Option[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] =
    scala.util
      .Try(
        c.inferImplicitValue(ChimneyType.PartialTransformer[From, To], silent = true, withMacrosDisabled = false)
      )
      .toOption
      .filterNot(_ == EmptyTree)
      .map(c.Expr[io.scalaland.chimney.PartialTransformer[From, To]](_))
}
