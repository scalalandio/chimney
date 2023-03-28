package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.{PartialTransformer, Transformer}
import io.scalaland.chimney.internal.compiletime.{Configurations, Contexts, Definitions}

// this is a temporary workaround
private[compiletime] trait Legacy { this: Definitions & Configurations & Contexts =>

  protected val legacy: LegacyImpl
  protected trait LegacyImpl {

    def legacyTotalTransformerDerivation[From, To](implicit
        ctx: Context.ForTotal[From, To]
    ): Expr[Transformer[From, To]]

    def legacyPartialTransformerDerivation[From, To](implicit
        ctx: Context.ForPartial[From, To]
    ): Expr[PartialTransformer[From, To]]
  }
}
