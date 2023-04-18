package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.internal.compiletime.{Configurations, Contexts, Definitions, DerivationResult}

// this is a temporary workaround
private[compiletime] trait Legacy { this: Definitions & Configurations & Contexts =>

  protected val legacy: LegacyImpl
  protected trait LegacyImpl {

    def deriveTransformerTargetExprWithOldMacros[From, To](implicit
        ctx: TransformerContext[From, To]
    ): DerivationResult[DerivedExpr[To]]
  }
}
