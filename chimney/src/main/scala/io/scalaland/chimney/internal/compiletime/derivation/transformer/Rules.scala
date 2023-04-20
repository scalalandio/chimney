package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{Definitions, DerivationResult}

private[compiletime] trait Rules { this: Definitions =>

  abstract protected class Rule {

    def isApplicableTo[From, To](ctx: TransformerContext[From, To]): Boolean

    def apply[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]]
  }
  protected object Rule {

    def apply[From, To](
        rules: Seq[Rule]
    )(implicit ctx: TransformerContext[From, To]): DerivationResult[DerivedExpr[To]] =
      rules.find(_.isApplicableTo(ctx)) match {
        case Some(rule) => rule[From, To]
        case None       => DerivationResult.fail(???)
      }
  }
}
