package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.derivation.transformer.DerivationPlatform
import io.scalaland.chimney.internal.compiletime.DerivationResult

private[compiletime] trait NotImplementedFallbackRuleModule { this: DerivationPlatform =>

  import Type.Implicits.*

  // TODO: remove this rule once all rules are migrated; it's here only to make the Scala 3 tests compile
  protected object NotImplementedFallbackRule extends Rule("NotImplementedFallback") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult.expandedTotal(Expr.asInstanceOf[Nothing, To](Expr.Nothing))
  }
}
