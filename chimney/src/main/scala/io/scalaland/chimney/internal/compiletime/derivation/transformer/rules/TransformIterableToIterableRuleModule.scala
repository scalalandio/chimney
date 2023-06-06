package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformIterableToIterableRuleModule { this: Derivation =>

  // import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformIterableToIterableRule extends Rule("IterableToIterable") {

    // TODO: append index to error path

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      DerivationResult.attemptNextRule
  }
}
