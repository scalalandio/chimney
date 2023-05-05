package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait TransformOptionToOptionRuleModule { this: Derivation =>

  object TransformOptionToOptionRule extends Rule("OptionToOption") {

    def expand[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if ((Type[From] <:< Type.Option(Type.Any)) && (Type[To] <:< Type.Option(Type.Any))) {
        DerivationResult.totalExpr(Expr.asInstanceOf[Nothing, To](Expr.Nothing)(Type.Nothing, Type[To]))
      } else {
        DerivationResult.continue
      }
  }
}
