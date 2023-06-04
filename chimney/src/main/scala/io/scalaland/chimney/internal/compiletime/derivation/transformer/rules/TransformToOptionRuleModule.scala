package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait TransformToOptionRuleModule { this: Derivation with TransformOptionToOptionRuleModule =>

  import TypeImplicits.*

  object TransformToOptionRule extends Rule("ToOption") {

    def expand[From, To](implicit ctx: TransformerContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case Type.Option(to2) if !to2.Type.isSealed =>
          if (Type[To] <:< Type[None.type]) {
            // TODO: log
            DerivationResult.notSupportedTransformerDerivation
          } else {
            // TODO: log
            TransformOptionToOptionRule.expand(ctx.updateFromTo[Option[From], To](Expr.Option(ctx.src)))
          }
        case _ =>
          DerivationResult.continue
      }
  }
}
