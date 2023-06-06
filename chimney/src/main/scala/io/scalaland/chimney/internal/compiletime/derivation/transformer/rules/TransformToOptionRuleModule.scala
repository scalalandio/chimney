package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformToOptionRuleModule { this: Derivation & TransformOptionToOptionRuleModule =>

  import TypeImplicits.*

  protected object TransformToOptionRule extends Rule("ToOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case Type.Option(to2) if !to2.Underlying.isSealed =>
          if (Type[To] <:< Type[None.type]) {
            // TODO: log
            DerivationResult.notSupportedTransformerDerivation
          } else {
            // TODO: log
            // We're constructing:
            // '{ Option(${ derivedTo2 }) } }
            TransformOptionToOptionRule.expand(ctx.updateFromTo[Option[From], To](Expr.Option(ctx.src)))
          }
        case _ =>
          DerivationResult.attemptNextRule
      }
  }
}
