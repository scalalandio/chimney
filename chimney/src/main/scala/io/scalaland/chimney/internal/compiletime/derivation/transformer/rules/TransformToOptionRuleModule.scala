package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformToOptionRuleModule { this: Derivation & TransformOptionToOptionRuleModule =>

  import Type.Implicits.*

  protected object TransformToOptionRule extends Rule("ToOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      if (Type[To] <:< Type[None.type])
        DerivationResult
          .notSupportedTransformerDerivation(ctx)
          .log(s"Discovered that target type is ${Type.prettyPrint[None.type]} which we explicitly reject")
      else if (Type[To].isOption)
        DerivationResult.namedScope(s"Lifting ${Type.prettyPrint[From]} -> ${Type
            .prettyPrint[To]} transformation into ${Type.prettyPrint[Option[From]]} -> ${Type.prettyPrint[To]}") {
          wrapInOptionAndTransform[From, To]
        }
      else DerivationResult.attemptNextRule
  }

  private def wrapInOptionAndTransform[From, To](implicit
      ctx: TransformationContext[From, To]
  ): DerivationResult[Rule.ExpansionResult[To]] =
    // We're constructing:
    // '{ Option(${ derivedTo2 }) } }
    TransformOptionToOptionRule.expand(ctx.updateFromTo[Option[From], To](Expr.Option(ctx.src)))
}
