package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformToOptionRuleModule { this: Derivation & TransformOptionToOptionRuleModule =>

  import Type.Implicits.*

  protected object TransformToOptionRule extends Rule("ToOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case Type.Option(to2) if !to2.Underlying.isSealed =>
          if (Type[To] <:< Type[None.type]) {
            DerivationResult
              .notSupportedTransformerDerivation(Expr.prettyPrint(ctx.src))
              .log(
                s"Discovered that target type is ${Type.prettyPrint[None.type]} which we explicitly reject"
              )
          } else {
            DerivationResult.namedScope(s"Lifting ${Type.prettyPrint[From]} -> ${Type
                .prettyPrint[To]} transformation into ${Type.prettyPrint[Option[From]]} -> ${Type.prettyPrint[To]}") {
              // We're constructing:
              // '{ Option(${ derivedTo2 }) } }
              TransformOptionToOptionRule.expand(ctx.updateFromTo[Option[From], To](Expr.Option(ctx.src)))
            }
          }
        case _ =>
          DerivationResult.attemptNextRule
      }
  }
}
