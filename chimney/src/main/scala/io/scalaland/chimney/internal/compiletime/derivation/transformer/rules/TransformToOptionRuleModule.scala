package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformToOptionRuleModule { this: Derivation & TransformOptionToOptionRuleModule =>

  import Type.Implicits.*

  protected object TransformToOptionRule extends Rule("ToOption") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case _ if Type[To] <:< Type[None.type] =>
          DerivationResult
            .notSupportedTransformerDerivation(ctx)
            .log(s"Discovered that target type is ${Type.prettyPrint[None.type]} which we explicitly reject")
        case OptionalValue(_) =>
          DerivationResult.namedScope(s"Lifting ${Type.prettyPrint[From]} -> ${Type
              .prettyPrint[To]} transformation into ${Type.prettyPrint[Option[From]]} -> ${Type.prettyPrint[To]}") {
            wrapInOptionAndTransform[From, To]
          }
        case _ =>
          DerivationResult.attemptNextRule
      }
  }

  private def wrapInOptionAndTransform[From, To](implicit
      ctx: TransformationContext[From, To]
  ): DerivationResult[Rule.ExpansionResult[To]] =
    // We're constructing:
    // '{ ${ derivedTo2 } /* created from Option(src) */  }
    TransformOptionToOptionRule.expand(
      ctx.updateFromTo[Option[From], To](Expr.Option(ctx.src), updateFallbacks = wrapFallbacks)
    )

  private val wrapFallbacks: TransformerOverride.ForFallback => Vector[TransformerOverride.ForFallback] = {
    case fb @ TransformerOverride.Fallback(fallback) =>
      import fallback.{Underlying as Fallback, value as fallbackExpr}
      Vector(Type[Fallback] match {
        case OptionalValue(_) => fb
        case _                => TransformerOverride.Fallback(Expr.Option(fallbackExpr).as_??)
      })
  }
}
