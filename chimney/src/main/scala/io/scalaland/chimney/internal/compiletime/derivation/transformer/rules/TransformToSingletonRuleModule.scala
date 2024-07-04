package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

private[compiletime] trait TransformToSingletonRuleModule { this: Derivation =>

  import Type.Implicits.*

  protected object TransformToSingletonRule extends Rule("ToSingleton") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      Type[To] match {
        case _ if !(Type[To] =:= Type[Null]) && (Type[To] <:< Type[Unit] || Type[To] <:< Type[None.type]) =>
          DerivationResult.attemptNextRuleBecause(
            s"Explicitly ignoring singletons of ${Type.prettyPrint[To]} due to safety concerns"
          )
        case Singleton(toExpr) => DerivationResult.expandedTotal(toExpr)
        case _                 => DerivationResult.attemptNextRule
      }
  }
}
